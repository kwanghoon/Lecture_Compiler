module Closure where 


import Id (TlvId(..))
import qualified Type as T
import qualified KNormal as K
import qualified Data.Set as Set
import qualified Data.Map as Map

type Id = String

type Closure = (TlvId, [Id])

data C =
    Unit
  | Int Int 
  | Float Double
  | Neg Id
  | Add Id Id
  | Sub Id Id
  | FNeg Id
  | FAdd Id Id
  | FSub Id Id
  | FMul Id Id
  | FDiv Id Id
  | IfEq Id Id C C
  | IfLE Id Id C C
  | Let (Id, T.Type) C C
  | Var Id
  | MakeCls (Id, T.Type) Closure C
  | AppCls Id [Id]
  | AppDir TlvId [Id]
  | Tuple [Id]
  | LetTuple [(Id, T.Type)] Id C
  | Get Id Id
  | Put Id Id Id
  | ExtArray TlvId
  deriving (Show)

type FunDef = ((TlvId, T.Type), [(Id, T.Type)], [(Id,T.Type)], C)

data Prog = Prog [FunDef] C deriving (Show)

fv :: C -> Set.Set Id
fv Unit = Set.empty
fv (Int _) = Set.empty
fv (Float _) = Set.empty
fv (Neg x) = Set.singleton x
fv (Add x y) = Set.fromList [x, y]
fv (Sub x y) = Set.fromList [x, y]
fv (FNeg x) = Set.singleton x
fv (FAdd x y) = Set.fromList [x, y]
fv (FSub x y) = Set.fromList [x, y]
fv (FMul x y) = Set.fromList [x, y]
fv (FDiv x y) = Set.fromList [x, y]
fv (IfEq x y e1 e2) = 
    Set.insert x $ Set.insert y $ Set.union (fv e1) (fv e2)
fv (IfLE x y e1 e2) = 
    Set.insert x $ Set.insert y $ Set.union (fv e1) (fv e2)
fv (Let (x, _) e1 e2) =
    Set.union (fv e1) (Set.delete x (fv e2))
fv (Var x) = Set.singleton x
fv (MakeCls _ (_, ys) e) = Set.difference (fv e) (Set.fromList ys)
fv (AppCls x ys) = Set.fromList (x : ys)
fv (AppDir _ xs) = Set.fromList xs
fv (Tuple xs) = Set.fromList xs
fv (LetTuple xts y e) = 
    Set.insert y $ Set.difference (fv e) (Set.fromList $ map fst xts)
fv (Get x y) = Set.fromList [x, y]
fv (Put x y z) = Set.fromList [x, y, z]
fv (ExtArray _) = Set.empty

cloconv :: K.KNorm -> Map.Map Id T.Type -> Set.Set Id -> C
cloconv K.Unit _env _known = Unit
cloconv (K.Int i) _env _known = Int i
cloconv (K.Float d) _env _known = Float d
cloconv (K.Neg x) _env _known = Neg x
cloconv (K.Add x y) _env _known = Add x y
cloconv (K.Sub x y) _env _known = Sub x y
cloconv (K.FNeg x) _env _known = FNeg x
cloconv (K.FAdd x y) _env _known = FAdd x y
cloconv (K.FSub x y) _env _known = FSub x y
cloconv (K.FMul x y) _env _known = FMul x y
cloconv (K.FDiv x y) _env _known = FDiv x y
cloconv (K.IfEq x y e1 e2) env known = 
    IfEq x y (cloconv e1 env known) (cloconv e2 env known)
cloconv (K.IfLE x y e1 e2) env known =
    IfLE x y (cloconv e1 env known) (cloconv e2 env known)
cloconv (K.Let (x, t) e1 e2) env known =
    Let (x, t) (cloconv e1 env known) 
        (cloconv e2 (Map.insert x t env) known)
cloconv (K.Var x) _env _known = Var x
cloconv (K.LetRec (x, t) yts e1 e2) env known =
    undefined 
cloconv (K.App x ys) _env known | Set.member x known = 
    AppDir (TlvId x) ys
cloconv (K.App x ys) _env known = AppCls x ys
cloconv (K.Tuple xs) _env _known = Tuple xs
cloconv (K.LetTuple xts y e) env known =
    LetTuple xts y (cloconv e (Map.union (Map.fromList xts) env) known)
cloconv (K.Get x y) _env _known = Get x y
cloconv (K.Put x y z) _env _known = Put x y z
cloconv (K.ExtArray x) _env _known = ExtArray (TlvId x)
cloconv (K.ExtFunApp x ys) _env _known = 
    AppDir (TlvId ("min_caml" ++ x)) ys