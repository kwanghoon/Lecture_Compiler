module Alpha(alpha) where 

import Id(gentmp)
import qualified Type as T
import KNormal( KNorm(..) )
import Control.Monad.State ( State, get, put, runState )
import qualified Data.Map as Map

type Id = String

_gentmp :: T.Type -> State Integer Id
_gentmp t = do
    c <- get
    let (x, c1) = gentmp t c
    put c1
    return x

alpha :: KNorm -> KNorm
alpha k = k1
    where (k1, _) = runState (_alpha Map.empty k) 0

_alpha :: Map.Map Id Id -> KNorm ->State Integer KNorm
_alpha = undefined