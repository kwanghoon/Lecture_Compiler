module EitherState(Either_, _Right, _Left, _run, _onLeft, _fresh) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)

{- A combination of Either and a global integer variable. 

 The TypeChecker could directly use monad transformers.
 However, I want to avoid this, as it would require the reader 
 to understand the concepts of monads and monad transformers, 
 which feels like using a hedgehammer.
 Instead, I aim to provide an approach similar to the one used 
 with the Either type in the TypeChecker.
-}

type InternalState = Integer

type Either_ e a = ExceptT e (State InternalState) a

_Right :: a -> Either_ e a
_Right = return

_Left :: e -> Either_ e a
_Left = throwE

_runFrom:: Integer -> Either_ e a -> Either e a 
_runFrom _initialInteger e = 
  let (r, _) = runState (runExceptT e) _initialInteger 
  in r

_run :: Either_ e a -> Either e a
_run = _runFrom initialInteger 
  where initialInteger = 0

_onLeft :: Either_ e a -> (e -> Either_ e a) -> Either_ e a
_onLeft= catchE

_fresh :: Either_ e InternalState
_fresh = do newNum <- lift $ get
            lift $ modify (+1)
            return newNum 