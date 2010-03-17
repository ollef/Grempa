{-# LANGUAGE GADTs, ExistentialQuantification, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.State

data Ref a env where
    Zero :: Ref a (a, env')
    Succ :: Ref a env' -> Ref a (x, env')

data Env f env where
    Empty :: Env f ()
    Ext   :: f a -> Env f env' -> Env f (a, env')

--data GEnv s env  = forall a. GEnv (Env (P s) env) (Ref a env)

lookupRef :: Ref a env -> Env f env -> f a
lookupRef Zero     (Ext p _ ) = p
lookupRef (Succ r) (Ext _ ps) = lookupRef r ps

updateRef :: Ref a env -> (f a -> f a) -> Env f env -> Env f env
updateRef Zero     f (Ext p ps) = Ext (f p) ps
updateRef (Succ r) f (Ext p ps) = Ext p (updateRef r f ps)

data RefEnv f r = forall env. RefEnv (Env f env) (Ref r env)

--data RefStateT f r m a = RefState 
  --{ unRefState :: StateT (RefEnv f r) m a 
  --} deriving (MonadTrans m, MonadState (RefEnv f r))

type RefStateT f r = StateT (RefEnv f r)

lookup :: Ref r e -> RefStateT f r m (f r)
lookup r = do
    RefEnv (env :: Env f e) refs <- get
    return $ lookupRef r env
        
