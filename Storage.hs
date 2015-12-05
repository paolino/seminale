{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}
import qualified Control.Monad.State as C

import Index
import qualified  Data.Map as M

instance C.MonadState (M.Map Integer a) m => Storage m a where
        get i = flip (M.!) i <$> get 
        put i x = M.insert i x <$> C.modify 
