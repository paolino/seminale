{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Index where

data ModifyM a = ModifyM (Index a -> a -> a) (Index a)

class Monad m => Storage m a where
        data family Index a
        get :: Index a  -> m a
        put :: a -> m (Index a)
        update :: a -> Maybe (ModifyM a) -> m (Index a)


