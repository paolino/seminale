{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module Storage where

data ModifyM a = ModifyM (Index a -> a -> a) (Index a)

class Monad m => Storage m a where
        data family Index a
        pull :: Index a  -> m a
        push :: a -> m (Index a)
        update :: a -> (Index a -> a -> a) -> Index a -> m (Index a)


