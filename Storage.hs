{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-- | Storage model
module Storage where

-- | Backend interface
class Monad m => Storage m a where
        data family Index a
        pull :: Index a  -> m a -- ^ pick a value of type a at given index
        push :: a -> m (Index a) -- ^ store a value of type a
        update :: a -> (Index a -> a -> a) -> Index a -> m (Index a) -- ^ store a value of type a and modify the value at the given index feeding the new index
        delete :: Index a -> m ()
        


