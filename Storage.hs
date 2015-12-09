{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, Rank2Types #-}

-- | Storage model
module Storage where

import qualified Data.Map as M
import Control.Arrow
import Control.Monad.Except
import Control.Lens

type Serialized p  = [p]


type Node a = M.Map (Pred a) (Obj a)

class Parse p a where
        data family Pred a
        data family Obj a
        parse :: p -> Maybe (Pred a, Obj a)
        serialize :: (Pred a, Obj a) -> p


fromSerialized :: (Ord (Pred a), Parse p a) => Serialized p -> Maybe (Node a)
fromSerialized [] = Just $ M.empty
fromSerialized (x:rs) = do
        (p',o') <- parse x
        M.insert p' o' <$> fromSerialized rs

toSerialized :: (Ord (Pred a), Parse p a) =>  Node a -> Serialized p 
toSerialized  = map serialize . M.toList 

data Storage m a = Storage  {
        pull :: Integer  -> m a -- ^ pick a value of type a at given index
        , push :: a -> m Integer -- ^ store a value of type a
        , update :: a -> (Integer -> a -> m a) -> Integer -> m Integer -- ^ store a value of type a and modify the value at the given index feeding the new index
        , delete :: Integer -> m ()
        }
      
data ParseError = ParseError
tryParse f y = case fromSerialized y of
        Nothing -> throwError ParseError
        Just y' -> f y'
serializedStorage
  :: (Ord (Pred a), Parse p a, MonadError ParseError m) =>
     Storage m (Serialized p) -> Storage m (Node a)
serializedStorage (Storage pull push update delete) = Storage
        (\i -> pull i >>= tryParse return)
        (push . toSerialized)
        (\x f i -> update (toSerialized x) 
                (\i' -> fmap toSerialized . tryParse (f i')) 
                i
        )
        delete
        
lensedStorage :: Monad m => Lens' (Node a) b -> Storage m (Node a) -> Storage m b
lensedStorage l (Storage pull push update delete) = Storage 
        (\i -> view l <$> pull i )
        (push . flip (set l) M.empty)
        (\x f -> update (set l x M.empty) 
                        (\i' y -> flip (set l) y <$> f i' (view l y)
                        )
        )
        delete
