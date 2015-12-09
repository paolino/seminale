{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, Rank2Types #-}

-- | Storage model
module Storage where

import qualified Data.Map as M
import Control.Arrow
import Control.Monad.Except
import Control.Lens hiding (Index)


-- a class for serializable semantic resources, p is the token type (Bson.Field)
class Serialize p a where
        data family Pred a -- predicates values
        data family Obj a  -- object values
        parse :: p -> Maybe (Pred a, Obj a) -- parse a predication
        serialize :: (Pred a, Obj a) -> p --serialize a predication

-- in memory structure of a resource. This is an hybrid definition of a resource value, it still has to be projected to the real thing (with a lens ?)
type Node a = M.Map (Pred a) (Obj a)

-- raw serialization of a resource
type Serialized p  = [p]

fromSerialized :: (Ord (Pred a), Serialize p a) => Serialized p -> Maybe (Node a)
fromSerialized [] = Just $ M.empty
fromSerialized (x:rs) = do
        (p',o') <- parse x
        M.insert p' o' <$> fromSerialized rs

toSerialized :: (Ord (Pred a), Serialize p a) =>  Node a -> Serialized p 
toSerialized  = map serialize . M.toList 
----------------------------------------------------------------------------


-- index to a resource
type Index = Integer

-- storage cage
data Storage m a = Storage  {
          pull :: Index  -> m a -- ^ pick a value of type a at given index
        , push :: a -> m Index -- ^ store a value of type a
        , update :: a -> (Index -> a -> m a) -> Index -> m Index -- ^ store a value of type a and modify the value at the given index feeding the new index
        , delete :: Index -> m () -- ^ delete a resource (not used)
        }
      
data SerializeError = SerializeError

trySerialize f =  maybe (throwError SerializeError) f . fromSerialized 

-- lift a Storage from Serialized to Node
fromSerializedStorage
        ::      (Ord (Pred a), Serialize p a, MonadError SerializeError m) 
        =>      Storage m (Serialized p)         
        ->      Storage m (Node a)

fromSerializedStorage (Storage pull push update delete) = Storage pl ps ud delete where
        pl i    = pull i >>= trySerialize return
        ps      = push . toSerialized
        ud x f  = update (toSerialized x) $ \i' -> fmap toSerialized . trySerialize (f i')
                        
        
-- lift a Storage from Node to anything with a lens from Node
fromNodeStorage 
        ::      Monad m 
        =>      Lens' (Node a) b 
        ->      Storage m (Node a) 
        ->      Storage m b
fromNodeStorage l (Storage pull push update delete) = Storage pl ps ud delete  where
        pl i    = view l <$> pull i 
        ps      = push . flip (set l) M.empty
        ud x f  = update (set l x M.empty) $ \i' y -> flip (set l) y <$> f i' (view l y)
        
