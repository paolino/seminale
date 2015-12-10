{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, Rank2Types #-}

-- | Storage model
module Storage where

import qualified Data.Map as M
import Control.Arrow
import Control.Monad.Except
import Control.Lens hiding (Index)
import Data.List
import  Data.Maybe


data family Predication a -- predicates values
-- a class for serializable semantic resources, p is the token type (Bson.Field)

class Serialize p a where
        parse :: p -> Maybe (Predication a) -- parse a predication
        serialize :: Predication a -> p --serialize a predication

-- in memory structure of a resource. This is an hybrid definition of a resource value, it still has to be projected to the real thing (with a lens ?)
type Node a = [Predication a]

-- raw serialization of a resource
type Serialized p  = [p]

fromSerialized :: (Ord (Predication a), Serialize p a) => Serialized p -> Node a
fromSerialized = sort . catMaybes . map parse

toSerialized :: Serialize p a =>  Node a -> Serialized p 
toSerialized  = map serialize 

class Struct a where
        type SubType a
        struct :: SubType a -> Node a -> a
        destruct :: a -> (SubType a, Node a)

up :: (Ord (Predication a),Struct a, Serialize p a) => SubType a -> Serialized p -> a
up x = struct x . sort . fromSerialized 

down :: (Ord (Predication a),Struct a, Serialize p a) => a -> (SubType a, Serialized p)
down  = second toSerialized . destruct 
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


-- lift a Storage from Serialized to Node
fromSerializedStorage
        ::      (Ord (Predication a),Serialize p a, MonadError SerializeError m) 
        =>      Storage m (Serialized p)         
        ->      Storage m (Node a)

fromSerializedStorage (Storage pull push update delete) = Storage pl ps ud delete where
        pl i    = fromSerialized <$> pull i 
        ps      = push . toSerialized
        ud x f  = update (toSerialized x) $ \i' y -> toSerialized <$> f i' (fromSerialized y)
                        
{-        
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
       
-} 
