{-# LANGUAGE FlexibleInstances, ExistentialQuantification, MultiParamTypeClasses, TypeFamilies#-}


-- | In memory backend, based on lazy map, for Typeable a
module InMemory where

import Prelude hiding (lookup)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import System.Random
import Control.Monad.Trans
import Control.Lens hiding (Index)
import Control.Lens.TH
import Text.Read (readMaybe)
import Data.Typeable
import qualified Data.Map as M

import Storage

data B = forall a. Typeable a => B a

type Vault = M.Map Integer B


lookup :: Typeable a => Integer -> Vault -> Maybe a
lookup k v = M.lookup k v >>= \(B x) -> cast x

insert :: Typeable a =>  a -> Vault -> (Integer,Vault)
insert x v = (n, M.insert n (B x) v) where
        n = succ . fst . M.findMax $ v

modifyB :: Typeable a => (a -> a) -> Integer -> Vault -> Maybe Vault
modifyB f k v = do
        B x <- M.lookup k v
        y <- cast x
        return $ M.insert k (B $ f y) v


type InMemory = StateT Vault (Except String)

instance Typeable a =>  Storage InMemory a where
        pull (i) = gets (lookup i) >>= maybe (throwError "get failed") return 
        push x = do
                (i,v) <- gets (insert x)
                put v
                return $ i
        update x f t@(i') = do
                i <- push x
                gets (modifyB (f i) i') >>= maybe (throwError "modify failed") put 
                return i
     
drive :: InMemory a -> Vault -> Either String (a,Vault)           
drive a v =  runExcept (runStateT a v)
                
                
