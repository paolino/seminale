
{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, Rank2Types, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, StandaloneDeriving, OverloadedStrings #-}

import Semantics
import Resources
import Predicates
import Instances
import Objects

import Control.Concurrent.STM
import qualified Data.Map as M
import System.Random
import Data.Time.Clock
import Data.Bson
import Data.Maybe
import Control.Monad.Reader
import Data.Int


main = do
  m <- newTVarIO $ M.empty  
  let   u x Nothing = do
                n <- fromIntegral <$> (randomIO :: IO Int)
                atomically $ modifyTVar m $ M.insert n x
                return n
        u x (Just (ModifyM f i)) = do
                n <- fromIntegral <$> (randomIO :: IO Int)

                atomically $ modifyTVar m $ M.adjust (f n) i . M.insert n x
                return n
        g i = flip (M.!) i <$> readTVarIO m         
        interface = Interface g u
        z = newNode "ciao" :: NodeS Value
  t0 <- getCurrentTime
  flip runReaderT interface $ do
          t1 <- liftIO getCurrentTime
          r <- new z t1
          t2 <- liftIO getCurrentTime 
          r' <- modify (NewF Author $ Secret "mamma") r t2
          Just r'' <- r' `atTime` t1 
          Nothing <- r' `atTime`  t0
          Nothing <- r `atTime` t0
          True <- return $ r'' == r
          let now f = liftIO getCurrentTime >>= f
          now $ modify (NewF (Generic "età") (Meaning . Literal . val $ (28 :: Int32))) r
          liftIO $ print r 
          now $ modify (NewF (Generic "I was born") (Meaning . Link $ Timed t1  )) r
          return ()
           


