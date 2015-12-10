{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, Rank2Types, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, StandaloneDeriving, MultiParamTypeClasses, ConstraintKinds, OverloadedStrings #-}

module Resources where

import Control.Lens (set)
import Control.Lens.TH
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Except
import Storage
import GHC.Exts
import Data.Bson
import Data.Time



-- history is defined as a double linked list, 
class History a where
  type Back a  -- type of reversed modifications
  data Forth a -- type of a straight modification
  checkForth :: Forth a -> a -> Bool  -- possibly a fake, dunno. check if a Forth is applyable to an a
  goforth :: Forth a -> a -> (a, Back a) -- apply a forth step, defining the new state and a reverse operation
  goback :: Back a -> a -> a -- apply a back step

-- application specific value for Time


-- a possibly timed link
data Link = TLink UTCTime Index | Link Index


--deriving instance (Show (Time a), Show b , Show (Index (Step a)), Show (Link (Step a))) => Show (StepLink b a)
--deriving instance (Show (Forth a), Show (Back a), Show (Time a), Show (Index (Step a)), Show (Link (Step a))) => Show (Step a)

-- a Linkd augmented with a Forth or Back
data StepLink b = StepLink b Link


-- a double linked list of modifications (of a resource), a resource pointing here with a synchronized state can travel in time moving its link and updating with Back and Forth values
data Step a = Step {
        _back :: Maybe (StepLink (Back a)),
        _forth :: Maybe (StepLink (Forth a))
        }

makeLenses ''Step

-- resources on which we keep history and links to them can carry a time travel request
data Timed a = Timed {
        _res :: a, -- black box resource part
        _timestamp :: UTCTime, -- last modification applied timestamp
        _history :: Index -- straight link to a resource part of the history
        } deriving (Show)
makeLenses ''Timed


-- generic constraint for resources interface, time is same between a and Step a as we confront  Step a and a time instances
-- Ord (Time a) is necessary to browse history and keep it coherent
type Env m a =  (Monad m, History a) 

-- bring a resource to a state compatible with a time. Time compatibility is assessed with a (Time a -> Ordering).
-- When applying a modification is too far in the future and the check gives as a not LT with the actual state we stop travelling
travel 
        :: Env m a 
        => Storage m (Step a) -- we need to access (Step a) from the storage sistem
        -> (UTCTime -> Ordering) -- decide where to stop
        -> Timed a -- a resource seen as a timed
        -> m (Maybe (Timed a)) -- same resource shifted to right state
travel s ct r@(Timed x (ct -> GT) i) = do
        m <- pull s $ i -- get history
        case m of
                Step _ Nothing -> return $ Just r -- there is nothing beyond, we are actual
                Step _ (Just (StepLink f (TLink (ct -> LT) i ))) -> return $ Just r -- the future is too far, we settle
                Step _ (Just (StepLink f (TLink t i))) -> travel s ct $ Timed (fst $ goforth f x) t i -- let's see the future

travel s ct r@(Timed x (ct -> EQ) i) = return $ Just r

travel s ct (Timed x _ i) = do
        m <- pull s $ i
        case m of
                Step Nothing _ -> return Nothing -- there was nothing at the time
                Step (Just (StepLink b (TLink t i))) _ -> ($ Timed (goback b x) t i) $ case ct t of 
                                        LT -> travel s ct -- let's see the past
                                        _ -> return . Just -- the past is too behind, we settle

-- travel to a state compatible with a time
atTime :: Env m a => Storage m (Step a) -> Timed a -> UTCTime ->  m (Maybe (Timed a))
atTime s r t = travel s (compare t) r

-- travel to actuality
actual  :: Env m a => Storage m (Step a) ->  Timed a -> m (Timed a)
actual s r = fromJust <$> travel s (const GT) r-- point up, always, travel function should expose failures only going back

data StepError  = TimeError -- trying to assign a timestamp to a new step smaller than the last one
                | ForthError -- forth check failed

-- insert a modification on the resource actualized, time consecutio and Forth correctness is tested here 
-- (Forth correctness request resource access to actuality)
modify  :: (Env m a, MonadError StepError m)  
        => Storage m (Step a) -- storage access to history
        -> Forth a  -- a step forth
        -> Timed a  -- the resource in a random (in time) state
        -> UTCTime -- the timestamp for the step
        -> m (Timed a) -- the resource actualized with history updated if successful
modify s f  r tn = do
        Timed x t i <- actual s r -- can this be expressed in the Timed type ? It's nonsense to pass a Forth for a rewinded Timed
        when  (t >= tn) $ throwError TimeError
        when  (not $ checkForth f x) $ throwError ForthError
        let (x',b) = goforth f x
        Timed x' tn <$>  update s 
                                (Step (Just $ StepLink b $ TLink t i) Nothing)  
                                (\i' -> return . set forth (Just . StepLink f $ TLink tn i')) i 

-- a new resource from its core and boot time
new     :: Env m a 
        => Storage m (Step a) -- storage access to history
        -> a -- the new core
        -> UTCTime -- the boot time for the resource
        -> m (Timed a) -- a fresh timed resource on the core and timestamp with empty history
new s x t = Timed x t <$> push s (Step Nothing Nothing) 


data instance Predication (Timed a) =History Integer | Timestamp UTCTime deriving (Eq,Ord, Show)

instance Serialize Field (Timed a) where
        parse ("History" := Int64 t) = Just $ History $ fromIntegral t
        parse ("Timestamp" := UTC t) = Just $ Timestamp t
        parse _ = Nothing
        serialize (History t) = ("History" := Int64 (fromIntegral t))
        serialize (Timestamp t) = ("Timestamp" := UTC t)

instance (Show (Predication (Timed a))) => Struct (Timed a) where
        type SubType (Timed a) = a
        struct x [History h, Timestamp t] = Timed x t h
        struct x a = error (show a)
        destruct (Timed x t h) = (x, [History h,Timestamp t])     
instance History () where
        data Forth () = DoNothing ()
        type Back () = Forth ()
        goforth f _ = ((),f)
        goback f =  fst . goforth f
        checkForth _ _ = True
