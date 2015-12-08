{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, Rank2Types, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, StandaloneDeriving, MultiParamTypeClasses #-}

module Resources where

import Control.Lens (set)
import Control.Lens.TH
import Data.Maybe
import Control.Monad.Reader
import Storage

deriving instance (Show (Time a), Show b , Show (Index (Mod a)), Show (Link (Mod a))) => Show (Mlink b a)
deriving instance (Show (Forth a), Show (Back a), Show (Time a), Show (Index (Mod a)), Show (Link (Mod a))) => Show (Mod a)


class Browse a where
  type Back a
  type Forth a
  checkForth :: Forth a -> a -> Bool
  goforth :: Forth a -> a -> (a, Back a)
  goback :: Back a -> a -> (a, Forth a)

type family Time a

data Link a = Link (Maybe (Time a)) (Index a) 

deriving instance (Eq (Time a), Eq (Index a)) => Eq (Link a)
deriving instance (Ord (Time a), Ord (Index a)) => Ord (Link a)

data Mlink b a = Mlink b (Link (Mod a))


data Mod a = Mod {
        _back :: Maybe (Mlink (Back a) a),
        _forth :: Maybe (Mlink (Forth a) a)
        }

makeLenses ''Mod

data Timed a = Timed {
        _res :: a,
        _timestamp :: Time a,
        _history :: Index (Mod a)
        }
makeLenses ''Timed

deriving instance (Show (Time a), Show (Index (Mod a)), Show a) => Show (Timed a)
deriving instance (Eq (Time a), Eq (Index (Mod a)), Eq a) => Eq (Timed a)


cursor :: (Monad m, Browse a, Storage m (Mod a), Time a ~ Time (Mod a)) => (Time a -> Ordering) -> Timed a -> m (Maybe (Timed a))
cursor ct r@(Timed x (ct -> GT) i) = do
        m <- pull i -- get history
        case m of
                Mod _ Nothing -> return $ Just r -- there is nothing beyond, we are actual
                Mod _ (Just (Mlink f (Link (fmap ct -> Just LT) i ))) -> return $ Just r -- the future is too far, we settle
                Mod _ (Just (Mlink f (Link (Just t) i))) -> cursor  ct $ Timed (fst $ goforth f x) t i -- let's see the future

cursor ct r@(Timed x (ct -> EQ) i) = return $ Just r

cursor ct (Timed x _ i) = do
        m <- pull i
        case m of
                Mod Nothing _ -> return Nothing -- there was nothing at the time
                Mod (Just (Mlink b (Link (Just t) i))) _ -> ($ Timed (fst $ goback b x) t i) $ case ct t of 
                                        LT -> cursor  ct -- let's see the past
                                        _ -> return . Just -- the past is too behind, we settle


atTime :: (Monad m, Browse a, Ord (Time a), Storage m (Mod a), Time a ~ Time (Mod a)) => Timed a -> Time a ->  m (Maybe (Timed a))
atTime r t = cursor (compare t) r

-- | step a resource till its top
actual  :: (Monad m, Browse a, Storage m (Mod a), Time a ~ Time (Mod a)) => Timed a -> m (Timed a)
actual r = fromJust <$> cursor (const GT) r-- point up, always, it must expose failures only going back

data TimeError = TimeError | ForthUnacceptable

-- | insert a modification on the resource actualized, time consecutio is tested here
modify :: (Monad m , Browse a, Ord (Time a), Storage m (Mod a), Time a ~ Time (Mod a)) => Forth a -> Timed a -> Time a -> m (Either TimeError (Timed a))
modify  f  r tn = do
        Timed x t i <- actual r -- can this be expressed in the Timed type ? It's nonsense to pass a Forth for a rewinded Timed
        if (t < tn) && (checkForth f x) then do 
                let (x',b) = goforth f x
                Right <$> Timed x' tn <$>  update (Mod (Just $ Mlink b $ Link (Just t) i) Nothing)  (\i' -> set forth . Just $ Mlink f $ Link (Just tn) i') i 
         else return $ Left TimeError

-- | a new resource from its core and boot time
new :: (Monad m , Browse a, Ord (Time a),Storage m (Mod a)) => a -> Time a -> m (Timed a)
new x t = Timed x t <$> push (Mod Nothing Nothing) 





