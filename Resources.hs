{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, Rank2Types, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, StandaloneDeriving, MultiParamTypeClasses #-}

module Resources (new, modify, actual, atTime, Browse (..), Res (),res,timestamp,history, Time, Mod,  Index) where

import Control.Lens (set)
import Control.Lens.TH
import Data.Maybe
import Control.Monad.Reader
import Index

deriving instance (Show (Time a), Show (b a), Show (IM a)) => Show (Link b a)
deriving instance (Show (Forth a), Show (Back a), Show (Time a), Show (IM a)) => Show (Mod a)

type IM a = Index (Mod a)

class Browse a where
  data Back a
  data Forth a
  goforth :: Forth a -> a -> (a, Back a)
  goback :: Back a -> a -> (a, Forth a)

type family Time a

data Link b a = Link (b a) (IM a) (Time a)


data Mod a = Mod {
        _back :: Maybe (Link Back a),
        _forth :: Maybe (Link Forth a)
        }

makeLenses ''Mod

data Res a = Res {
        _res :: a,
        _timestamp :: Time a,
        _history :: IM a
        }
makeLenses ''Res

deriving instance (Show (Time a), Show (IM a), Show a) => Show (Res a)
deriving instance (Eq (Time a), Eq (IM a), Eq a) => Eq (Res a)


cursor :: (Monad m, Browse a, Storage m (Mod a)) => (Time a -> Ordering) -> Res a -> m (Maybe (Res a))
cursor ct r@(Res x (ct -> GT) i) = do
        m <- get i -- get history
        case m of
                Mod _ Nothing -> return $ Just r -- there is nothing beyond, we are actual
                Mod _ (Just (Link f i (ct -> LT))) -> return $ Just r -- the future is too far, we settle
                Mod _ (Just (Link f i t)) -> cursor  ct $ Res (fst $ goforth f x) t i -- let's see the future

cursor ct r@(Res x (ct -> EQ) i) = return $ Just r
cursor ct (Res x _ i) = do
        m <- get i
        case m of
                Mod Nothing _ -> return Nothing -- there was nothing at the time
                Mod (Just (Link b i t)) _ -> ($ Res (fst $ goback b x) t i) $ case ct t of 
                                        LT -> cursor  ct -- let's see the past
                                        _ -> return . Just -- the past is too behind, we settle


atTime :: (Monad m, Browse a, Ord (Time a), Storage m (Mod a)) => Res a -> Time a ->  m (Maybe (Res a))
atTime r t = cursor (compare t) r

-- | step a resource till its top
actual  :: (Monad m, Browse a, Storage m (Mod a)) => Res a -> m (Res a)
actual r = fromJust <$> cursor (const GT) r-- point up, always, it must expose failures only going back

data TimeError = TimeError

-- | insert a modification on the resource actualized, time consecutio is tested here
modify :: (Monad m , Browse a, Ord (Time a), Storage m (Mod a)) => Forth a -> Res a -> Time a -> m (Either TimeError (Res a))
modify  f  r tn = do
        Res x t i <- actual r -- can this be expressed in the Res type ? It's nonsense to pass a Forth for a rewinded Res
        if t < tn then do 
                let (x',b) = goforth f x
                Right <$> Res x' tn <$>  update (Mod (Just $ Link b i t) Nothing)  (Just $ ModifyM (\i' -> set forth . Just $ Link f i' tn) i )
         else return $ Left TimeError

-- | a new resource from its core and boot time
new :: (Monad m , Browse a, Ord (Time a),Storage m (Mod a)) => a -> Time a -> m (Res a)
new x t = Res x t <$> update (Mod Nothing Nothing) Nothing





