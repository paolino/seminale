{-# LANGUAGE DataKinds, GADTs, TypeFamilies, ExistentialQuantification, KindSignatures, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ViewPatterns, StandaloneDeriving, UndecidableInstances, AllowAmbiguousTypes #-}

-- | festival of partiality

module Roles where

import Storage
import Resources
import qualified Data.Map as M
import Control.Arrow
import Control.Lens.TH
import Control.Lens hiding (Index)
import Data.Maybe
import Control.Applicative
import Control.Monad.Except

type family Hash a

-- Roled resource states
data Roles a = Free | Owned (Hash a) | Authored (Hash a) (Hash a)  
data Roled a = Roled {
        _roles :: Roles a,
        _core :: a
        }

makeLenses ''Roled

promote :: Roled a -> Hash a -> Roled a
promote (Roled Free x) h = Roled (Owned h) x
promote (Roled (Owned h) x) h' = Roled (Authored h h') x

license :: Roled a -> (Roled a, Hash a)
license (Roled (Authored h h') x) = (Roled (Owned h) x,  h')
license (Roled (Owned h) x) = (Roled Free x,h)

change :: Roled a -> Hash a -> (Roled a, Hash a)
change (Roled (Owned h) x) h' = (Roled (Owned h') x ,h)
change (Roled (Authored h h') x) h'' = (Roled (Authored h h'') x ,h')

data Roling a = Promote (Hash a)
                | License
                | Change (Hash a)
                | Semantics (Forth a)

instance (Forth a ~ Back a, History a) => History  (Roled  a)  where
        type Forth (Roled a) = Roling a
        type Back (Roled a) = Roling a
        goforth (Promote h) x = (promote x h,  License)
        goforth License x = second Promote $ license x
        goforth (Change h) x = second Change $ change x h
        goforth (Semantics f) x = (flip (set core) x *** Semantics) $ goforth f (view core x)
        
        checkForth (Promote _) (Roled (Authored _ _) _) = False
        checkForth _ _ = True
        goback b = fst . goforth b
owner,author :: Roles a -> Maybe (Hash a)

owner Free = Nothing
owner (Owned p) = Just p
owner (Authored p _) = Just p

author (Authored _ a) = Just a
author _ = Nothing
---------------------
-- contracting roles
---------------------

-- possible openings: changing actual level of role or promoting to the next
data Action =  Changing | Promoting

mapAction :: Action -> Hash a -> Roling a
mapAction Changing = Change
mapAction Promoting = Promote

-- resources to defer roles modification
data Roler a = Roler Action (Hash a) Index

data AdminError = DifferentOwner | NotOwned | PromotingBeyondAuthor | AdminSub StepError

-- | the owner is opening a role to someone else
requestRoler 
        ::      (MonadError AdminError m, Hash a ~ Hash (Timed (Roled a)),  Eq (Hash a)) 
        =>      Storage m (Timed (Roled a))
        ->      Hash a 
        ->      Action 
        ->      Index 
        ->      m (Roler (Timed (Roled a)))
requestRoler s h ac i = do
        x <- pull s i
        case owner (view (res . roles) x) of
                Nothing -> throwError NotOwned
                Just ((==) h -> True) -> return $ Roler ac h i
                Just _ -> throwError  DifferentOwner 
                
-- | someone has found a roler and he's posting to it
postRoler   
        :: forall m a . 
                (       Ord (Time a) 
                ,       Time a ~ Time (Step (Roled a))
                ,       Time a ~ Time (Roled a)
                ,       MonadError AdminError m
                ,       Back a ~ Forth a
                ,       History a
                ,       Eq (Hash a)
                ,       Hash a ~ Hash (Timed (Roled a))
                )
        =>      Storage (ExceptT AdminError m) (Timed (Roled a))
        ->      Storage (ExceptT StepError m) (Step (Roled a))
        ->      Hash a  -- ^ poster
        ->      Time a -- ^ posting time 
        ->      Roler (Timed (Roled a))  
        ->      ExceptT AdminError m (Timed (Roled a))
postRoler st sr h t (Roler ac h' i) = do
        x <- pull st i -- get the resource to be modified
        case owner (view (res . roles) x) of
                Nothing -> throwError NotOwned
                Just ((==) h' -> True) -> 
                        mapExceptT (fmap $ over _Left AdminSub) 
                                $ modify sr (mapAction ac h) x t 
                Just _ -> throwError DifferentOwner 



-------------------------
---- Semantic interface
-------------------------
data  RoledPred = OwnedBy | AuthoredBy deriving (Eq,Ord) 


data  HashedObj a = Hashed (Hash a) | EmptyHash


getOwner :: M.Map RoledPred (HashedObj t) -> Maybe (Hash t)
getOwner (M.lookup OwnedBy  -> Just (Hashed  x)) = Just x
getOwner _ = Nothing

{-
getAuthor :: (Ord (Pred a), Ord a) => Node a -> Maybe (Hash (Node a))
getAuthor (M.lookup AuthoredBy  -> Just ((Hashed x))) = Just x
getAuthor _ = Nothing

sematicRoles :: forall a . (OP a, Ord a , Value a ~ HashedObj (Node a),Pred a ~ RoledPred a) => Lens' (Node a) (Roled (Node a))
sematicRoles = lens g s where
        g m = (flip Roled m . fromJust) $ (
                liftA2 Authored (getOwner m) (getAuthor m)
                <|>
                Owned <$> (getOwner m)
                <|> pure Free
                )
        s _ (Roled Free (Node m)) = Node . M.delete AuthoredBy . M.delete OwnedBy $ m
        s _ (Roled (Owned h) (Node m)) = Node . M.delete AuthoredBy . M.insert OwnedBy (Literal $ Hashed h) $ m
        s _ (Roled (Authored h h') (Node m)) = Node . M.insert AuthoredBy (Literal (Hashed h')) . M.insert OwnedBy (Literal (Hashed h)) $ m


data RolerPred = ActionPred | TargetPred  deriving (Ord,Eq)

rolerFromNode ::  forall a . (Hash a ~ Hash (Node a), Value a ~ Action, Pred a ~ RolerPred) => Roled (Node a) -> Maybe (Roler a)
rolerFromNode (Roled (Owned h) (Node m))  = do
        p <- M.lookup ActionPred m
        o <- M.lookup TargetPred m
        case p of
                Literal p' -> case o of
                        Single (Link Nothing l) -> Just $ Roler p' h l
                        _ -> Nothing
                _ -> Nothing

rolerToNode :: (Hash a ~ Hash (Node a), Value a ~ Action, Pred a ~ RolerPred) => Roler a -> Roled (Node a)
rolerToNode (Roler p h l) = Roled (Owned h) . Node . M.insert ActionPred (Literal p) $ M.singleton TargetPred (Single (Link Nothing l))
-}


