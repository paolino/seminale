{-# LANGUAGE DataKinds, GADTs, TypeFamilies, ExistentialQuantification, KindSignatures, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ViewPatterns, StandaloneDeriving, UndecidableInstances, AllowAmbiguousTypes #-}

-- | festival of partiality

module Roles where

import Resources
import Semantics
import qualified Data.Map as M
import Storage
import Control.Arrow
import Control.Lens.TH
import Control.Lens hiding (Index)
import Data.Maybe
import Control.Applicative

type family Hash a

-- Roled resource states
data Roles a = Free | Owned (Hash a) | Authored (Hash a) (Hash a)  
data Roled a = Roled {
        _roles :: Roles a,
        _core :: a
        }

makeLenses ''Roled




promote :: Roled a -> (Hash a) -> Roled a
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

instance (Forth a ~ Back a, Browse a) => Browse  (Roled  a)  where
        type Forth (Roled a) = Roling a
        type Back (Roled a) = Roling a
        goforth (Promote h) x = (promote x h,  License)
        goforth License x = second Promote $ license x
        goforth (Change h) x = second Change $ change x h
        goforth (Semantics f) x = (flip (set core) x *** Semantics) $ goforth f (view core x)
        
        checkForth (Promote _) (Roled (Authored _ _) _) = False
        checkForth _ _ = True
        goback = goforth

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
data Roler a = Roler Action (Hash a) (Index a)

data AdminError a = AdminSub a | DifferentOwner | NotOwned | PromotingBeyondAuthor

-- | the owner is opening a role to someone else
requestRoler 
        :: (Hash a ~ Hash (Timed (Roled a)), Storage m (Timed (Roled a)), Eq (Hash a)) 
        =>      Hash a 
        ->      Action 
        ->      Index (Timed (Roled a)) 
        ->      m (Either (AdminError TimeError) (Roler (Timed (Roled a))))
requestRoler h ac i = do
        x <- pull i
        case owner (view (res . roles) x) of
                Nothing -> return $ Left NotOwned
                Just ((==) h -> True) -> return $ Right $ Roler ac h i
                Just _ -> return $ Left DifferentOwner 
                
-- | someone has found a roler and he's posting to it
postRoler   
        :: forall m a . 
                (       Ord (Time a) 
                ,       Time a ~ Time (Roled a)
                ,       Back a ~ Forth a
                ,       Storage m (Timed (Roled a))
                ,       Storage m (Mod (Roled a))
                ,       Browse a
                ,       Eq (Hash a)
                ,       Hash a ~ Hash (Timed (Roled a))
                )
        => Hash a  -- ^ poster
        -> Time a -- ^ posting time 
        -> Roler (Timed (Roled a))  
        -> m (Either (AdminError TimeError) (Timed (Roled a)))
postRoler h t (Roler ac h' i) = do
        x <- pull i
        case owner (view (res . roles) x) of
                Nothing -> return $ Left NotOwned
                Just ((==) h' -> True) -> over _Left AdminSub <$> modify (mapAction ac h) x t
                Just _ -> return $ Left DifferentOwner 



-------------------------
---- Semantic interface
-------------------------
data  RoledPred a = OwnedBy | AuthoredBy | Generic (Pred a) 

deriving instance Eq (Pred a) => Eq (RoledPred a)
deriving instance Ord (Pred a) => Ord (RoledPred a)

data  HashedObj a = Hashed (Hash a) | Clear a | EmptyHash

getOwner :: (Ord (Pred a), Ord a , Value a ~ HashedObj (Node a),Pred a ~ RoledPred a) => Node a -> Maybe (Hash (Node a))
getOwner (M.lookup OwnedBy . unNode -> Just (Literal (Hashed  x))) = Just x
getOwner _ = Nothing

getAuthor :: (Ord (Pred a), Ord a , Value a ~ HashedObj (Node a),Pred a ~ RoledPred a) => Node a -> Maybe (Hash (Node a))
getAuthor (M.lookup AuthoredBy . unNode -> Just (Literal (Hashed x))) = Just x
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
{-
-}


