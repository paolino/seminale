{-# LANGUAGE DataKinds, GADTs, TypeFamilies, ExistentialQuantification, KindSignatures, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ViewPatterns, StandaloneDeriving, UndecidableInstances, AllowAmbiguousTypes, MultiParamTypeClasses, OverloadedStrings #-}

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
import Data.Bson
import Data.Text
import Data.Time


-- Roled resource states
data Roles = Free | Owned Text | Authored Text Text  deriving (Show)
data Roled a = Roled {
        _core :: a,
        _roles :: Roles
        } deriving (Show)

makeLenses ''Roled

data instance Predication (Roled a) = Author Text | Owner Text deriving (Ord,Eq)
instance Serialize Field (Roled a) where
        parse ("Owner" := String t) = Just $ Owner t
        parse ("Author" := String t) = Just $ Author t
        parse _ = Nothing
        serialize (Owner t) = ("Owner" := String t)
        serialize (Author t) = ("Author" := String t)
instance Struct (Roled a) where
        type SubType (Roled a) = a
        struct x [] = Roled x Free
        struct x [Owner p] = Roled x $ Owned p
        struct x [Author p, Owner q] = Roled x $ Authored q p
        destruct (Roled x Free) = (x,[])
        destruct (Roled x (Owned p)) = (x, [Owner p])
        destruct (Roled x (Authored p q)) = (x,[Owner p, Author q])
        
        

promote :: Text -> Roles  -> Roles 
promote h Free  = Owned h
promote h' (Owned h)  = Authored h h'

license :: Roles -> (Roles , Text)
license (Authored h h') = (Owned h,  h')
license (Owned h) = (Free ,h)

change :: Text -> Roles   -> (Roles, Text)
change h' (Owned h) = (Owned h' ,h)
change h'' (Authored h h')  = (Authored h h'' ,h')

instance (Forth a ~ Back a, History a) => History  (Roled  a)  where
        data Forth (Roled a) = Promote Text
                | License
                | Change Text
                | Semantics (Forth a)
        type Back (Roled a) = Forth (Roled a)
        goforth (Promote h) x = (over roles (promote h) x ,  License)
        goforth License x = (flip (set roles) x *** Promote) $ license (view roles $ x)
        goforth (Change h) x = (flip (set roles) x *** Change) $ change h (view roles $ x) 
        goforth (Semantics f) x = (flip (set core) x *** Semantics) $ goforth f (view core x)
        
        checkForth (Promote _) (Roled _ (Authored _ _)) = False
        checkForth _ _ = True
        goback b = fst . goforth b

owner,author :: Roles -> Maybe Text

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

mapAction :: Action -> Text -> Forth (Roled a)
mapAction Changing = Change
mapAction Promoting = Promote

-- resources to defer roles modification
data Roler a = Roler Action Text Index

data AdminError = DifferentOwner | NotOwned | PromotingBeyondAuthor | AdminSub StepError

-- | the owner is opening a role to someone else. This is the highest type as it links a (Timed (Roled a))
requestRoler 
        ::      (MonadError AdminError m,  Eq Text) 
        =>      Storage m (Timed (Roled a))
        ->      Text 
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
                (       
                       MonadError AdminError m
                ,       Back a ~ Forth a
                ,       History a
                ,       Eq Text
                )
        =>      Storage (ExceptT AdminError m) (Timed (Roled a))
        ->      Storage (ExceptT StepError m) (Step (Roled a))
        ->      Text  -- ^ poster
        ->      UTCTime -- ^ posting time 
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




{-

-------------------------
---- Semantic interface
-------------------------

data  family SemanticPred a
data  RoledPred a = OwnedBy | AuthoredBy | GenericPred (SemanticPred a)  


data  family SemanticObj a
data  HashedObj a = Hashed Text | EmptyHash | GenericObj (SemanticObj a)

type RNode a = M.Map (RoledPred a) (HashedObj a)

getOwner :: Ord (RoledPred a) => RNode a -> Maybe Text
getOwner (M.lookup OwnedBy  -> Just (Hashed  x)) = Just x
getOwner _ = Nothing

getAuthor :: Ord (RoledPred a) => RNode a -> Maybe Text
getAuthor (M.lookup AuthoredBy  -> Just ((Hashed x))) = Just x
getAuthor _ = Nothing

sematicRoles :: (Ord (RoledPred a),Text ~ Hash (RNode a)) => Lens' (RNode a) (Roled (RNode a))
sematicRoles = lens g s where
        g m = (flip Roled m . fromJust) $ (
                liftA2 Authored (getOwner m) (getAuthor m)
                <|>
                Owned <$> (getOwner m)
                <|> pure Free
                )
        s _ (Roled Free m) = M.delete AuthoredBy . M.delete OwnedBy $ m
        s _ (Roled (Owned h) m) =  M.delete AuthoredBy . M.insert OwnedBy (Hashed h) $ m
        s _ (Roled (Authored h h') m) =  M.insert AuthoredBy (Hashed h') . M.insert OwnedBy (Hashed h) $ m

semanticTimed :: (Ord (RoledPred a),Text ~ Hash (RNode a)) => Lens' (RNode a) (Timed (Roled (RNode a)))
semanticTimed lens g s where
        g m = 


{-
data RolerPred = ActionPred | TargetPred  deriving (Ord,Eq)

rolerFromNode ::  forall a . (Text ~ Hash (Node a), Value a ~ Action, Pred a ~ RolerPred) => Roled (Node a) -> Maybe (Roler a)
rolerFromNode (Roled (Owned h) (Node m))  = do
        p <- M.lookup ActionPred m
        o <- M.lookup TargetPred m
        case p of
                Literal p' -> case o of
                        Single (Link Nothing l) -> Just $ Roler p' h l
                        _ -> Nothing
                _ -> Nothing

rolerToNode :: (Text ~ Hash (Node a), Value a ~ Action, Pred a ~ RolerPred) => Roler a -> Roled (Node a)
rolerToNode (Roler p h l) = Roled (Owned h) . Node . M.insert ActionPred (Literal p) $ M.singleton TargetPred (Single (Link Nothing l))
-}

-}
