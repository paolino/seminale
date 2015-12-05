
{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, Rank2Types, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, EmptyDataDecls, DataKinds, GADTs, StandaloneDeriving, ScopedTypeVariables #-}

module Semantics where

import Resources (Res, Browse (Forth,Back,goforth,goback),Time, actual,atTime,Mod )
import qualified Data.Map as M
import Data.List (delete)
import Index


data family Pred a 

type IR a = Index (Res a)

data Link a = Link (Maybe (Time a)) (IR a) 

deriving instance (Eq (Time a), Eq (IR a)) => Eq (Link a)

data Obj a 
  = Semantic a
  | Single (Link a) 
  | Coll [Link a] 

deriving instance (Eq (Time a), Eq (IR a),Eq a) => Eq (Obj a)
instance Eq (Link a) => Browse (Obj a) where
        data Forth (Obj a) = AddLinkF (Link a) | DeleteLinkF (Link a) | ChangeLinkF (Link a) | SubstituteF a
        data Back (Obj a) = AddLinkB (Link a) | DeleteLinkB (Link a) | ChangeLinkB (Link a) | SubstituteB a
        goforth (AddLinkF i) (Coll is) = ((Coll $ (i:) . delete i $ is), DeleteLinkB i)
        goforth (DeleteLinkF i) (Coll is) = (  (Coll $ delete i $ is), AddLinkB i) 
        goforth (ChangeLinkF i) (Single i') = (Single i, ChangeLinkB i')
        goforth (SubstituteF x) (Semantic x') = (Semantic x, SubstituteB x')
        goback (AddLinkB i) (Coll is) = ( (Coll $ (i:) . delete i $ is), DeleteLinkF i)
        goback (DeleteLinkB i) (Coll is) = (  (Coll $ delete i $ is), AddLinkF i)
        goback (ChangeLinkB i) (Single i') = (Single i, ChangeLinkF i')
        goback (SubstituteB x) (Semantic x') = (Semantic x, SubstituteF x')


newtype Node a = Node (M.Map (Pred a) (Obj a)) 

-- delegate objects modification to their Browse instance
instance  (Ord (Pred a), Eq (Link a)) => Browse (Node a) where
        data Forth (Node a) 
                = NewF (Pred a) (Obj a) 
                | DeleteF (Pred a) 
                | CorrectF (Pred a) (Forth (Obj a))
        data Back (Node a) 
                = DeleteB (Pred a) 
                | NewB (Pred a) (Obj a)
                | CorrectB (Pred a) (Back (Obj a))
        goforth (NewF p o) (Node m)  = (Node $ M.insert p o m, DeleteB p) 
        goforth (DeleteF p ) (Node m) = (Node $ M.delete p m, NewB p o) where
                Just o = M.lookup p m
        goforth (CorrectF p f) (Node m) = (Node $ M.insert p o m, CorrectB p f') where
                Just (o,f') = goforth f <$> M.lookup p m
        goback (DeleteB p) (Node m)  = (Node $ M.delete p m, NewF p o) where
                Just o = M.lookup p m
        goback (NewB p o) (Node m)  = (Node $ M.insert p o m, DeleteF p) 
        goback (CorrectB p b) (Node m) = (Node $ M.insert p o m, CorrectF p b') where
                Just (o,b') = goback b <$> M.lookup p m


follow :: forall a m . (Ord (Time a), Browse a, Storage m (Mod a), Storage m (Res a)) => Link a -> m (Maybe (Res a))
follow (Link Nothing i) = Just <$> (get i >>= actual)
follow (Link (Just t) i) = get i >>= flip atTime t

