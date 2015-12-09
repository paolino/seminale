
{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, Rank2Types, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, EmptyDataDecls, DataKinds, GADTs, StandaloneDeriving, ScopedTypeVariables, ConstraintKinds #-}

module Semantics where

import qualified Data.Map as M
import Data.List (delete)
import Storage hiding (delete)

type family Value a

type family Time a

data family Index a

data Link a = Link (Maybe (Time a)) (Index a) 


data Obj a 
  = Literal (Value a)
  | Single (Link a) 
  | Coll [Link a] 

type family Pred a 
newtype Node a = Node {unNode :: M.Map (Pred a) (Obj a)}



type Convert a b = Lens' (Node a) (Node b)




{-
data ModObj a = AddLink (Link a) | DeleteLink (Link a) | ChangeLink (Link a) | Substitute (Value a)

deriving instance (Eq (Time a), Eq (Index a),Eq (Value a)) => Eq (Obj a)
deriving instance (Ord (Time a), Ord (Index a),Ord (Value a)) => Ord (Obj a)


instance Eq (Link a) => Browse (Obj a) where
        type Forth (Obj a) = ModObj  a
        type Back (Obj a) = ModObj a
        goforth (AddLink i) (Coll is) = ((Coll $ (i:) . delete i $ is), DeleteLink i)
        goforth (DeleteLink i) (Coll is) = (  (Coll $ delete i $ is), AddLink i) 
        goforth (ChangeLink i) (Single i') = (Single i, ChangeLink i')
        goforth (Substitute x) (Literal x') = (Literal x, Substitute x')
        checkForth _ _ = True -- check for dups ?!
        goback = goforth
-}


data ModPred  a = New (Pred a) (Obj a) 
                | Delete (Pred a) 
                | Correct (Pred a) (Forth (Obj a))

deriving instance (Eq (Pred a), Eq (Time a), Eq (Value a), Eq (Index a), Eq (ModObj  a)) => Eq (ModPred a)
deriving instance (Ord (Pred a), Ord (Time a), Ord (Value a), Ord (Index a), Ord (ModObj  a)) => Ord (ModPred a)

type OP  a = (Ord (Pred a), Ord (Time a), Ord (Value a), Ord (Index a), Ord (ModObj a))
-- delegate objects modification to their Browse instance
instance  (Ord (Pred a), Eq (Link a)) => Browse (Node a) where
        type Forth (Node  a) = ModPred  a
        type Back (Node  a) = ModPred  a
        goforth (New p o) (Node m)  = (Node $ M.insert p o m, Delete p) 
        goforth (Delete p ) (Node m) = (Node $ M.delete p m, New p o) where
                Just o = M.lookup p m
        goforth (Correct p f) (Node m) = (Node $ M.insert p o m, Correct p f') where
                Just (o,f') = goforth f <$> M.lookup p m
        goback = goforth
        checkForth _ _ = True


data PredMod = LBack | LForth deriving (Eq,Ord)
data ObjMod a = OBack (Mlink (Back a) a) | OForth (Mlink (Forth a) a)
fromOBack (Literal (OBack x)) = x
fromOForth (Literal (OForth x)) = x
modFromNode :: (Pred a ~ PredMod, ObjMod a ~ Value a) => Node a -> Mod a
modFromNode (Node m) = Mod  (fromOBack <$> M.lookup LBack m) 
        (fromOForth <$> M.lookup LForth m)

follow :: forall a  m . (Time a ~ Time (Timed a), Time (Mod a) ~ Time a, Ord (Time a), Browse a, Storage m (Mod a), Storage m (Timed a)) => Link (Timed a) -> m (Maybe (Timed a))
follow (Link Nothing i) = Just <$> (pull i >>= actual)
follow (Link (Just t) i) = pull i >>= flip atTime t
