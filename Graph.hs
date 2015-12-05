
{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, Rank2Types, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, StandaloneDeriving, OverloadedStrings, StandaloneDeriving #-}

module Graph where


import Semantics
import Resources
import Predicates

import Data.Time.Clock
import Data.Bson
import Data.Text
import Data.List


data Graph

type instance Hashed Graph = Text

data instance ObjectsS Graph = Literal Value | Link Integer | Coll [Integer] deriving (Show, Eq)

type ObjSG = ObjS Graph

instance Modify ObjSG where
  data Forth ObjSG = AddLinkF Integer | DeleteLinkF Integer deriving Show
  data Back ObjSG = AddLinkB Integer | DeleteLinkB Integer  deriving Show
  goforth (AddLinkF i) (Meaning (Coll is)) = (Meaning (Coll $ (i:) . delete i $ is), DeleteLinkB i)
  goforth (DeleteLinkF i) (Meaning (Coll is)) = ( Meaning (Coll $ delete i $ is), AddLinkB i)
  goback (AddLinkB i) (Meaning (Coll is)) = (Meaning (Coll $ (i:) . delete i $ is), DeleteLinkF i)
  goback (DeleteLinkB i) (Meaning (Coll is)) = ( Meaning (Coll $ delete i $ is), AddLinkF i)

type instance PredicateS Graph = Text
         
type instance Index Mod (NodeS Graph) = Int

