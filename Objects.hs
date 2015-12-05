
{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, Rank2Types, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, StandaloneDeriving, OverloadedStrings, StandaloneDeriving #-}

module Objects where


import Semantics
import Resources 
import Predicates

import Data.Bson
import Data.Text
import Data.List


type instance Index Mod (NodeS Value) = Integer

deriving instance Show (Time Value) => Show Timed 
deriving instance Eq (Time Value) => Eq Timed 

type instance Hashed Value = Text

 
data instance ObjectsS Value = Literal Value

type ObjSG = ObjS Value


type instance PredicateS Value = Text
         

