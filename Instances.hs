{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, Rank2Types, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, EmptyDataDecls, DataKinds, GADTs, StandaloneDeriving #-}

module Instances where

import Predicates
import Resources
import Semantics

-- debugging ---------


deriving instance (Show (Pred a), Show (Obj a), Show (Forth (Obj a))) => Show (Forth (Node a))
deriving instance (Show (Pred a), Show (Obj a), Show (Back (Obj a))) => Show (Back (Node a))

deriving instance (Show (PredicateS a)) => Show (Pred (Semantics a))
deriving instance (Show (ObjS a) , Show (Hashed a), Show (ObjectsS a)) => Show (Obj (Semantics a))


deriving instance (Eq (Obj (Semantics a)), Eq (PredicateS a)) => Eq (Node (Semantics a))
deriving instance (Eq (Hashed a),Eq (ObjectsS a)) => Eq (Obj (Semantics a))


---- Data.Map.Map constraints
deriving instance (Ord (PredicateS a)) => Ord (Pred (Semantics a))

---- predicate selection
deriving instance Eq (PredicateS a)=> Eq (Pred (Semantics a))

