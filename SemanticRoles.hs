{-# LANGUAGE FlexibleContexts, TypeFamilies, ScopedTypeVariables, ViewPatterns, StandaloneDeriving, UndecidableInstances  #-}

import Semantics
import Resources
import Roles

import Control.Applicative
import qualified Data.Map as M
import Data.Maybe
import Control.Lens hiding (Index)





checkOwned h (Roled (Owned ((==) h -> True)) _) = True
checkOwned _ _ = False
checkAuthored h (Roled (Authored _ ((==) h -> True)) _) = True
checkAuthored _ _ = False

checkOwnedOrAuthored h = liftA2 (||) (checkOwned h) (checkOwned h)

data Result a = Deferred (Roler a) | Pass (Forth (Roled a))

routeForthNode 
        ::  forall a b .   
        (       Ord a  -- Ord (Generic a) requires
        ,       OP a
        ,       b ~ Node a
        ,       Eq (Hash a) -- (h == h')
        ,       Hash a ~ Hash b -- short circuit for Hash
        ,       Value a ~ HashedObj b -- required by semanticRoles
        ,       Pred a ~ RoledPred a -- required by semanticRoles
        ) 
        =>      Hash b  -- ^ caller
        ->      Forth b  -- ^ modification of a node
        ->      Roled b  -- ^ target of modification, already realized as Roled
        ->      Index (Res (Roled b))
        ->      Maybe (Result b)  -- ^ judgement

routeForthNode h (New OwnedBy (Literal (Hashed ((==) h -> True)))) (Roled Free _) _ = Just (Pass $ Promote h)

routeForthNode h (New AuthoredBy (Literal EmptyHash)) (checkOwned h -> True) i = Just (Deferred $ Roler Promoting h i)
routeForthNode h (Delete AuthoredBy) (checkOwnedOrAuthored h -> True) _ = Just (Pass License)
routeForthNode h (Correct OwnedBy (Substitute EmptyHash)) (checkOwned h -> True) i = Just (Deferred $ Roler Changing h i)
routeForthNode h (Correct AuthoredBy (Substitute EmptyHash)) (checkAuthored h -> True) i = Just (Deferred $ Roler Changing h i)

routeForthNode h f@(New (Generic x) _) (checkOwnedOrAuthored h -> True) _ = Just (Pass $ Semantics f) 
routeForthNode h f@(Delete (Generic x)) (checkOwnedOrAuthored h -> True) _ = Just (Pass $ Semantics f) 
routeForthNode h f@(Correct (Generic x) _) (checkOwnedOrAuthored h -> True) _ = Just (Pass $ Semantics f) 
routeForthNode _ _ _ _ = Nothing


{-
-}

