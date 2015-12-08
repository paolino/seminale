
{-# LANGUAGE TypeFamilies, ViewPatterns, TemplateHaskell, Rank2Types, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, EmptyDataDecls, DataKinds, GADTs, StandaloneDeriving, PartialTypeSignatures #-}
module Application where

import Resources (Time, Forth, Res)
import Semantics
import Data.Bson
import Data.Time
import Data.Text
import Control.Applicative
import qualified Data.Map as M
import Index

-- type instance Index Pending = Integer
type instance Time Value = UTCTime

type Hash = Text
data instance Pred Value = Owner | Author | Predication Text deriving (Show,Read,Ord,Eq)

type N = Node Value
type IRV = Index (Res Value)

object :: Val a => a -> Obj Value
object = Semantic . val

newNode :: Hash -> N
newNode = Node . M.singleton Owner . object

owned :: Eq IRV => Hash -> N -> Bool 
owned p (Node m) = maybe False (object p ==) $ M.lookup Owner m

authoredBy :: Eq IRV => Hash -> N -> Bool 
authoredBy p (Node m) = maybe False (object p ==) $ M.lookup Author m <|> M.lookup Owner m

authored ::  N -> Bool 
authored  (Node m) = maybe False (const True) $ M.lookup Author m

disowned ::  N -> Bool 
disowned (Node m) = not  $ M.notMember Owner m

disauthored ::  N -> Bool 
disauthored (Node m) = not  $ M.notMember Author m


check :: Eq IRV =>  Hash -> Forth N -> N -> Bool
check _ (NewF Owner _) = disowned 
check p (NewF (Predication _) _) = authoredBy p

check p (DeleteF Owner) = owned p
check p (DeleteF Author) = liftA2 (||) (authoredBy p) (owned p)
check p (DeleteF (Predication _)) =  authoredBy p
check p (CorrectF (Predication _) _) =  authoredBy p


check _  _ = const False

type IN = Index N

data Post = Post (Forth N) IN

data Proposal = SetAuthor Hash | ChangeOwner Hash | ChangeAuthor Hash 

checkI ::Eq IRV =>   Hash -> Proposal -> N -> Maybe (IN -> Post)
checkI q (ChangeOwner p) (owned p -> True) = Just $ Post (CorrectF Owner (SubstituteF $ val q))
checkI q (ChangeAuthor p) (owned p -> True) = Just $ Post (CorrectF Author (SubstituteF $ val q))
checkI q (SetAuthor p) (liftA2 (||) disauthored (owned p) -> True) = Just $ Post (NewF Author $ object q)
checkI _ _ _ = Nothing


data Pending =  Pending Proposal IN

type IPV = Index Pending

data Change a where 
         Direct :: Hash -> Forth N -> IN -> Response Post
         Open :: Hash -> Pending -> Response IPV
         Close :: Hash -> IPV -> Response Post

type Response a = Change (Maybe a)

realize :: (Eq IRV,Monad m ,Storage m Pending , Storage m N ) => Change a -> m a
realize (Direct p f i) = do
        m <- pull i
        return $ if check p f m then Just $ Post f i else Nothing
realize (Close p j) = do
        Pending t i <- pull j
        fmap ($ i) <$> checkI p t <$> pull i
realize (Open p t@(Pending _ i)) = do
        m <- pull i
        if owned p m then Just <$> push t else return Nothing
                
        
        
        

