{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}
import Resources
import Application
import Semantics
import Index
import Storage
import Data.Vault.Lazy
import Data.Bson
import Text.Read
import Data.Time.Clock
import qualified Data.Map as M


type K = Key (Res Value)

data Request 
        = New 
        | Get Int
        | Delete Int
        | Update Int (Forth N) deriving (Read)

deriving instance Read (Forth N)
deriving instance Read (Forth (Obj Value))
deriving instance Read (Obj Value)
deriving instance Read (Link Value)
deriving instance Read ( Value)
deriving instance Read (IR Value)
data Logged = Logged Hash Request         


test = do
        print "start"
        let loop v m c = do
                l <- readMaybe <$> getLine
                t <- getCurrentTime
                case l of 
                        Just (Logged h New) -> do
                                Right (i,v') <- drive (new (newNode h) t) v 
                                print $ "Nuova risorsa " ++ show c
                                loop v' (M.insert c i) (c + 1)
                        _ -> loop v m c
        loop empty (M.empty) 0
                        

                
                                 
                                
                
                
