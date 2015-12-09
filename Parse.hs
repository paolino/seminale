import Storage
import Resources
import Data.Bson

instance Parse (Mod a) where
        data Pred (Mod a) = BackP | ForthP
        data Obj (Mod a) = BackO (Maybe (Mlink (Back a) a)) |  ForthO (Maybe (Mlink (Forth a) a))
        parse (
