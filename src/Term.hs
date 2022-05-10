module Term where

import Prelude hiding ((||), (&&), not)
import qualified Data.List as List
import Data.String
import Class

data Term = TAnd Term Term
    | TOr Term Term
    | TBool Bool
    | TNot Term
    | TEq Term Term
    | TContains Term Term
    | TList [Term]
    | TStr String
    | TVar String
    deriving (Show, Eq)

instance IsString Term where
    fromString = TStr

instance Target Term where
    fromList = TList

    TStr x === TStr y = TBool $ x == y
    TBool a === TBool b = TBool $ a == b
    a === b = TEq a b

    contains x (TList xs) = foldr (||) (TBool False) $ (x===) <$> xs
    contains a b = TContains a b

-- TODO: precedence
printTerm :: Term -> String
printTerm (TAnd a b) = unwords [printTerm a, "AND", printTerm b]
printTerm (TOr a b) = unwords [printTerm a, "OR", printTerm b]
printTerm (TNot b) = unwords ["NOT", printTerm b]
printTerm (TVar s) = s
printTerm (TEq a b) = unwords [printTerm a, "=", printTerm b]
printTerm (TStr s) = show s
printTerm (TContains x xs) = mconcat [ "FIND_IN_SET(", printTerm x, ", ", printTerm xs, ")" ]
printTerm (TList []) = show ""
printTerm (TList (x : xs)) = foldr (\a b -> mconcat ["CONCAT(", a, ",", b]) (printTerm x) (map printTerm xs) -- FIXME
printTerm (TBool True) = "TRUE"
printTerm (TBool False) = "FALSE"

instance Algebra Term where
    fromBool = TBool

    TBool True && a = a
    a && TBool True = a
    TBool False && _ = TBool False
    _ && TBool False = TBool False
    a && b = a `TAnd` b

    TBool False || a = a
    TBool True || _ = TBool True
    a || TBool False = a
    _ || TBool True = TBool True
    a || b = a `TOr` b

    not (TBool x) = TBool $ not x
    not x = TNot x

printQuery :: forall a. HasDB a => Term -> String
printQuery clauses = unwords
    [ "SELECT"
    , List.intercalate ", " (columnNames @a)
    , "FROM"
    , tableName @a
    , "WHERE"
    , printTerm clauses
    ]
