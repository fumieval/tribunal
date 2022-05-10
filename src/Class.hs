module Class (Algebra(..), Target(..), HasDB(..)) where

import Prelude hiding ((||), (&&), not)
import Data.String

infixr 2 ||
infixr 3 &&

class Algebra a where
    fromBool :: Bool -> a
    not :: a -> a
    (&&) :: a -> a -> a
    (||) :: a -> a -> a

instance Algebra Bool where
    fromBool = id
    not False = True
    not True = False
    True && True = True
    _ && _ = False
    False || False = True
    _ || _ = True

class (IsString a, Algebra a) => Target a where
    fromList :: [a] -> a
    (===) :: a -> a -> a
    contains :: a -> a -> a
    ifThenElse :: a -> a -> a -> a
    ifThenElse p t f = p && t || not p && f

class HasDB a where
    tableName :: String
    columnNames :: [String]
