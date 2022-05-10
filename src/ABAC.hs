module ABAC
    ( ABAC(..)
    , User(..)
    , Resource(..)
    , inverse
    ) where

import Class
import Core
import Term

data ABAC t where
    UserId :: ABAC Str
    UserTeams :: ABAC (List Str)
    ResourceId :: ABAC Str
    ResourceTagIds :: ABAC (List Str)
    HasRole :: String -> ABAC Boole

deriving instance Show (ABAC t)
deriving instance Eq (ABAC t)

instance HasDB User where
    tableName = "user"
    columnNames = ["id", "teams"]

instance HasDB Resource where
    tableName = "resource"
    columnNames = ["id"]

data User = User
    { uid :: String
    , uteams :: [String]
    }

data Resource = Resource
    { rid :: String
    , rtagIds :: [String]
    }

-- Enumerate users that has an access to the specified resource, or vice versa
inverse
    :: String
    -> Maybe User
    -> Maybe Resource
    -> Exp ABAC Boole
    -> Term
inverse role user resource = compile fromVar where
    fromVar :: ABAC a -> Term
    fromVar = \case
        UserId
            | Just u <- user -> TStr $ uid u
            | otherwise -> TVar "id"
        UserTeams
            | Just u <- user -> TList $ map TStr $ uteams u
            | otherwise -> TVar "teams"
        ResourceId
            | Just r <- resource -> TStr $ rid r
            | otherwise -> TVar "id"
        ResourceTagIds
            | Just r <- resource -> TList $ map TStr $ rtagIds r
            | otherwise -> TVar "tag_ids"
        HasRole r -> fromBool $ r == role
