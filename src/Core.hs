{-# LANGUAGE QuantifiedConstraints #-}
module Core where

import Prelude hiding ((||), (&&), not)
import Data.String
import Class

data Typ = Boole
    | Str
    | List Typ

data Exp v t where
    Contains :: Exp v a -> Exp v (List a) -> Exp v Boole
    If :: Exp v Boole -> Exp v a -> Exp v a -> Exp v a
    (:&) :: Exp v Boole -> Exp v Boole -> Exp v Boole
    (:|) :: Exp v Boole -> Exp v Boole -> Exp v Boole
    Not :: Exp v Boole -> Exp v Boole
    MkBool :: Bool -> Exp v Boole
    MkStr :: String -> Exp v Str
    EqStr :: Exp v Str -> Exp v Str -> Exp v Boole
    MkList :: [Exp v a] -> Exp v (List a)
    Var :: v a -> Exp v a

deriving instance (forall a. Show (v a)) => Show (Exp v t)

instance s ~ Str => IsString (Exp v s) where
    fromString = MkStr

instance t ~ Boole => Algebra (Exp v t) where
    fromBool = MkBool
    (&&) = (:&)
    (||) = (:|)
    not = Not

compile :: forall v t a. (Target a) => (forall x. v x -> a) -> Exp v t -> a
compile fromVar = go where
    go :: Exp v x -> a
    go = \case
        f :& g -> go f && go g
        f :| g -> go f || go g
        Not f -> not $ go f
        EqStr f g -> go f === go g
        If p t f -> ifThenElse (go p) (go t) (go f)
        Contains x xs -> go x `contains` go xs
        MkStr str -> fromString str
        MkList xs -> fromList $ map go xs
        MkBool b -> fromBool b
        Var v -> fromVar v
