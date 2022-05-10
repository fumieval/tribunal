{-# LANGUAGE OverloadedStrings #-}
module Examples where

{-
ghci> whoCanSee rFoo userTeamRequisitionCheck 
SELECT id, teams FROM user WHERE FIND_IN_SET("T-DEV", teams)
ghci> whoCanSee rFoo allowAllToKong 
SELECT * FROM User WHERE user.id = "U-XKONG"
ghci> availableTo fumieval userTeamRequisitionCheck 
SELECT id FROM resource WHERE id = "R-Foo" OR id = "R-Bar" OR id = "R-Baz"
-}

import Prelude hiding ((||), (&&), not, elem)

import Class
import ABAC
import Core
import Term

type Expr = Exp ABAC Boole

whoCanSee :: Resource -> Expr -> IO ()
whoCanSee res policy =
    putStrLn $ printQuery @User $ inverse "allow" Nothing (Just res) policy

availableTo :: User -> Expr -> IO ()
availableTo u policy =
    putStrLn $ printQuery @Resource $ inverse "allow" (Just u) Nothing policy

userTeamRequisitionCheck :: Expr
userTeamRequisitionCheck = If cond roleAllow roleDeny
    where
        cond = dev_team_id `Contains` Var UserTeams
            && Var ResourceId `Contains` resources
        resources = MkList
            [ MkStr $ rid rFoo
            , "R-Bar"
            , "R-Baz"
            ]
        dev_team_id = "T-DEV"

allowAllToKong :: Expr
allowAllToKong = If
    (Var UserId `EqStr` MkStr "U-XKONG")
    roleAllow
    roleDeny

roleAllow :: Expr
roleAllow = Var $ HasRole "allow"

roleDeny :: Expr
roleDeny = Var $ HasRole "deny"

uxkong :: User
uxkong = User
    { uid = "U-XKONG"
    , uteams = []
    }

fumieval :: User
fumieval = User
    { uid = "U-FUMI"
    , uteams = ["T-DEV"]
    }

rFoo :: Resource
rFoo = Resource
    { rid = "R-Foo"
    , rtagIds = []
    }