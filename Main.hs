{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Identity

data Context = Schedules | Job | Res
             deriving (Show, Eq, Ord)
                  
type Key = String

data Exp = Where [Key] Val Val
         | Last Val
         | Get Key Val
         | Count Val
         | GreaterEq Val Val
         | Diff Val Val
         deriving (Show, Eq, Ord)
                        
data Entity = Prop Key Val
            deriving (Eq, Ord)

instance Show (Entity) where
  show (Prop k v) = show k ++ ":" ++ show v
  
data Val = IntVal Int
         | BoolVal Bool
         | StringVal String
         | ObjVal [Entity]
         | ListVal [Val]
         | ExpVal Exp
         | ContextVal Context
         | None
         deriving (Eq, Ord)

instance Show (Val) where
  show (IntVal a) = show a
  show (BoolVal a) = show a
  show (StringVal a) = show a
  show (ObjVal a) = show a
  show (ListVal a) = show a
  show (ExpVal a) = show a
  show (ContextVal a) = show a


  
jacky = ObjVal
  [ Prop "name" $ StringVal "Jacky"
  , Prop "id" $ StringVal "1"
  , Prop "date" $ IntVal 6
  , Prop "cooldown" $ IntVal 2
  , Prop "availability" $ IntVal 1
  , Prop "roles" $ ListVal [ ObjVal [ Prop "type" $ StringVal "Leader"
                                    , Prop "kinds" $ ListVal [ StringVal "Sun"
                                                             , StringVal "Morning"]
                                    ]
                           , ObjVal [ Prop "type" $ StringVal "Vocal"
                                    , Prop "kinds" $ ListVal [ StringVal "Sun"
                                                             , StringVal "Morning"]
                                    ]
                           ]
  ]

timmy = ObjVal
  [ Prop "name" $ StringVal "Timmy"
  , Prop "id" $ StringVal "1"
  , Prop "date" $ IntVal 6
  , Prop "cooldown" $ IntVal 2
  , Prop "availability" $ IntVal 1
  , Prop "roles" $ ListVal [ ObjVal [ Prop "type" $ StringVal "Leader"
                                    , Prop "kinds" $ ListVal [ StringVal "Sun"
                                                             , StringVal "Morning"]
                                    ]
                           , ObjVal [ Prop "type" $ StringVal "Vocal"
                                    , Prop "kinds" $ ListVal [ StringVal "Sun"
                                                             , StringVal "Morning"]
                                    ]
                           ]
  ]


lok = ObjVal
  [ Prop "name" $ StringVal "Lok"
  , Prop "id" $ StringVal "1"
  , Prop "date" $ IntVal 6
  , Prop "cooldown" $ IntVal 2
  , Prop "availability" $ IntVal 1
  , Prop "roles" $ ListVal [ ObjVal [ Prop "type" $ StringVal "Leader"
                                    , Prop "kinds" $ ListVal [ StringVal "Sun"
                                                             , StringVal "Morning"]
                                    ]
                           , ObjVal [ Prop "type" $ StringVal "Vocal"
                                    , Prop "kinds" $ ListVal [ StringVal "Sun"
                                                             , StringVal "Morning"]
                                    ]
                           ]
  ]


resources :: [Val]
resources = [jacky, timmy]

job1 = ObjVal
  [ Prop "date" $ IntVal 6
  , Prop "roles" $ ListVal [ StringVal "Leader"
                           , StringVal "Vocal"]
  ]

job2 = ObjVal
  [ Prop "date" $ IntVal 13
  , Prop "roles" $ ListVal [ StringVal "Leader"
                           , StringVal "Vocal"]
  ]

jobs = [job1, job2]

schedules = 
    [ ObjVal [ Prop "job" job1
             , Prop "index" $ IntVal 0
             , Prop "team" $ ListVal [ ObjVal [ Prop "role" $ StringVal "Leader"
                                              , Prop "index" $ IntVal 0
                                              , Prop "res" jacky
                                              ]
                                     , ObjVal [ Prop "role" $ StringVal "Vocal"
                                              , Prop "index" $ IntVal 1
                                              , Prop "res" lok
                                              ]
                                     ]
             ]
    , ObjVal [ Prop "job" job2
             , Prop "index" $ IntVal 1
             , Prop "team" $ ListVal [ ObjVal [ Prop "role" $ StringVal "Leader"
                                              , Prop "index" $ IntVal 0
                                              , Prop "res" timmy]
                                     , ObjVal [ Prop "role" $ StringVal "Vocal"
                                              , Prop "index" $ IntVal 1
                                              , Prop "res" timmy]
                                     ]
             ]  
    ]



rules = [rule1]


genTeam :: Int -> Val -> Val -> Val
genTeam i ro re = ObjVal [ Prop "role" ro
                         , Prop "index" $ IntVal i
                         , Prop "res" re]

genSchedule :: Int -> Val -> Val -> Val
genSchedule i j t = ObjVal [ Prop "index" $ IntVal i
                           , Prop "job" j
                           , Prop "team" t]
                    
schedule :: [Exp] -> [Val] -> [Val] -> [Val] -> [Val]
schedule es schs js re = [ genSchedule i j $ ListVal $ assign es schs j re
                         | (i, j) <- zip [0.. (length js - 1)] js ]
                          
assign :: [Exp] -> [Val] -> Val -> [Val] -> [Val]
assign es schs j re = let (ListVal ros) = get "roles" j
                      in [ genTeam i ro $ match es schs j re
                         | (i,ro) <- zip [0.. (length ros - 1)] ros]
                         
match :: [Exp] -> [Val] -> Val -> [Val] -> Val
-- match es schs j rs = filter (\r -> evalRule es schs j r) rs
match es schs j re = head re

test = schedule rules [] jobs resources

rule1 = GreaterEq
         (ExpVal (Diff 
                  (ExpVal $ Count $ ContextVal Schedules)
                  (ExpVal (Get "index" (ExpVal $ Last $ ExpVal $
                                        Where ["team", "res", "name"]
                                        (StringVal "Jacky") (ContextVal Schedules)
                                       )
                          )
                  )
                 )
         )
         (ExpVal (Get "cooldown" $ ContextVal Res))

rule2 = Where ["index"] (IntVal 0) (ContextVal Schedules)
result = eval (ListVal test, job1, jacky) rule1


getVal :: (Val,Val,Val) -> Val -> Val
getVal ctx (ExpVal e) = eval ctx e
getVal (schs, j, r) (ContextVal e) 
  | e == Schedules = schs
  | e == Job = j
  | e == Res = r
               
getVal ctx v = v

eval :: (Val,Val,Val) -> Exp -> Val

eval ctx (Where ks v os) = where' ks (getVal ctx v) (getVal ctx os)

eval ctx (Last v) = let (ListVal res) = (getVal ctx v)
                    in last res

eval ctx (Get k v) = get k (getVal ctx v)

eval ctx (Count v) = let (ListVal res) = (getVal ctx v)
                     in IntVal $ length res

eval ctx (GreaterEq v1 v2) = BoolVal $ (getVal ctx v1) >= (getVal ctx v2)

eval ctx (Diff v1 v2) = let (IntVal i1) = (getVal ctx v1)
                            (IntVal i2) = (getVal ctx v2)
                        in IntVal $ i1 - i2
                                           
where' :: [Key] -> Val -> Val -> Val
where' ks v (ListVal os) = ListVal [ o | o <- os
                                       , checkKeyVal ks v o]

checkKeyVal :: [Key] -> Val -> Val -> Bool
checkKeyVal [] v o = False
checkKeyVal (k:ks) v o = let res = (get k o)
                         in case res of
                           (ListVal ls) -> or [checkKeyVal ks v l | l <- ls]
                           (ObjVal ov) -> checkKeyVal ks v res
                           _ -> res == v
                           
get :: Key -> Val -> Val
get k (ObjVal ps) = let res = [v | (Prop k' v) <- ps
                                    , k' == k]
                       in if length res == 0 then None
                          else head res

select :: Key -> [Val] -> [Val]
select k vs =  [ v | (ObjVal vs') <- vs
                   , (Prop k' v) <- vs'
                   , k' == k]

