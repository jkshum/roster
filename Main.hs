{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad   
import Control.Monad.Reader
import Control.Monad.Identity

data Context = Schedules | Job | Res
             deriving (Show, Eq, Ord)
                      
type Key = String

data Exp = Where [Key] Exp
         | Last Val
         | Get Key Val
         | Count Val
         | Group [Key] Val
         | GreaterEq Val Val
         | Eq Val Val
         | Diff Val Val
         | Any Val Val
         | In Val Val
         | Not Val
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
  show (ObjVal a) = "obj" ++ show a 
  show (ListVal a) = "list" ++ show a
  show (ExpVal a) = show a
  show (ContextVal a) = show a
  show None = show "None"
  
jacky = ObjVal
  [ Prop "name" $ StringVal "Jacky"
  , Prop "id" $ StringVal "1"
  , Prop "blockedDates" $ ListVal [IntVal 6]
  , Prop "cooldown" $ IntVal 2
  , Prop "availability" $ IntVal 5
  , Prop "roles" $ ListVal [ ObjVal [ Prop "type" $ StringVal "Leader"
                                    , Prop "kinds" $ ListVal [ StringVal "Sat"
                                                             , StringVal "Sun"
                                                             , StringVal "Morning"]
                                    ]
                           , ObjVal [ Prop "type" $ StringVal "Vocal"
                                    , Prop "kinds" $ ListVal [ StringVal "Sat"]
                                    ]
                           ]
  ]


timmy = ObjVal
  [ Prop "name" $ StringVal "Timmy"
  , Prop "id" $ StringVal "1"
  , Prop "blockedDates" $ ListVal [IntVal 13]
  , Prop "cooldown" $ IntVal 2
  , Prop "availability" $ IntVal 5
  , Prop "roles" $ ListVal [ ObjVal [ Prop "type" $ StringVal "Leader"
                                    , Prop "kinds" $ ListVal [ StringVal "Sun"
                                                             , StringVal "Morning"]
                                    ]
                           ]
  ]


lok = ObjVal
  [ Prop "name" $ StringVal "Lok"
  , Prop "id" $ StringVal "1"
  , Prop "blockedDates" $ ListVal [ IntVal 20]
  , Prop "cooldown" $ IntVal 2
  , Prop "availability" $ IntVal 5
  , Prop "roles" $ ListVal [ ObjVal [ Prop "type" $ StringVal "Vocal"
                                    , Prop "kinds" $ ListVal [ StringVal "Sun"
                                                             , StringVal "Sat"
                                                             , StringVal "Morning"]
                                    ]
                           ]
  ]

chung = ObjVal
  [ Prop "name" $ StringVal "chung"
  , Prop "id" $ StringVal "1"
  , Prop "blockedDates" $ ListVal [ IntVal 20]
  , Prop "cooldown" $ IntVal 2
  , Prop "availability" $ IntVal 5
  , Prop "roles" $ ListVal [ ObjVal [ Prop "type" $ StringVal "Vocal"
                                    , Prop "kinds" $ ListVal [ StringVal "Sun"
                                                             , StringVal "Sat"
                                                             , StringVal "Morning"]
                                    ]
                           ]
  ]

long = ObjVal
  [ Prop "name" $ StringVal "long"
  , Prop "id" $ StringVal "1"
  , Prop "blockedDates" $ ListVal [ IntVal 20]
  , Prop "cooldown" $ IntVal 2
  , Prop "availability" $ IntVal 5
  , Prop "roles" $ ListVal [ ObjVal [ Prop "type" $ StringVal "Vocal"
                                    , Prop "kinds" $ ListVal [ StringVal "Sun"
                                                             , StringVal "Sat"
                                                             , StringVal "Morning"]
                                    ]
                           ]
  ]


resources :: [Val]
resources = [jacky, timmy, lok, chung, long]

job1 = ObjVal
  [ Prop "date" $ IntVal 6
  , Prop "id" $ StringVal "0"
  , Prop "kind" $ StringVal "Sat"
  , Prop "role" $ StringVal "Leader"
  ]

job2 = ObjVal
  [ Prop "date" $ IntVal 6
  , Prop "id" $ StringVal "1"
  , Prop "kind" $ StringVal "Sat"
  , Prop "role" $ StringVal "Vocal"
  ]

job3 = ObjVal
  [ Prop "date" $ IntVal 6
  , Prop "id" $ StringVal "2"
  , Prop "kind" $ StringVal "Sat"
  , Prop "role" $ StringVal "Vocal"
  ]
  
job4 = ObjVal
  [ Prop "date" $ IntVal 6
  , Prop "id" $ StringVal "3"
  , Prop "kind" $ StringVal "Morning"
  , Prop "role" $ StringVal "Leader"
  ]


job5 = ObjVal
  [ Prop "date" $ IntVal 6
  , Prop "id" $ StringVal "4"
  , Prop "kind" $ StringVal "Morning"
  , Prop "role" $ StringVal "Vocal"
  ]


job6 = ObjVal
  [ Prop "date" $ IntVal 6
  , Prop "id" $ StringVal "5"
  , Prop "kind" $ StringVal "Morning"
  , Prop "role" $ StringVal "Vocal"
  ]
  
job7 = ObjVal
  [ Prop "date" $ IntVal 6
  , Prop "id" $ StringVal "6"
  , Prop "kind" $ StringVal "Morning"
  , Prop "role" $ StringVal "Vocal"
  ]

job8 = ObjVal
  [ Prop "date" $ IntVal 13
  , Prop "id" $ StringVal "7"
  , Prop "kind" $ StringVal "Sun"
  , Prop "role" $ StringVal "Leader"
  ]


job9 = ObjVal
  [ Prop "date" $ IntVal 13
  , Prop "id" $ StringVal "8"
  , Prop "kind" $ StringVal "Sun"
  , Prop "role" $ StringVal "Leader"
  ]
job10 = ObjVal
  [ Prop "date" $ IntVal 13
  , Prop "id" $ StringVal "9"
  , Prop "kind" $ StringVal "Sun"
  , Prop "role" $ StringVal "Vocal"
  ]
  
job11 = ObjVal
  [ Prop "date" $ IntVal 13
  , Prop "id" $ StringVal "10"
  , Prop "kind" $ StringVal "Sun"
  , Prop "role" $ StringVal "Vocal"
  ]





jobs = [job1, job2, job3, job4, job5, job6, job7, job8, job9, job10, job11]

schedules = 
  [ ObjVal [ Prop "job" job1
            , Prop "index" $ IntVal 0
            , Prop "res" jacky]
  , ObjVal [ Prop "job" job2
            , Prop "index" $ IntVal 1
            , Prop "res" timmy]
  , ObjVal [ Prop "job" job3
            , Prop "index" $ IntVal 2
            , Prop "res" lok]
  , ObjVal [ Prop "job" job4
            , Prop "index" $ IntVal 3
            , Prop "res" timmy]
  , ObjVal [ Prop "job" job5 
            , Prop "index" $ IntVal 4
            , Prop "res" lok]
  , ObjVal [ Prop "job" job6 
            , Prop "index" $ IntVal 5
            , Prop "res" chung]

  ]


rule1 = GreaterEq
         (ExpVal (Diff 
                  (ExpVal $ Count $ ContextVal Schedules)
                  (ExpVal (Get "index" (ExpVal $ Last $ ExpVal $
                                        Where ["team", "res", "name"]
                                        $ Eq (ExpVal $ Get "name" $ ContextVal Res)
                                        (ContextVal Schedules)
                                       )
                          )
                  )
                 )
         )
         (ExpVal (Get "cooldown" $ ContextVal Res))

rule2 = Not $ ExpVal $ In (ExpVal (Get "date" $ ContextVal Job)) (ExpVal (Get "blockedDates" $ ContextVal Res))
rule3 = GreaterEq
        (ExpVal $ Get "availability" $ ContextVal Res)
        (ExpVal $ Count $ ExpVal $ Where ["team", "res", "name"] $ Eq (ExpVal $ Get "name" $ ContextVal Res) (ContextVal Schedules))
        
rule4 = GreaterEq (ExpVal $ Count $ ExpVal $ Where ["type"] $ Eq (ExpVal $ Get "role" $ ContextVal Job) (ExpVal $ Get "roles" $ ContextVal Res)) (IntVal 1)


-- rule5

rule5 = Eq (ExpVal $Count $ ExpVal $ Where ["team", "res", "name"] $
            Eq (ExpVal $ Get "name" $ ContextVal Res)
            (ExpVal (Where ["job", "id"] $ Eq (ExpVal $ Get "id" $ ContextVal Job)
                    (ContextVal Schedules)))
           )
        (IntVal 0)

-- rule5 = Where ["type"] $ Eq (StringVal "Leader") (ExpVal $ Get "roles" jacky)

rule6 = (Diff 
         (ExpVal $ Count $ ContextVal Schedules)
         (ExpVal (Get "index" (ExpVal $ Last $ ExpVal $
                               Where ["team", "res", "name"]
                               $ Eq (ExpVal $ Get "name" $ ContextVal Res)
                               (ContextVal Schedules)
                              )
                 )
         )
        )

rules = [rule1, rule2, rule3, rule4, rule5]




-- rule1 = GreaterEq
--          (ExpVal (Diff 
--                   (ExpVal $ Count $ ContextVal Schedules)
--                   (ExpVal (Get "index" (ExpVal $ Last $ ExpVal $
--                                         Where ["res", "name"]
--                                         $ Eq (ExpVal $ Get "name" $ ContextVal Res)
--                                         (ContextVal Schedules)
--                                        )
--                           )
--                   )
--                  )
--          )
--          (ExpVal (Get "cooldown" $ ContextVal Res))

result = eval (ListVal schedules, job1, jacky) $                                         Group ["res", "name"]  (ContextVal Schedules)


test = schedule rules [None, None] jobs resources

displaySchedules :: [Val] -> IO ()
displaySchedules [] = putStrLn ""
displaySchedules (sch:schs) =
  do
    let res = get "date" $ get "job" sch
      in case res of
      None -> putStrLn ""
      (IntVal date) -> putStrLn $ show date
        
    let team = get "name" $ get "res" $ sch
      in case team of
      None -> putStrLn ""
      (StringVal ts) -> putStrLn $ show ts

    displaySchedules schs
  

genSchedule :: Int -> Val -> Val -> Val
genSchedule i job res = ObjVal [ Prop "index" $ IntVal i
                               , Prop "job" job
                               , Prop "res" res]

schedule :: [Exp] -> [Val] -> [Val] -> [Val] -> [Val]
schedule exps schs [] res = schs
schedule exps schs (job:jobs) res = let re = match exps schs job res
                                        i = length schs
                                    in let sch = genSchedule i job re
                                       in schedule exps (schs ++ [sch]) jobs res

match :: [Exp] -> [Val] -> Val -> [Val] -> Val
match es schs j rs =
  let found = filter (\r ->
                       and [ let (BoolVal res) = eval (ListVal schs, j, r) e
                             in res | e <- es] ) rs
  in if length found == 0 then None
     else head found

getVal :: (Val,Val,Val) -> Val -> Val
getVal ctx (ExpVal e) = eval ctx e
getVal (schs, j, r) (ContextVal e) 
  | e == Schedules = schs
  | e == Job = j
  | e == Res = r
getVal ctx v = v


noneToZero :: Val -> Val
noneToZero None = IntVal 0
noneToZero v = v

eval :: (Val,Val,Val) -> Exp -> Val

eval ctx (Where ks (Eq v1 v2)) = where' (==) ks (getVal ctx v1) (getVal ctx v2)
eval ctx (Where ks (In v1 v2)) = where' valIn ks (getVal ctx v1) (getVal ctx v2)
eval ctx (Where ks (Any v1 v2)) = where' any' ks (getVal ctx v1) (getVal ctx v2)

 -- where' ks (getVal ctx v) (getVal ctx os)

eval ctx (Last v) = let (ListVal res) = (getVal ctx v)
                    in if length res == 0 then None
                       else last res

eval ctx (Get k v) = get k (getVal ctx v)

eval ctx (Count v) = let (ListVal res) = (getVal ctx v)
                     in IntVal $ length res

eval ctx (GreaterEq v1 v2) = BoolVal $ (getVal ctx v1) >= (getVal ctx v2)

eval ctx (Eq v1 v2) = BoolVal $ (getVal ctx v1) == (getVal ctx v2)

eval ctx (Diff v1 v2) = liftIntVal2 (-) (noneToZero $ getVal ctx v1) (noneToZero $ getVal ctx v2)

eval ctx (In v1 v2) = BoolVal $ valIn (getVal ctx v1) (getVal ctx v2)

eval ctx (Not v) = let (BoolVal res) = getVal ctx v                         
                   in BoolVal $ not res

valIn :: Val -> Val -> Bool
valIn v1 (ListVal vs) = elem v1 vs

group' :: [Key] -> Val -> [Val]
group' ks (ListVal os) = let groups = groupBy (\x y -> (getByKeys ks x) == (getByKeys ks y)) os
                         in [ ObjVal [ Prop "index" $ IntVal i
                                     , Prop "objs" $ ListVal group]
                              | (i, group) <- zip [0.. length groups -1] groups]
any' :: Val -> Val -> Bool
any' (ListVal xs) v = elem v xs

where' :: (Val -> Val -> Bool) -> [Key] -> Val -> Val -> Val
where' fn ks v (ListVal os) = ListVal [ o | o <- os
                                          , checkKeyVal fn ks v o]
                              
checkKeyVal :: (Val -> Val -> Bool) -> [Key] -> Val -> Val -> Bool
checkKeyVal fn [] v o = False
checkKeyVal fn (k:ks) v o = let res = (get k o)
                            in case res of
                            (ListVal ls) -> if length ks == 0 then fn v (ListVal ls)
                                            else or [checkKeyVal fn ks v l | l <- ls]
                            (ObjVal ov) -> checkKeyVal fn ks v res
                            _ -> fn v res

getByKeys :: [Key] -> Val -> Val
getByKeys [] v = v
getByKeys (k:ks) v = getByKeys ks $ get k v
  
get :: Key -> Val -> Val
get k None = None
get k (ObjVal ps) = let res = [v | (Prop k' v) <- ps
                                    , k' == k]
                       in if length res == 0 then None
                          else head res

select :: Key -> [Val] -> [Val]
select k vs =  [ v | (ObjVal vs') <- vs
                   , (Prop k' v) <- vs'
                   , k' == k]


liftIntVal2 :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntVal2 f (IntVal a) (IntVal b) = IntVal $ f a b 



getgroup :: [Val] -> Val
getgroup schs = let res = (group' ["res", "name"] (ListVal schs))
                in ListVal res
                       
test2 = where' (==) ["objs","res","name"] (StringVal "Jacky") $ getgroup schedules
