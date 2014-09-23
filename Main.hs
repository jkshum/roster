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

-- schedule rules [] (ListVal jobs) (ListVal resources)
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
-- evalRule :: [Exp] -> Val -> Val -> Val -> Bool
-- evalRule es schs j r = foldl(\e a -> a && eval (schs, j, r) e) True es


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


getVal :: (Val,Val,Val) ->Exp -> Val
getVal ctx (ExpVal e) = eval ctx e
getVal (schs, j, r) (ContextVal e) 
  | e == Schedules = schs
  | e == Job = j
  | e == Res = schs
getVal ctx v = v
               

eval :: (Val,Val,Val) -> Exp -> Val

eval ctx (Where ks (ExpVal exp1) (ExpVal exp2)) = eval ctx $ Where ks (eval ctx exp1) (eval ctx exp2)
eval ctx (Where ks v (ExpVal exp2)) = eval ctx $ Where ks v (eval ctx exp2)
eval ctx (Where ks (ExpVal exp1) os) = eval ctx $ Where ks (eval ctx exp1) os
eval ctx (Where ks (ContextVal v) (ContextVal os)) = eval ctx $ Where ks (getVal v ctx) (getVal os ctx)
eval ctx (Where ks v (ContextVal os)) = eval ctx $ Where ks v (getVal os ctx)
eval ctx (Where ks (ContextVal v) os) = eval ctx $ Where ks (getVal v ctx) os
eval ctx (Where ks v os) = where' ks v os

eval ctx (Last (ExpVal exp)) = eval ctx $ Last $ eval ctx exp
eval ctx (Last (ListVal os)) = last os

eval ctx (Get k (ExpVal exp)) = eval ctx $ Get k (eval ctx exp)
eval ctx (Get k (ContextVal v)) = getVal v ctx
eval ctx (Get k v) = get k v

eval ctx (Count (ExpVal exp)) = eval ctx $ Count $ eval ctx exp
eval ctx (Count (ContextVal v)) = eval ctx $ Count $ getVal v ctx

eval ctx (Count (ListVal os)) = IntVal $ length os

eval ctx (GreaterEq (ExpVal exp1) (ExpVal exp2)) = eval ctx $ GreaterEq (eval ctx exp1) (eval ctx exp2)
eval ctx (GreaterEq v1 v2) = BoolVal $ v1 >= v2

eval ctx (Diff (ExpVal exp1) (ExpVal exp2)) = eval ctx $ Diff (eval ctx exp1) (eval ctx  exp2)
eval ctx (Diff (IntVal i1) (IntVal i2)) = IntVal $ i1 - i2
       
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

-- group' :: [Key] -> [Val] -> [[Val]]
-- group' ks vs = groupBy (\x y -> 
--                         get ks x == get ks y) vs


-- get :: [Key] -> Val -> Val
-- get (key:ks) (ObjVal es) =
--   let found = find (\(Prop k v) -> key == k) es
--   in if found == Nothing then None
--      else let (Just (Prop k v)) = found
--           in case v of
--               ObjVal obj -> get ks v 
--               otherwise -> v

top :: Int -> [Val] -> [Val]
top n vs = take n vs

index :: (Int, Val) -> Val
index (i,v) = IntVal i

count :: [Val] -> Val
count vs = IntVal $ length vs 


apply :: ([Val]-> [Val]) -> [[Val]] -> [Val]
apply fn vss = concat [fn vs | vs <- vss]

diff :: Val -> Val -> Val
diff (IntVal v1) (IntVal v2) = IntVal $  v1 - v2

-- test1 = select "job" schedules
-- test2 = group' ["date"] test1
-- test3 = apply (top 1) test2
-- test4 = where' ["date"] (get ["date"] $ jacky) test3
-- test5 = last test4
-- test6 = index test5 = diff (count test3) test6 > (get ["cooldown"] $ ObjVal jacky)


-- e1 = Get ["role", "kinds"] jacky
-- e2 = Apply Filter ["role", "type"] (StringVal "Leader") resources

-- evalExp :: Exp -> Val
-- evalExp (Get ks (ObjVal es)) = get ks es

-- evalSelect :: Select -> [Val]
-- evalSelect (Filter ks v vs) = filterByVal ks v vs 

-- filterByVal :: [Key] -> Val -> [Val] -> [Val]
-- filterByVal ks v vs = filter (\(ObjVal xs) -> get ks xs == v) vs

-- evalAggr :: Aggr -> Val
-- evalAggr (Count xs) = IntVal $ length xs

-- evalNest :: Nest -> [[Val]]
-- evalNest (Group ks vs) = groupBy (\(ObjVal xs) (ObjVal ys) -> 
--                                    get ks xs == get ks ys) vs


        
-- evalPre :: Predicate -> Bool
-- evalPre (Eq e1 e2) = and [v1 == v2 | (Prop k1 v1) <- res1
--                                    , (Prop k2 v2) <- res2]
--                      where ( res2) = evalExp e2
--                            ( res1) = evalExp e1

-- evalPre (In e1 e2) = and [elem v1 [v2 | (Prop k2 v2) <- res2]
--                      | (Prop k1 v1) <- res1]
--                      where ( res2) = evalExp e2
--                            ( res1) = evalExp e1

-- evalPre (Eq (Prop k1 v1) (Prop k2 v2)) = v1 == v2
-- resource = [e1, e2]

-- eq "name" e1 "name" e2

-- evalExpr (Predicate o e1 e2) = BoolVal $ eval o (evalExpr e1) (evalExpr e2)
-- evalExpr (Extract k e) = fromJust $ getue k e

-- getue :: String -> Prop -> 
-- getue s ps = let res  = filter (\(k v) -> k == s) ps
                       
                       
                       



-- commonProps = [ Var "date" $ IntVal 6
--               , Var "kind" $ StringVal "Sat"]
              
-- jobs = [ Props $ [Var "role" $ StringVal "Leader"] ++ commonProps
--        , Props $ [Var "role" $ StringVal "Vocal"] ++ commonProps 
--        , Props $ [Var "role" $ StringVal "Vocal"] ++ commonProps 
--        , Props $ [Var "role" $ StringVal "Vocal"] ++ commonProps 
--        , Props $ [Var "role" $ StringVal "Vocal"] ++ commonProps 
--        ]

-- schedules = Schedule {}

-- schs = View [(jobs !! 0, resource !! 0)]


-- r2 = Where Eq schs "Id" 1 (Extract "Id" (resource !! 0))
-- r3 = Query Count r2
-- r4 = Apply GreaterEq (Extract "availability" (resource !! 0)) r3

-- s1 = Where Objectby schs "date" 1 None --[[Row]]
-- s2 = Where Eq s1 "date" 0  (Extract "Id" (resource !! 0))
-- s3 = Foreach Get Top 1 This s1
-- -- s5 = Apply Index s2 s4

-- -- s6 = Apply GreaterEq (Extract "cooldown" (resource !! 0)) Query Count s4

-- getEntity :: Col -> Row -> Entity
-- getEntity c r
--   | c == 0 = fst r
--   | c == 1 = snd r

-- evalExpr :: Expr -> Val

-- evalExpr (Foreach e es) = map (\r -> evalExpr e r) es

-- evalExpr (Get o i (View rows))
--   | o == Top = ExprVal $ View $ take i rows
               
-- evalExpr (Apply o e1 e2)
--   | o == GreaterEq = let IntVal i1 = evalExpr e1
--                          IntVal i2 = evalExpr e2

--                      in BoolVal $ i1 >= i2
--   | o == Index = let ExprVal (View rs1) = evalExpr e1
--                      ExprVal (View rs2) = evalExpr e2
--                      in IntList $
--                         map (\r1->
--                               let indices = elemIndices r1 rs2
--                               in if length indices > 0
--                                     then head indices
--                                     else -1 
--                             ) rs1
                     
-- evalExpr (Query o e)
--   | o == Count = let (ExprVal (View res)) = evalExpr e in
--                      IntVal $ length res

-- evalExpr (Where o (View rows) k1 c e2)
--   | o == Objectby = ExprList $
--                    [View $ l | l <- groupBy
--                    (\x y -> evalExpr (Extract k1 $ getEntity c x) ==
--                             evalExpr (Extract k1 $ getEntity c y)) rows]
                   
--   | otherwise =  ExprVal $ View $ filter
--                  (\r ->
--                    let BoolVal res =
--                          evalExpr $ Predicate o (Extract k1 (getEntity c r)) e2
--                    in res
--                  ) rows

-- evalExpr (Predicate o e1 e2) = BoolVal $ eval o (evalExpr e1) (evalExpr e2)
-- evalExpr (Extract k e) = fromJust $ getue k e

-- getue :: String -> Entity -> Maybe Val
-- getue s (Props ps) = let res  = find (\ (Var k v) -> k == s) ps
--                         in case res of
--                         Nothing -> Nothing
--                         Just (Var k v) ->  Just v

-- eval :: Op -> Val -> Val -> Bool
-- eval In (IntVal x )  (IntList y ) = elem x y;
-- eval Eq (IntVal x )  (IntVal y ) = x == y;
-- eval Eq (StringVal x )  (StringVal y ) = x == y;

-- evalRule :: Job -> Person -> Bool
-- evalRule ::
-- schedule :: [Job] -> [Person] -> [Schedule]
-- schedule j:js ps = 

         -- , blockedDates   :: [Int]
         -- , times :: Int
         -- , cooldown :: Int
         -- , kinds :: [(Role, [Kind])]
         -- , roles :: [Role]
         -- , properties :: [Property (String)]

-- persons = [Person "Jacky" [6] 4 2 [(Leader, [Sat, Sun, Morning]), (Vocal, [Sat])] [Leader, Vocal],
--            Person "Timmy" [13] 2 2 [(Leader, [Sat, Sun, Morning]), (Vocal, [Sat])] [Leader, Vocal],
--            Person "Lok" [21] 2 2 [(Leader, [Sat, Sun, Morning]), (Vocal, [Sat])] [Leader],
--            Person "Chung" [21] 2 2 [(Leader, [Sat, Sun, Morning]), (Vocal, [Sat])] [Leader],
--            Person "Paul" [21] 2 2 [(Vocal, [Sat, Sun])] [Vocal],
--            Person "Emily" [21] 2 2 [(Vocal, [Sat, Sun])] [Vocal],
--            Person "Nami" [21] 2 2 [(Vocal, [Sat, Sun])] [Vocal],
--            Person "Robin" [21] 2 2 [(Vocal, [Sat, Sun])] [Vocal],
--            Person "Boa" [21] 2 2 [(Vocal, [Sun, Morning])] [Vocal],
--            Person "John" [21] 2 2 [(Vocal, [Sun, Morning])] [Vocal],
--            Person "Peter" [21] 2 2 [(Vocal, [Sun, Morning])] [Vocal],
--            Person "Andrew" [21] 2 2 [(Vocal, [Sun, Morning])] [Vocal],
--            Person "Simon" [21] 2 2 [(Vocal, [Sat, Sun, Morning])] [Vocal],
--            Person "Long" [21] 2 2 [(Vocal, [Sat, Sun, Morning])] [Vocal]
--           ]

-- data Team =
--   Team { teamName :: String,
--          teamMembers :: [Person]
--        } deriving (Show, Eq)

-- teams = [Team "A" [persons !! 0, persons !! 1],
--          Team "B" [persons !! 2, persons !! 3],
--          Team "C" [persons !! 4, persons !! 5],
--          Team "D" [persons !! 6, persons !! 7],
--          Team "E" [persons !! 8, persons !! 9],
--          Team "F" [persons !! 10, persons !! 11]
--         ]
                
-- data Job =
--   Job {date :: Int
--       , jobKind :: Kind
--       , jobForm :: [Role]
--       } deriving (Show, Eq)


-- teamForm = [Leader, Vocal, Vocal, Vocal, Vocal]

-- dutyDates :: [Job]
-- dutyDates = [Job 6 Sat teamForm,
--              Job 7 Sun teamForm,
--              Job 7 Morning teamForm,
--              Job 13 Sat teamForm,
--              Job 14 Sun teamForm,
--              Job 14 Morning teamForm,
--              Job 20 Sat teamForm,
--              Job 21 Sun teamForm,
--              Job 21 Morning teamForm,
--              Job 27 Sat teamForm,
--              Job 28 Sun teamForm,
--              Job 28 Morning teamForm]



-- data Schedule =
--   Schedule {scheduledJob :: Job
--            , scheduledPersons :: [(Role, Maybe Person)]
--            } deriving (Show, Eq)
            
            
-- type Env  =  [Schedule]
-- type Plan a = ReaderT Env Identity a

-- schedule :: [Person] -> [Job] -> Plan Env
-- schedule _ [] = do env <- ask
--                    return env
-- schedule ps (j:js) = do env <- ask
--                         res <- match j ps (jobForm j)
--                         local (const $ env ++ [Schedule j res] ) $ schedule ps js
                        
-- match :: Job -> [Person] -> [Role] -> Plan [(Role, Maybe Person)]
-- match _ [] _ = do return []
-- match _ _ [] = do return []
-- match j ps (r:rs) = do res <- assign j ps r
--                        matched <- match j (consume (snd res) ps ) rs

--                        return $ res: matched

-- consume :: Maybe Person -> [Person] -> [Person]
-- consume p ps
--   | ps == [] = []
--   | p == Nothing = ps             
--   | otherwise = delete (fromJust p) ps
  
-- assign :: Job -> [Person] -> Role -> Plan (Role, Maybe Person)
-- assign j ps r = do
--   found <- pick j $ filter (\x -> elem r (roles x)) ps
--   return (r, found) 
     
-- pick :: Job -> [Person] -> Plan (Maybe Person)
-- pick j ps = do res <- filterM (\p -> evalState p j) ps
--                case res of
--                  [] -> return Nothing
--                  _ -> return $ (Just $ head res)

-- evalState :: Person -> Job -> Plan Bool
-- evalState p j = do env <- ask 
--                    let res = filter (\x -> elem (Just p) ([snd sp | sp <- scheduledPersons x])) env in
--                      return $ times p > (length res) &&
--                      (not $ elem (date j) (blockedDates p)) &&
--                      (cooldown p < lastAssigned p env)

-- lastAssigned :: Person -> Env -> Int
-- lastAssigned p env
--   | res == [] = maxBound :: Int
--   | otherwise = length env - last res
--   where res = findIndices (\x -> elem (Just p) ([snd sp | sp <- scheduledPersons x])) env


-- roster = runIdentity $ runReaderT (schedule persons dutyDates) []

-- display :: Schedule -> (Int, Kind, [(Role, String)])
-- display (Schedule {scheduledJob = sj, scheduledPersons = sps}) =
--   (date sj, jobKind sj, [(fst sp, getName $ snd sp) | sp <- sps])

-- getName :: Maybe Person -> String
-- getName p
--   | p == Nothing = ""
--   | otherwise = name $ fromJust p

-- main =
--   putStrLn $ unlines $ map ( show . display) $ runIdentity $ runReaderT (schedule persons dutyDates) []

