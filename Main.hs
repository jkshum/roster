{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Identity

-- data Kind = Sat | Sun | Morning
--           deriving (Eq, Ord, Show, Read, Bounded, Enum)  

-- data Role = Leader | Vocal
--           deriving (Eq, Ord, Show, Read, Bounded, Enum)  

type Key = String
type Col = Int
-- type Row = (Entity, Entity)

-- data Expr = Apply Op Expr Expr
--           | Foreach Expr [Expr]
--           | Select Oyffp Int Expr
--           | Query Op Expr
--           | Where Op Expr Key Col Expr
--           | Predicate Op Expr Expr
--           | Extract Key Entity
--           | View [Row]
--           | Group [Expr]
--           | This
--           | None 
--           deriving (Show, Eq)

data Exp = Select Key Entity
         deriving (Show, Eq)
                  
data Predicate = Eq Exp Exp
               | In Exp Exp
               deriving (Show, Eq)
                        
data Entity = Result [Prop]
            | Sub Exp
            deriving (Show, Eq)
              
data Prop = Prop Key Val
            | Group Key Entity
            deriving (Show, Eq)

data Val = IntVal Int
         | StringVal String
         | BoolVal Bool
         deriving (Show, Eq)

                  
-- data Prop  = VarInt String Int
--            | VarString String String
--            | VarIntList String [Int]
--            | VarStringList String [String]
--            | VarEntity String Entity
--           deriving (Show, Eq)

-- data Val = IntVal Int 
--          | StringVal String
--          | BoolVal Bool
--          deriving (Show, Eq)

-- data Op = In | Eq | Count | GreaterEq | Groupby | Index | Top
--         deriving (Show, Eq)

-- type Row  =  [Schedule]                      

             

-- data Schedule =
--   Schedule { job :: Job
--            , res :: Person
--            } deriving (Show, Eq)

jacky =
  [ Prop "name" $ StringVal "Jacky"
  , Prop "id" $ StringVal "1"
  , Prop "date" $ IntVal 6
  , Prop "date" $ IntVal 13
  , Prop "availability" $ IntVal 1
  , Group "role" $ Result [ Prop "type" $ StringVal "Leader"
                 , Prop "kind" $ StringVal "Sun"
                 , Prop "kind" $ StringVal "Morning"]
  , Group "role" $ Result [ Prop "type" $ StringVal "Vocal"
                 , Prop "kind" $ StringVal "Sun"
                 , Prop "kind" $ StringVal "Morning"]
  ]

job =
  [ Prop "date" $ IntVal 6
  , Prop "kind" $ StringVal "Moring"
  ]

schedule =
  [ Group "job" $ Result job
  , Group "resource" $  Result jacky
  ]
  
-- e1 = Eq (Select "name" jacky) (Select "name" jacky)
-- e2 = In (Select "date" job) (Select "date" jacky)
e3 = In (Select "kind" $ Result job) (Select "kind" $ Sub (Select "role" $ Result jacky))

match :: Key -> Prop -> Bool
match key (Prop k v) = key == k
match key (Group k e) = key == k

evalExp :: Exp -> Entity
evalExp (Select key (Result props)) = Result $ filter (\p-> match key p) props

evalPre :: Predicate -> Bool
-- evalPre (Eq e1 e2) = evalPre' (==) e1 e2
evalPre (In e1 e2) = and [elem v1 [v2 | (Prop k2 v2) <- res2]
                     | (Prop k1 v1) <- res1]
                     where (Result res2) = evalExp e2
                           (Result res1) = evalExp e1

-- evalPre' :: (Val -> Val -> Bool) -> Exp -> Exp -> Bool
-- evalPre' fn e1 e2 = and [fn v1 v2 | (Prop k1 v1) <- evalExp e1
--                                   , (Prop k2 v2) <- evalExp e2]

-- evalPre (Eq (Prop k1 v1) (Prop k2 v2)) = v1 == v2
-- resource = [e1, e2]

-- eq "name" e1 "name" e2

-- evalExpr (Predicate o e1 e2) = BoolVal $ eval o (evalExpr e1) (evalExpr e2)
-- evalExpr (Extract k e) = fromJust $ getValue k e

-- getValue :: String -> Prop -> 
-- getValue s ps = let res  = filter (\(k v) -> k == s) ps
                       
                       
                       



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

-- s1 = Where Groupby schs "date" 1 None --[[Row]]
-- s2 = Where Eq s1 "date" 0  (Extract "Id" (resource !! 0))
-- s3 = Foreach Select Top 1 This s1
-- -- s5 = Apply Index s2 s4

-- -- s6 = Apply GreaterEq (Extract "cooldown" (resource !! 0)) Query Count s4

-- getEntity :: Col -> Row -> Entity
-- getEntity c r
--   | c == 0 = fst r
--   | c == 1 = snd r

-- evalExpr :: Expr -> Val

-- evalExpr (Foreach e es) = map (\r -> evalExpr e r) es

-- evalExpr (Select o i (View rows))
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
--   | o == Groupby = ExprList $
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
-- evalExpr (Extract k e) = fromJust $ getValue k e

-- getValue :: String -> Entity -> Maybe Val
-- getValue s (Props ps) = let res  = find (\ (Var k v) -> k == s) ps
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

