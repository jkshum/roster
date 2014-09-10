{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Identity

data Kind = Sat | Sun | Morning
          deriving (Eq, Ord, Show, Read, Bounded, Enum)  

data Role = Leader | Vocal
          deriving (Eq, Ord, Show, Read, Bounded, Enum)  

data Person =
  Person { name  :: String
         , blockedDates   :: [Int]
         , times :: Int
         , cooldown :: Int
         , kinds :: [(Role, [Kind])]
         , roles :: [Role]
         } deriving (Show, Eq)

persons = [Person "Jacky" [6] 4 2 [(Leader, [Sat, Sun, Morning]), (Vocal, [Sat])] [Leader, Vocal],
           Person "Timmy" [13] 2 2 [(Leader, [Sat, Sun, Morning]), (Vocal, [Sat])] [Leader, Vocal],
           Person "Lok" [21] 2 2 [(Leader, [Sat, Sun, Morning]), (Vocal, [Sat])] [Leader],
           Person "Chung" [21] 2 2 [(Leader, [Sat, Sun, Morning]), (Vocal, [Sat])] [Leader],
           Person "Paul" [21] 2 2 [(Vocal, [Sat, Sun])] [Vocal],
           Person "Emily" [21] 2 2 [(Vocal, [Sat, Sun])] [Vocal],
           Person "Nami" [21] 2 2 [(Vocal, [Sat, Sun])] [Vocal],
           Person "Robin" [21] 2 2 [(Vocal, [Sat, Sun])] [Vocal],
           Person "Boa" [21] 2 2 [(Vocal, [Sun, Morning])] [Vocal],
           Person "John" [21] 2 2 [(Vocal, [Sun, Morning])] [Vocal],
           Person "Peter" [21] 2 2 [(Vocal, [Sun, Morning])] [Vocal],
           Person "Andrew" [21] 2 2 [(Vocal, [Sun, Morning])] [Vocal],
           Person "Simon" [21] 2 2 [(Vocal, [Sat, Sun, Morning])] [Vocal],
           Person "Long" [21] 2 2 [(Vocal, [Sat, Sun, Morning])] [Vocal]
          ]

data Team =
  Team { teamName :: String,
         teamMembers :: [Person]
       } deriving (Show, Eq)

teams = [Team "A" [persons !! 0, persons !! 1],
         Team "B" [persons !! 2, persons !! 3],
         Team "C" [persons !! 4, persons !! 5],
         Team "D" [persons !! 6, persons !! 7],
         Team "E" [persons !! 8, persons !! 9],
         Team "F" [persons !! 10, persons !! 11]
        ]
                
data Job =
  Job {date :: Int
      , jobKind :: Kind
      , jobForm :: [Role]
      } deriving (Show, Eq)


teamForm = [Leader, Vocal, Vocal, Vocal, Vocal]

dutyDates :: [Job]
dutyDates = [Job 6 Sat teamForm,
             Job 7 Sun teamForm,
             Job 7 Morning teamForm,
             Job 13 Sat teamForm,
             Job 14 Sun teamForm,
             Job 14 Morning teamForm,
             Job 20 Sat teamForm,
             Job 21 Sun teamForm,
             Job 21 Morning teamForm,
             Job 27 Sat teamForm,
             Job 28 Sun teamForm,
             Job 28 Morning teamForm]



data Schedule =
  Schedule {scheduledJob :: Job
           , scheduledPersons :: [(Role, Maybe Person)]
           } deriving (Show, Eq)
            
            
type Env  =  [Schedule]
type Plan a = ReaderT Env Identity a

schedule :: [Person] -> [Job] -> Plan Env
schedule _ [] = do env <- ask
                   return env
schedule ps (j:js) = do env <- ask
                        res <- (mapM (assign j ps) (jobForm j))
                        local (const $ env ++ [Schedule j res] ) $ schedule ps js
                        
assign :: Job -> [Person] -> Role -> Plan (Role, Maybe Person)
assign j ps r = do
  found <- pick j $ filter (\x -> elem r (roles x)) ps
  return (r, found) 
     
pick :: Job -> [Person] -> Plan (Maybe Person)
pick j ps = do res <- filterM (\p -> evalState p j) ps
               case res of
                 [] -> return Nothing
                 _ -> return $ (Just $ head res)

evalState :: Person -> Job -> Plan Bool
evalState p j = do env <- ask 
                   let res = filter (\x -> elem (Just p) ([snd sp | sp <- scheduledPersons x])) env in
                     return $ times p > (length res) &&
                     (not $ elem (date j) (blockedDates p)) &&
                     (cooldown p < lastAssigned p env)

lastAssigned :: Person -> Env -> Int
lastAssigned p env
  | res <= [] = maxBound :: Int
  | otherwise = length env - last res
  where res = findIndices (\x -> elem (Just p) ([snd sp | sp <- scheduledPersons x])) env


main =
  putStrLn $ show $ runIdentity $ runReaderT (schedule persons dutyDates) []


roster = runIdentity $ runReaderT (schedule persons dutyDates) []
