{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Identity
import Control.Lens

data Kind = Sat | Sun | Morning
          deriving (Eq, Ord, Show, Read, Bounded, Enum)  

data Person =
  Person { name  :: String
         , dates   :: [Int]
         , times :: Int
         , cooldown :: Int
         , kind :: [Kind]
         } deriving (Show, Eq)

persons = [Person "Jacky" [6] 4 2 [Sat, Sun, Morning],
           Person "Timmy" [13] 2 2 [Sat, Morning],
           Person "Lok" [21] 2 2 [Morning]]

data Job =
  Job {date :: Int
      , jobKind :: Kind
      } deriving (Show, Eq)

dutyDates :: [Job]
dutyDates = [Job 6 Sat,
             Job 7 Sun,
             Job 7 Morning,
             Job 13 Sat,
             Job 14 Sun,
             Job 14 Morning,
             Job 20 Sat,
             Job 21 Sun,
             Job 21 Morning,
             Job 27 Sat,
             Job 28 Sun,
             Job 28 Morning]
            
type Env  =  [(Maybe Person, Job)]
type Plan a = ReaderT Env Identity a

schedule :: [Person] -> [Job] -> Plan Env
schedule _ [] = do env <- ask
                   return env
schedule ps (j:js) = do env <- ask
                        found <- pick ps j
                        local (const $ env ++ [(found, j)]) $ schedule ps js

pick :: [Person] -> Job -> Plan (Maybe Person)
pick ps j = do res <- filterM (\p -> evalState p j) ps
               case res of
                 [] -> return Nothing
                 _ -> return $ (Just $ head res)

evalState :: Person -> Job -> Plan Bool
evalState p j = do env <- ask 
                   let res = filter (\x -> fst x == Just p) env in
                     return $ times p > (length $ res) &&
                     (not $ elem (date j) (dates p)) &&
                     elem (jobKind j) (kind p) &&
                     (cooldown p < lastAssigned p env)

lastAssigned :: Person -> Env -> Int
lastAssigned p env
  | res <= [] = maxBound :: Int
  | otherwise = length env - last res
  where res = findIndices (\x -> fst x == Just p) env
