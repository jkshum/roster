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

persons = [(Person "Jacky" [6] 4 2 [Sat, Sun, Morning]),
           (Person "Timmy" [13] 2 2 [Sat, Morning]),
           (Person "Lok" [21] 2 2) [Morning]]

dutyDates :: [Int]
dutyDates = [6, 7, 13, 14, 20, 21, 27, 28]

type Env  =  [(Maybe Person, Int)]
type Plan a = ReaderT Env Identity a

schedule :: [Person] -> [Int] -> Plan Env
schedule _ [] = do env <- ask
                   return env
schedule ps (d:ds) = do env <- ask
                        found <- pick ps d
                        local (const $ env ++ [(found, d)]) $ schedule ps ds

pick :: [Person] -> Int -> Plan (Maybe Person)
pick ps d = do res <- filterM (\p -> evalState p d) ps
               case res of
                 [] -> return Nothing
                 _ -> return $ (Just $ head res)


-- evalJob :: Person -> Int -> Bool
-- evalJob p d = not $ elem d (dates p)

evalState :: Person -> Int -> Plan Bool
evalState p d = do env <- ask 
                   let res = filter (\x -> fst x == Just p) env in
                     return $ times p > (length $ res) &&
                     (not $ elem d (dates p)) &&
                     (cooldown p < lastAssigned p env)

lastAssigned :: Person -> Env -> Int
lastAssigned p env
  | res <= [] = maxBound :: Int
  | otherwise = length env - last res
  where res = findIndices (\x -> fst x == Just p) env
