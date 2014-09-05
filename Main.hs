{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Map as Map
import Data.List
import Data.Maybe

data Person =
  Person { name  :: String
         , dates   :: [Int]
         , times :: Int
         } deriving (Show, Eq)

persons = [(Person "Jacky" [6] 4),(Person "Timmy" [13] 2)]

dutyDates :: [Int]
dutyDates = [6, 7, 13, 14, 20, 21, 27, 28]

-- splitedDates = Split.splitEvery (List.length persons)

type Env  =  [(Maybe Person, Int)] 


schedule :: Env -> [Person] -> [Int] -> Env
schedule env _ [] = env
schedule env ps (d:ds) = schedule (env ++ [(pick env ps d, d)]) ps ds

-- countOfElem :: (Eq a) => a -> [a] -> Int
-- countOfElem elem list = length $ filter (\x -> x == elem) list

pick :: Env -> [Person] -> Int -> Maybe Person
pick env ps d  
  | res == [] = Nothing
  | otherwise = Just $ head res
  where res = [ p | p <- ps ,
                not $ elem d (dates p),
                exceeded env (Just p)]                
                
exceeded :: Env -> Maybe Person -> Bool
exceeded env p = (times $ fromJust p) > length (filter (\x -> fst x == p) env)

                
