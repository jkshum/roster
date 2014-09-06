{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.Identity

data Person =
  Person { name  :: String
         , dates   :: [Int]
         , times :: Int
         } deriving (Show, Eq)

persons = [(Person "Jacky" [6] 4),(Person "Timmy" [13] 2)]

dutyDates :: [Int]
dutyDates = [6, 7, 13, 14, 20, 21, 27, 28]

type Env  =  [(Maybe Person, Int)]
type Context a = Reader Env a

schedule :: [Person] -> [Int] -> Context Env
schedule _ [] = do env <- ask
                   return env
schedule ps (d:ds) = do env <- ask
                        local (const (env ++ [(pick env ps d, d)]))  (schedule ps ds)

pick :: Env -> [Person] -> Int -> Maybe Person
pick env ps d  
  | res == [] = Nothing
  | otherwise = Just $ head res
  where res = [ p | p <- ps ,
                not $ elem d (dates p),
                exceeded env (Just p)]                
                
exceeded :: Env -> Maybe Person -> Bool
exceeded env p = (times $ fromJust p) > length (filter (\x -> fst x == p) env)




