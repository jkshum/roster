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
type Context a = ReaderT Env Identity a

schedule :: [Person] -> [Int] -> Context Env
schedule _ [] = do env <- ask
                   return env
schedule ps (d:ds) = do env <- ask
                        found <- pick ps d
                        local (const $ env ++ [(found, d)]) $ schedule ps ds

pick :: [Person] -> Int -> Context (Maybe Person)
pick ps d = do env <-ask
               let res = [p | p <- ps,
                          not $ elem d (dates p),
                          times p > $ length $ filter (\x -> fst x == Just p) env]
                 in case res of
                 [] -> return Nothing
                 _ -> return $ (Just $ head res)


