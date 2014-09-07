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
         , cooldown :: Int
         } deriving (Show, Eq)

persons = [(Person "Jacky" [6] 4 2), (Person "Timmy" [13] 2 2), (Person "Lok" [21] 2 2)]

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
pick ps d = let res = filter (\p -> evalJob p d) ps
            in case res of
                [] -> return Nothing
                _ -> do res' <- filterM evalState res
                        case res' of
                          [] -> return Nothing
                          _ -> return $ (Just $ head res')


evalJob :: Person -> Int -> Bool
evalJob p d = not $ elem d (dates p)

evalState :: Person -> Context Bool
evalState p = do env <- ask
                 let res = filter (\x -> fst x == Just p) env in
                  return $ times p > (length $ res)


                  -- && (cooldown p <= elemIndex  snd $ last res 
                 

