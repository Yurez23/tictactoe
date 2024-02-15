{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE GeneralisedNewtypeDeriving #-}
-- {-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tictactoe
  ( Board,
    Player,
    Cell,
    flatBoardToBoard,
    boardToParsableText,
    boardHtml,
    showBoardConsole,
    emptyBoard,
    firstPlayer,
    nextPlayer,
    isWinner,
    isFull,
    makeMove
  )
where

import Data.List (intercalate, transpose)
import Data.Maybe (isJust)
import Control.Applicative (asum)
import qualified Data.List.Split as LS
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5 as H
import qualified Web.Scotty as S

data Player = X | O
  deriving (Show, Read, Eq)

instance S.Parsable Player where
  parseParam :: TL.Text -> Either TL.Text Player
  parseParam "X" = Right X
  parseParam "O" = Right O
  parseParam _ = Left "Parse error on Player"

instance (S.Parsable a) => S.Parsable (Maybe a) where
  parseParam :: (S.Parsable a) => TL.Text -> Either TL.Text (Maybe a)
  parseParam "Nothing" = Right Nothing
  parseParam t
    | TL.take 5 t == "Just " = case S.parseParam (TL.drop 5 t) of
        Right x -> Right $ Just x
        Left err -> Left err
    | otherwise = Left "Parse error on Maybe type"

-- Изначальная идея была парсить вложенные списки
-- Не сработало, потому что во внутренних списках тоже есть запятые
-- newtype ParsableList a = ParsableList {getParsableList :: [a]} deriving (Show, Functor, Foldable, Traversable)

-- instance S.Parsable a => S.Parsable (ParsableList a) where
--   parseParam :: S.Parsable a => TL.Text -> Either TL.Text (ParsableList a)
--   parseParam t
--     | TL.head t == '[' && TL.last t == ']' = let t' = TL.init (TL.tail t) in
--       mapM S.parseParam (ParsableList (TL.splitOn "," t'))
--     | otherwise = Left "No proper brackets"

-- nestedListToParsable :: [[a]] -> ParsableList (ParsableList a)
-- nestedListToParsable = ParsableList . map ParsableList

-- parsableToNestedList :: ParsableList (ParsableList a) -> [[a]]
-- parsableToNestedList = map getParsableList . getParsableList

type Cell = Maybe Player

type Board = [[Cell]]

boardToParsableText :: Board -> TL.Text
boardToParsableText = TL.filter (\c -> c /= '[' && c /= ']') . TL.pack . show

flatBoardToBoard :: [Cell] -> Board
flatBoardToBoard = LS.chunksOf 3

cellHtml :: Cell -> H.Html
cellHtml Nothing = "   "
cellHtml (Just X) = " X "
cellHtml (Just O) = " O "

boardHtml :: Board -> [[H.Html]]
boardHtml = map (map cellHtml)

showBoardConsole :: Board -> String
showBoardConsole = intercalate "\n-----------\n" . map formatRow
  where
    formatRow :: [Cell] -> String
    formatRow = intercalate "|" . map formatCell
    formatCell :: Maybe Player -> String
    formatCell Nothing = "   "
    formatCell (Just X) = " X "
    formatCell (Just O) = " O "

emptyBoard :: Board
emptyBoard = replicate 3 $ replicate 3 Nothing

firstPlayer :: Player
firstPlayer = X

isWinner :: Board -> Maybe Player
isWinner b = asum $ checkRows b ++ checkColumns b ++ checkDiagonals b
  where
    checkRows = map checkLine
    checkColumns = map checkLine . transpose
    checkDiagonals b' = [checkMainDiagonal b', checkSideDiagonal b']

checkLine :: [Cell] -> Maybe Player
checkLine [Just O, Just O, Just O] = Just O
checkLine [Just X, Just X, Just X] = Just X
checkLine _ = Nothing

checkMainDiagonal :: Board -> Maybe Player
checkMainDiagonal b = checkLine [b !! i !! i | i <- [0 .. 2]]

checkSideDiagonal :: Board -> Maybe Player
checkSideDiagonal b = checkLine [b !! i !! (2 - i) | i <- [0 .. 2]]

isFull :: Board -> Bool
isFull = (all . all) isJust

makeMove :: Board -> (Int, Int) -> Player -> Board
makeMove b (x, y) p =
  let (before, toUpdate : after) = splitAt y b -- Если координаты и доска нормальные, то проблем с неисчерпывающим паттерном не будет
   in before ++ [updateList x toUpdate p] ++ after
  where
    updateList :: Int -> [Cell] -> Player -> [Cell]
    updateList x' l p' =
      let (before, _ : after) = splitAt x' l -- Если координаты и доска нормальные, то проблем с неисчерпывающим паттерном не будет
       in before ++ [Just p'] ++ after

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X