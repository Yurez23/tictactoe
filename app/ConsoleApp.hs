module ConsoleApp (gameConsole) where

import Tictactoe
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT) )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad (unless, guard)
import Data.Maybe (isNothing)
import Control.Applicative (empty)

gameConsole :: IO ()
gameConsole = game' emptyBoard firstPlayer
  where
    game' :: Board -> Player -> IO ()
    game' board player = do
      putStrLn $ showBoardConsole board
      case isWinner board of
        Just player' -> putStrLn $ "Победитель - " ++ show player'
        Nothing -> do
          if isFull board
            then putStrLn "Ничья"
            else do
              (x, y) <- getCoords board
              let board' = makeMove board (x, y) player
              game' board' (nextPlayer player)

getCoords :: Board -> IO (Int, Int)
getCoords b = maybe (getCoords b) return =<< runMaybeT (getValidCoordsMaybe b)

getValidCoordsMaybe :: Board -> MaybeT IO (Int, Int)
getValidCoordsMaybe b = do
  [rx, ry] <- map reads . words <$> liftIO getLine
  case (rx, ry) of
    ([(x, "")], [(y, "")]) -> do
      unless (isCoordsValid (x, y)) $ liftIO $ putStrLn "Недопустимые координаты"
      guard $ isCoordsValid (x, y)
      unless (isPlaceFree b (x, y)) $ liftIO $ putStrLn "Эта позиция занята"
      guard $ isPlaceFree b (x, y)
      return (x, y)
    _ -> do
      liftIO $ putStrLn "Некорректный ввод координат"
      empty


isCoordsValid :: (Int, Int) -> Bool
isCoordsValid (x, y) = all (\c -> c >= 0 && c <= 2) [x, y]

isPlaceFree :: Board -> (Int, Int) -> Bool
isPlaceFree b (x, y) = isNothing $ b !! y !! x