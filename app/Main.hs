import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (isNothing)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Web.Scotty as S
import Tictactoe

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    board <- flatBoardToBoard <$> param "board" `rescue` (\_ -> return $ concat emptyBoard)
    player <- param "player" `rescue` (\_ -> return firstPlayer)
    liftIO $ putStrLn ("Received in get board:  " ++ show board)
    liftIO $ putStrLn ("Received in get player:  " ++ show player)
    let winner = isWinner board
    liftIO $ putStrLn ("Received in get winner:  " ++ show winner)
    S.html $ renderHtml $ gamePage board player winner

  post "/make_move" $ do
    board <- flatBoardToBoard <$> param "board" `rescue` (\_ -> return $ concat emptyBoard)
    player <- param "player" `rescue` (\_ -> return firstPlayer)
    [x, y] <- map read . words <$> param "cell"
    liftIO $ putStrLn ("Received in post board:  " ++ show board)
    liftIO $ putStrLn ("Received in post player:  " ++ show player)
    liftIO $ putStrLn ("Received in post cell coordinates:  " ++ show (x, y))
    let board' = makeMove board (x, y) player
        player' = nextPlayer player
        path = TL.concat ["/?board=", boardToParsableText board', "&player=", TL.pack (show player')]
    redirect path


gamePage :: Board -> Player -> Maybe Player -> H.Html
gamePage board player winner = H.docTypeHtml $ do
  H.head $ do
    H.title "Крестики-нолики"
    H.style "button.cell-button { min-height: 30px; min-width: 30px; }"
    H.style ".spacer { margin-bottom: 50px; }"
    H.style ".h2 { height: 20px; }"
  H.body $ do
    H.h1 "Крестики-нолики"
    H.form ! A.method "post" ! A.action "/make_move" $ do
      H.input ! A.type_ "hidden" ! A.name "board" ! A.value (H.lazyTextValue $ boardToParsableText board)
      H.input ! A.type_ "hidden" ! A.name "player" ! A.value (H.stringValue $ show player)
      H.table $ do
        H.tr $ do
          H.td $ cellH 0 0
          H.td $ cellH 1 0
          H.td $ cellH 2 0
        H.tr $ do
          H.td $ cellH 0 1
          H.td $ cellH 1 1
          H.td $ cellH 2 1
        H.tr $ do
          H.td $ cellH 0 2
          H.td $ cellH 1 2
          H.td $ cellH 2 2
    case winner of
      Just w -> H.h2 $ "Победил " <> H.toHtml (show w) ! A.class_ "h2"
      Nothing ->
        if isFull board
          then H.h2 "Ничья" ! A.class_ "h2"
          else H.h2 "" ! A.class_ "h2"
    H.div ! A.class_ "spacer" $ ""
    H.form ! A.method "get" ! A.action "/" $ do
      H.button "Заново" ! A.type_ "submit" ! A.value "" ! A.name "again"
  where
    boardH = boardHtml board
    cellH' x y = H.button (boardH !! y !! x) ! A.type_ "submit" ! A.value (H.stringValue $ show x ++ " " ++ show y) ! A.name "cell" ! A.class_ "cell-button"
    cellH x y
      | isNothing (board !! y !! x) && isNothing winner = cellH' x y
      | otherwise = cellH' x y ! A.disabled ""
