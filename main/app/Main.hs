import Board (Side (..))
import Engine (runGame, runBot)
import Network.Simple.TCP (withSocketsDo)

readColor :: IO Side
readColor = do
  putStrLn "Select color: \"w\" for white \"b\" for black."
  line <- getLine
  case parseColor line of
    (Just c) -> return c
    Nothing -> do
      putStrLn "Unexpected color, try again"
      readColor
  where
    parseColor :: String -> Maybe Side
    parseColor "w"     = Just White
    parseColor "white" = Just White
    parseColor "b"     = Just Black
    parseColor "black" = Just Black
    parseColor _       = Nothing


main :: IO ()
main = withSocketsDo $ do
  side <- readColor
  line <- getLine
  case line of
    "b" -> runBot side
    _   -> runGame side

--TODO: style, warnings, tests, TH?, imports
--TH: trivial-read, intance Serializable
--add board to broadcastBoard
