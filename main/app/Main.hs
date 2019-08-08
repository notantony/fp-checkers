import Graphics.Gloss
import Graphics.Gloss.Data.Controller
import Graphics.Gloss.Interface.IO.Game
import System.Environment
import Control.Monad.Writer(execWriter, tell)
import MyGraphics
  ( makeTextLarge
  , makeTextNormal
  , toBold
  )
import Board
  ( Board(..)
  )

newtype Scene = Scene [Actor]
  deriving ()

data Handler = Handler 
  { hPrior  :: Int 
  , hAction :: Event -> Scene -> Maybe (IO Scene)
  }
  deriving ()

data Actor = Actor
  { aId      :: String
  , aHandler :: Maybe Handler
  , aPicture :: Picture
  }
  deriving ()


drawScene :: Scene -> IO Picture
drawScene (Scene actors) = do
  return $ Pictures $ map aPicture actors

handleEvent :: Event -> Scene -> IO Scene
handleEvent event x = do
  return x

handleTick :: Float -> Scene -> IO Scene
handleTick _ x = do
  return x

-- drawScene :: Scene -> Picture 
-- drawScene MainMenu = 
  -- Pictures $ execWriter $ do
  -- tell [toBold $ makeTextLarge (-100, 200) "Main menu"]
  -- tell [makeTextNormal (0, 0) "Play"]
  -- tell [t]
  -- tell [makeTextNormal (1, 0) "Hello"]
  -- tell [makeTextNormal (100, 100) "Hello"]

decor :: Picture -> Actor
decor pic = Actor{ aId = "", aHandler = Nothing, aPicture = pic}
  
mainMenuScene :: Scene
mainMenuScene = Scene
  [ decor $ toBold $ makeTextLarge (-150, 200) "Main menu"
  , decor $ makeTextNormal (0, 0) "Play"
  ]


makeButton :: Handler -> Point -> String -> Actor
makeButton (x, y) s = 

flag = mainMenuScene



-- Длина кнопки зависит от количества символов
main :: IO ()
main = do
  playIO (InWindow "Checkers" (800, 600) (200, 200)) black 1 flag drawScene handleEvent handleTick


-- import Network
-- import Control.Concurrent
-- import System.IO

-- main :: IO ()
-- main = withSocketsDo $ do
--   sock <- listenOn $ PortNumber 5002
--   loop sock

-- loop sock = do
--    (h,_,_) <- accept sock
--    forkIO $ body h
--    loop sock
--   where
--    body h = do
--        hPutStr h msg
--        hFlush h
--        hClose h

-- msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"
