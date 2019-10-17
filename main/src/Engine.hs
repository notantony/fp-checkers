module Engine
  ( runGame
  , runBot
  ) where

import Board (Board (..), Coord (..), Field (..), Move (..), Piece (..), Side (..), chainMoves,
              checkDir, chooseMove, dumpBoard, getFieldUnsafe, getPiece, getPossibleMoves,
              getStarts, inBoardRange, makeMove, startingPosition, tryMove)
import Control.Concurrent (MVar, forkIO, killThread, putMVar, readMVar, takeMVar, tryPutMVar,
                           tryTakeMVar)
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Graphics.Gloss
import Graphics.Gloss.Data.Controller
import Graphics.Gloss.Data.Point (Point (..), pointInBox)
import qualified Graphics.Gloss.Data.Point.Arithmetic as PA (negate, (*), (+), (-))
import Graphics.Gloss.Interface.IO.Game
import MyGraphics (makePiece, makeTextLarge, makeTextNormal, toBold)
import Network.Simple.TCP
import Resources (boardTex, dotTex, ghostTex)
import Server (Message (..), dummyMsg, makeMsg, runClient)
import System.Environment
import System.Exit (exitSuccess)
import Util (Serializable (..), fromIntegralPair, headMaybe, readNetworkCfg, runPipe)

newtype Scene = Scene { unScene :: [Actor] }

data Handler = Handler
  { hPrior  :: Int
  , hAction :: Event -> Scene -> IO Scene
  }

data Actor = Actor
  { aId      :: String
  , aPicture :: Picture
  , aPrior   :: Int
  }

drawScene :: Scene -> IO Picture
drawScene (Scene actors) = do
  let (x, y) = PA.negate windowCenter
  return $ Translate x y $ Pictures $ map aPicture actors

handleEvent :: [Handler] -> Event -> Scene -> IO Scene
handleEvent handlers event scene@(Scene actors) = do
  let
    mx :: Int
    mx = maximum $ map hPrior handlers
    actions :: [Event -> Scene -> IO Scene]
    actions = map hAction $ filter (\handler -> hPrior handler == mx) handlers
    pipe :: [Scene -> IO Scene]
    pipe = map (\op -> op event) actions
  (Scene actors) <- runPipe pipe scene
  return $ setupScene actors

decorate :: String -> Picture -> Actor
decorate label pic = Actor{ aId = label, aPicture = pic, aPrior = 0 }

mainMenuScene :: Scene
mainMenuScene = Scene
  [ decorate "menu_txt" $ toBold $ makeTextLarge (-150, 200) "Main menu"
  , decorate "play_btn" $ makeTextNormal (0, 0) "Play"
  ]

makeGameOver :: String -> Scene
makeGameOver string = Scene
  [ decorate "gameover_txt" $ toBold $ makeTextNormal (windowCenter PA.+ (-100, 0)) "Game over!"
  , decorate "result_txt" $ makeTextNormal (windowCenter PA.+ (-100, -100)) string
  ]

loadingScreenScene :: Scene
loadingScreenScene = Scene
  [ decorate "loading_txt" $ makeTextNormal (400, 400) "Loading..."
  ]

windowSize :: Num a => (a, a)
windowSize = (860, 860)

windowCenter :: Point
windowCenter = 0.5 PA.* windowSize

boardBegin :: Num a => (a, a)
boardBegin = (60, 60)

fieldSide :: Num a => a
fieldSide = 100

makeFieldAction :: (Coord -> Scene -> IO Scene) -> Event -> Scene -> IO Scene
makeFieldAction action (EventKey (MouseButton LeftButton) Down _ point) = do
  let
    normalized :: Point
    normalized = point PA.+ windowCenter
    mCoord :: Maybe Coord
    mCoord = pointToCoord normalized
  case mCoord of
    Nothing    -> return
    Just coord -> action coord
makeFieldAction _ _ = return

coordToPoint :: Coord -> Point
coordToPoint (Coord p) = fieldSide PA.* (fromIntegralPair p) PA.+ boardBegin

pointToCoord :: Point -> Maybe Coord
pointToCoord p =
  if inBoardRange (a, b)
  then Just $ Coord (a, b)
  else Nothing
  where
    coord :: (Float, Float)
    coord = (1 / fieldSide) PA.* (p PA.- boardBegin)
    a :: Int
    a = floor $ fst coord
    b :: Int
    b = floor $ snd coord

setupPiece :: Piece -> Coord -> Actor
setupPiece piece = setupAtCoord (makePiece piece) 0 "piece"

setupAtCoord :: Picture -> Int -> String -> Coord -> Actor
setupAtCoord tex prior label coord = Actor{ aId = label, aPicture = placed, aPrior = prior }
  where
    placed :: Picture
    placed = uncurry Translate (coordToPoint coord) tex

setupScene :: [Actor] -> Scene
setupScene actors = Scene $ sortBy (\a b -> compare (aPrior a) (aPrior b)) actors

makeBoard :: Board -> Scene
makeBoard board =
  let
    boardActor :: Actor
    boardActor = Actor{ aId = "board", aPicture = boardTex, aPrior = -1 }
    dumped :: [(Coord, Piece)]
    dumped = dumpBoard board
    pieces :: [Actor]
    pieces = map (uncurry . flip $ setupPiece) dumped
  in
    setupScene $ boardActor : pieces

register :: Side -> MVar Message -> MVar Message -> IO (Maybe String)
register side recvBuf sendBuf = do
  putMVar sendBuf $ makeMsg "side" side
  ans <- takeMVar recvBuf
  if mBody ans == "ok"
  then do
    putStrLn "Waiting for other players"
    putMVar sendBuf dummyMsg
    _ <- takeMVar recvBuf
    return Nothing
  else return $ Just $ "Register failed: \"" ++ mBody ans ++ "\""

runGame :: Side -> IO ()
runGame side = do
  (host, port) <- readNetworkCfg "client.cfg"
  (clientThreadId, sendBuf, recvBuf) <- runClient host port

  startPtr <- newIORef Nothing
  bodyPtr <- newIORef (Man side)
  queuePtr <- newIORef []
  boardPtr <- newIORef startingPosition
  sidePtr <- newIORef White

  let
    clearQueue :: IO ()
    clearQueue = do
      writeIORef startPtr Nothing
      writeIORef queuePtr []

    pushQueueAction :: Event -> Scene -> IO Scene
    pushQueueAction = makeFieldAction $ \coord scene@(Scene actors) -> do
      let
        setupPremove :: (Board, Coord) -> IO Scene
        setupPremove (board, premoveCoord) = do
          q <- readIORef queuePtr
          return $ nextScene (getNextCoords $ headMaybe q)
          where
            getNextCoords :: Maybe Move -> [Coord]
            getNextCoords lastMoveM = map (\(_, b, _) -> b) (getPossibleMoves lastMoveM board premoveCoord)
            nextScene :: [Coord] -> Scene
            nextScene nextCoords = Scene $ ghost : nextDots ++ filtered
              where
                ghost :: Actor
                ghost = setupAtCoord ghostTex 1 "ghost" premoveCoord
                nextDots :: [Actor]
                nextDots = map (setupAtCoord dotTex 2 "ndot") nextCoords
                filtered :: [Actor]
                filtered = filter ((/=) "ndot" . aId) actors
      mStart <- readIORef startPtr
      case mStart of
        Nothing -> do
          curSide <- readIORef sidePtr
          if side /= curSide
          then return scene
          else do
            board <- readIORef boardPtr
            case getPiece side coord board of
              Nothing -> return scene
              (Just body) ->
                if coord `elem` getStarts side board
                then do
                  writeIORef startPtr $ Just coord
                  writeIORef bodyPtr body
                  setupPremove (board, coord)
                else return scene
        (Just start) -> do
          body <- readIORef bodyPtr
          board <- readIORef boardPtr
          q <- readIORef queuePtr
          let
            tryResult :: Maybe (Move, (Board, Coord))
            tryResult = do
              (_, lastCoord) <- chainMoves body q start board
              move <- makeMove coord lastCoord
              if checkDir move body
              then do
                result <- chainMoves body (move: q) start board
                return (move, result)
              else Nothing
          case tryResult of
            Nothing -> return scene
            Just (move, newPremove) -> do
              modifyIORef' queuePtr (\moves -> move : moves)
              setupPremove newPremove

    releaseQueueAction :: Event -> Scene -> IO Scene
    releaseQueueAction (EventKey (Char 'r') Down _ _) scene = do
      curSide <- readIORef sidePtr
      when (curSide == side) $ do
        mCoord <- readIORef startPtr
        case mCoord of
          Nothing -> return ()
          (Just coord) -> do
            q <- readIORef queuePtr
            putMVar sendBuf $ makeMsg "move" (q, coord)
            clearQueue
      return scene
    releaseQueueAction _ scene = return scene

    exitAction :: Event -> Scene -> IO Scene
    exitAction (EventKey (Char 'q') Down _ _) _ = do
      putStrLn "Bye!"
      exitSuccess
    exitAction _ scene = return scene

    handleTick :: Float -> Scene -> IO Scene
    handleTick _ scene = do
      recieved <- tryTakeMVar recvBuf
      case recieved of
        Nothing -> return scene
        Just msg@Message{ mHead = hd, mBody = body } ->
          case hd of
            "finish" -> do
              killThread clientThreadId
              return $ makeGameOver body
            "board" -> do
              let
                (newBoard, newSide) = deserialize body :: (Board, Side)
              clearQueue
              writeIORef boardPtr newBoard
              when (newSide /= side) $ putMVar sendBuf dummyMsg
              writeIORef sidePtr newSide
              return $ makeBoard newBoard
            "str" -> do
              putStrLn body
              return scene
            _ -> do
              putStrLn $ "Unexpected message: " ++ show msg
              return scene


  status <- register side recvBuf sendBuf
  case status of
    (Just errorMsg) -> putStrLn errorMsg
    Nothing -> do
      when (side == Black) $ putMVar sendBuf dummyMsg
      playIO
        (InWindow "Checkers" windowSize (0, 0))
        black
        10
        -- loadingScreenScene
        (makeBoard startingPosition)
        drawScene
        ( handleEvent
          [ Handler{ hAction = pushQueueAction, hPrior = 0 }
          , Handler{ hAction = releaseQueueAction, hPrior = 0 }
          , Handler{ hAction = exitAction, hPrior = 0}
          ]
        )
        handleTick
  return ()

runBot :: Side -> IO ()
runBot side = do
  (host, port) <- readNetworkCfg "client.cfg"
  (clientThreadId, sendBuf, recvBuf) <- runClient host port
  status <- register side recvBuf sendBuf
  case status of
    (Just errorMsg) -> putStrLn errorMsg
    Nothing -> playLoop White startingPosition -- TODO: Black
      where
        playLoop :: Side -> Board -> IO ()
        playLoop curSide board = do
          if side == curSide
          then do
            let
              move :: ([Move], Coord)
              move = chooseMove 1 curSide board
            print move
            putMVar sendBuf $ makeMsg "move" move
          else putMVar sendBuf dummyMsg
          msg@Message{ mHead = hd, mBody = body } <- takeMVar recvBuf
          case hd of
            "board" -> do
              let (newBoard, newSide) = deserialize body :: (Board, Side)
              playLoop newSide newBoard
            "str" -> do
              print body
              playLoop side board
            "finish" -> do
              killThread clientThreadId
              exitSuccess
            _ -> do
              putStrLn $ "Unexpected message: " ++ show msg
              playLoop side board

