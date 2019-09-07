import Engine
  ( runGame
  )
import Network.Simple.TCP
  ( withSocketsDo
  )

main :: IO ()
main = withSocketsDo $ do
  runGame
  -- (_ : args) <- getArgs
  -- case args of
  --   []  -> do
  --     return ()
  --   args -> do
  --     putStrLn "data"

      -- parseArgs
    
  -- runServerDefault
  -- runGame

  -- (buf, listenerId) <- runTranslatorDefault

  -- m1 <- getLine
  -- _ <- putMVar buf m1

  -- m2 <- getLine
  -- _ <- putMVar buf m2
  
  -- m3 <- getLine
  -- _ <- putMVar buf m3
  
  -- killThread listenerId
  -- return ()