import Engine
  ( runGame
  )
import Network.Simple.TCP
  ( withSocketsDo
  )

main :: IO ()
main = withSocketsDo runGame