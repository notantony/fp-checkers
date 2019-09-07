import Network.Simple.TCP
  ( withSocketsDo
  , HostPreference(Host)
  )
import Server
  ( runServer
  )
import Util
  ( readNetworkCfg   
  )

main :: IO ()
main = withSocketsDo $ do
  (host, port) <- readNetworkCfg "server.cfg"
  runServer (Host host) port
