import Network.Simple.TCP (HostPreference (Host), withSocketsDo)
import Server (runServer)
import Util (readNetworkCfg)

main :: IO ()
main = withSocketsDo $ do
  (host, port) <- readNetworkCfg "server.cfg"
  runServer (Host host) port
