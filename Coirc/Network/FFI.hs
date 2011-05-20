module Coirc.FFI where
import qualified Network as N
import System.IO

hConnect :: String -> IO Handle
hConnect host = do
  h <- N.connectTo host (N.PortNumber 6667)
  hSetBuffering h NoBuffering
  return h

