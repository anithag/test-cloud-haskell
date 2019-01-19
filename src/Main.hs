{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Control.Concurrent
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport (EndPointAddress(..))
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Data.Binary
import Data.Typeable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Environment (getArgs)
import GHC.Generics (Generic)
import Foreign.StablePtr

data Term = Term Int Char deriving (Generic, Typeable) 
instance Binary Term

sampleTask :: (String, ProcessId) -> Process ()
sampleTask (s, pid) = do
  say "I'm on the other node"
  liftIO $ putStrLn s
  send pid (42::Int)

remotable ['sampleTask]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable


main :: IO ()
main = do
  (tp:rest) <- getArgs
  let workeraddr = "10.0.3.6"
  let manageraddr = "10.0.3.1"
  case tp of
    "worker" -> case rest of
      [port] -> do
        putStrLn $ "starting worker on " ++ port
        Right transport <- createTransport workeraddr port (\port'-> (workeraddr, port') ) defaultTCPParameters
        let addr = mkAddr workeraddr port
        let us = NodeId $ EndPointAddress (BS8.pack addr)
        putStrLn $ "started worker on " ++ port
        node <- newLocalNode transport myRemoteTable
        windowToEternity <- newEmptyMVar
        _ <- newStablePtr =<< myThreadId
        putStrLn $ "waiting for work to come.."
        takeMVar windowToEternity
    "manager" -> case rest of
      [lp, rp] -> do
        putStrLn $ "starting node on " ++ lp
        Right transport <- createTransport manageraddr lp (\port'-> (manageraddr, port') ) defaultTCPParameters
        let remote = mkAddr workeraddr rp
        let local = mkAddr manageraddr lp
        let us = NodeId $ EndPointAddress (BS8.pack local)                                                                    
        let them = NodeId $ EndPointAddress (BS8.pack remote)
        node <- newLocalNode transport myRemoteTable
        reply <- newEmptyMVar
        runProcess node $ do
           me <- getSelfPid
           say "test2"
           _pid <- spawn them $ $(mkClosure 'sampleTask) ("using spawn", me)  
           i <- expect :: Process Int
           liftIO $ putMVar reply i
        print =<< takeMVar reply
  where
    mkAddr ipaddr port = ipaddr ++ ":" ++ port ++ ":0"
