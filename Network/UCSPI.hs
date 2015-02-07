module Network.UCSPI where

import Control.Arrow (first)
import Data.List (stripPrefix)
import System.IO (Handle, hPutStrLn, stderr)
import System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd(..))
import System.Environment (getEnvironment)

data UCSPIOptions = UCSPIOptions
  { ucspiProtocol   :: String
  , ucspiLocalVars  :: [(String,String)]
  , ucspiRemoteVars :: [(String,String)]
  } deriving (Eq, Show)

gatherOptions :: [(String,String)] -> Maybe UCSPIOptions
gatherOptions envs = do
  proto <- lookup "PROTO" envs
  let locals = [ (k, v) | let pr = proto ++ "LOCAL"
                        , (Just k, v) <- map (first (stripPrefix pr)) envs
                        ]
  let remotes = [ (k, v) | let pr = proto ++ "REMOTE"
                         , (Just k, v) <- map (first (stripPrefix pr)) envs
                         ]
  return $ UCSPIOptions
             { ucspiProtocol = proto
             , ucspiLocalVars = locals
             , ucspiRemoteVars = remotes
             }


ucspiClient :: (UCSPIOptions -> Handle -> Handle -> IO ()) -> IO ()
ucspiClient client = do
  opts <- gatherOptions `fmap` getEnvironment
  case opts of
    Nothing -> hPutStrLn stderr "Non-conformant UCSPI client: no PROTO supplied"
    Just opts' -> do
      rdH <- fdToHandle (Fd 6)
      wrH <- fdToHandle (Fd 7)
      client opts' rdH wrH

ucspiServer :: (UCSPIOptions -> IO ()) -> IO ()
ucspiServer server = do
  opts <- gatherOptions `fmap` getEnvironment
  case opts of
    Nothing -> hPutStrLn stderr "Non-conformant UCSPI server: no PROTO supplied"
    Just opts' -> server opts'
