module Network.UCSPI
 ( UCSPIOptions(..)
 , ucspiClient
 , ucspiServer
 ) where

import Control.Arrow (first)
import Data.List (stripPrefix)
import System.IO (Handle, hClose, hPutStrLn, stderr)
import System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd(..))
import System.Environment (getEnvironment)

-- | The @UCSPI@ spec indicates that applications will receive information
--   about their connection through environment variables. A 'UCSPIOptions'
--   value will contains all the relevant values filtered out. As most of
--   these (save for the protocol name) are prefixed with both the protocol
--   name and either @LOCAL@ or @REMOTE@, those values are stripped before
--   they are added to the respective lists: for example, if a @UCSPI@
--   application for the @TCP@ protocol passes in an environment variable
--   @TCPREMOTEPORT=7777@, then the value of 'ucspiProtocol' will be
--   @TCP@ and 'ucspiRemoteVars' will contain a pair @(\"PORT\","7777")@.
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


-- | A @UCSPI@ client is passed an options value, a handle which corresponds
--   to the reading end of a socket, and a handle which corresponds to the
--   writing end of a socket. For example, a sample @UCSPI@ client which
--   connects to a server, writes a line, and reads something back, regardless
--   of the underlying transport mechanism, would look like
--
--   > main :: IO ()
--   > main = ucspiClient $ \_ rdH wrH -> do
--   >   hPutStrLn wrH "hello"
--   >   ln <- hGetLine rdH
--   >   putStrLn ln
ucspiClient :: (UCSPIOptions -> Handle -> Handle -> IO ()) -> IO ()
ucspiClient client = do
  opts <- gatherOptions `fmap` getEnvironment
  case opts of
    Nothing -> hPutStrLn stderr "Non-conformant UCSPI client: no PROTO supplied"
    Just opts' -> do
      rdH <- fdToHandle (Fd 6)
      wrH <- fdToHandle (Fd 7)
      client opts' rdH wrH
      hClose rdH
      hClose wrH

-- | A @UCSPI@ server is passed only an options value. In order to read from or
--   write to the connection, it just uses @stdin@ and @stdout@, which means that
--   a server which reads a single line from a client and echoes it back would
--   look like
--
-- > main :: IO ()
-- > main = ucspiServer $ \_ -> getLine >>= putStrLn
ucspiServer :: (UCSPIOptions -> IO ()) -> IO ()
ucspiServer server = do
  opts <- gatherOptions `fmap` getEnvironment
  case opts of
    Nothing -> hPutStrLn stderr "Non-conformant UCSPI server: no PROTO supplied"
    Just opts' -> server opts'
