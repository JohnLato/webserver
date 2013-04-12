{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wall #-}
module Network.Web.Server.Params where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Time
import Data.Time.Clock.POSIX
import Network.TCPInfo
import Network.Web.HTTP
import System.Posix.Files

{-|
  The configuration for the basic web server.
-}
data BasicConfig = BasicConfig {
   -- | A mapper from 'URI' to 'Path'.
   mapper :: Request -> IO Path
   -- | Resource obtaining function. The second argument is
   --   (offset of the resource, and length from the offset).
 , obtain :: FilePath -> Maybe (Integer,Integer) -> IO L.ByteString
   -- | A function to return the size of the resource and
   --   its modification time if exists.
 , info   :: FilePath -> IO (Maybe (Integer, UTCTime))
   -- | A server name specified the Server: field.
 , serverName :: S.ByteString
   -- | 'TCPInfo' for passing CGI. (See c10k library.)
 , tcpInfo :: TCPInfo
}

{-|
  Default 'BasicConfig', with 'obtain', 'info', and 'serverName' filled in.
  It is necessary to override the 'mapper' and 'tcpInfo' fields
-}
defaultConfig :: BasicConfig
defaultConfig = BasicConfig
  { mapper = error "BasicConfig: no mapper defined"
  , obtain = defaultObtain
  , info   = defaultInfo
  , serverName = "BasicConfig: no server name"
  , tcpInfo    = error "BasicConfig: no TCPInfo"
  }

{-|
  Control information of how to handle 'URI'.
-}
data Path =
    -- | 'URI' cannot be converted into any resources.
    None
    -- | 'URI' is converted into a resource (typically a file).
  | File FilePath
    -- | 'URI' is converted into CGI.
  | PathCGI CGI
    -- | 'URI' is converted into a handler callback
  | Handler (IO Response)

instance Eq Path where
    (File fp1) == (File fp2) = fp1 == fp2
    (PathCGI cgi1) == (PathCGI cgi2) = cgi1 == cgi2
    None == None = True
    _ == _ = False

instance Show Path where
    show None = "None"
    show (File fp) = "File " ++ show fp
    show (PathCGI cgi) = "PathCGI (" ++ show cgi ++ ")"
    show (Handler _)   = "Handler"

{-|
  Internal information of CGI converted from 'URI'.
-}
data CGI = CGI {
    -- | A program path to be executed.
    progPath    :: FilePath
    -- | A script name.
  , scriptName  :: String
    -- | A path information.
  , pathInfo    :: String
    -- | A query string.
  , queryString :: String
  } deriving (Eq,Show)

-- | Get the size and modification time of a file, if possible.
defaultInfo :: FilePath -> IO (Maybe (Integer, UTCTime))
defaultInfo fp = do
    exists <- fileExist fp
    if exists
        then do
            status <- getFileStatus fp
            let size = fromIntegral $ fileSize status
                mt   = posixSecondsToUTCTime . realToFrac $ modificationTime status
            return $ Just (size,mt)
        else return Nothing

-- | Obtain a data slice from a file as a lazy bytestring.
defaultObtain :: FilePath -> Maybe (Integer,Integer) -> IO L.ByteString
defaultObtain fp Nothing = L.readFile fp
defaultObtain fp (Just (offset,numBytes)) = L.take nb . L.drop ofs <$> L.readFile fp
  where
    nb  = fromIntegral numBytes
    ofs = fromIntegral offset
