{-# LANGUAGE OverloadedStrings #-}

{-|
  Creating basic 'WebServer'.
  Created 'WebServer' can handle GET \/ HEAD \/ POST;
  OK, Not Found, Not Modified, Moved Permanently, etc;
  partial getting; language negotication;
  CGI, chunked data for CGI output;
-}

{-# OPTIONS -Wall #-}
module Network.Web.Server.Basic (serveHTTP,
                                 basicServer,
                                 module Network.Web.Server.Params) where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Exception (bracket)
import Control.Monad
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Data.Maybe
import Data.Time
import Network (withSocketsDo, PortID(..), sClose, listenOn)
import Network.TCPInfo
import Network.Web.Date
import Network.Web.HTTP
import Network.Web.Server
import Network.Web.Server.CGI
import Network.Web.Server.Lang
import Network.Web.Server.Params
import Network.Web.Server.Range
import Network.Web.URI
import System.FilePath
import System.IO (openFile, IOMode(..), BufferMode(..), hPutStrLn, hSetBuffering, hClose)
import System.Directory (createDirectoryIfMissing)

import Text.Printf


----------------------------------------------------------------

{-|
  Run an HTTP server, using a default BasicConfig.
-}
serveHTTP :: FilePath       -- ^ Directory to write logfiles, "access.log" and "error.log".  Will be created if it doesn't exist.
          -> Int            -- ^ HTTP port
          -> S.ByteString   -- ^ Server name
          -> (URI -> Path)  -- ^ site mapping function
          -> IO ()
serveHTTP logPath httpPort servName sitemap = do
    createDirectoryIfMissing True logPath
    acclogchan <- newTChanIO
    accsync    <- newEmptyMVar
    errlogchan <- newTChanIO
    errsync    <- newEmptyMVar
    let errlog  = logPath </> "error.log"
        acclog  = logPath </> "access.log"
        logwait = do
            atomically $ writeTChan acclogchan Nothing
            atomically $ writeTChan errlogchan Nothing
            mapM_ takeMVar [accsync, errsync]
        doAcc   = atomically . writeTChan acclogchan . Just
        doErr   = atomically . writeTChan errlogchan . Just

    _ <- forkIO $ logger acclog acclogchan accsync
    _ <- forkIO $ logger errlog errlogchan errsync

    let cfg = WebConfig {
              closedHook = const $ return ()
            , accessHook = doAcc
            , errorHook  = doErr
            , fatalErrorHook = doErr
            , connectionTimer = 2
            }
        topHandler tcpi = basicServer $ defaultConfig {
              serverName = servName
            , tcpInfo = tcpi
            , mapper = sitemap
            }
        runserver = withSocketsDo $ do
            sock <- listenOn (PortNumber $ fromIntegral httpPort)
            void $ mainLoop sock
            sClose sock
        mainLoop sock = do
            conn <- accept sock
            void $ forkIO (runConn conn)
            mainLoop sock
        runConn (hndl,tcpi) = do
            connection hndl (topHandler tcpi) cfg
    runserver
    logwait

logger :: FilePath -> TChan (Maybe String) -> MVar () -> IO ()
logger path chan sync = bracket opener closer go
  where
    opener = do
        h <- openFile path AppendMode
        hSetBuffering h LineBuffering
        return h
    closer h = do
        hClose h
        putMVar sync ()
    go h = do
        val <- atomically $ readTChan chan
        case val of
            Just msg -> hPutStrLn h msg >> go h
            Nothing  -> hClose h >> putMVar sync ()

----------------------------------------------------------------

{-|
  Creating 'WebServer' with 'BasicConfig'.
  The created 'WebServer' can handle GET \/ HEAD \/ POST;
  OK, Not Found, Not Modified, Moved Permanently, etc;
  partial getting; language negotication;
  CGI, chunked data for CGI output;
  If http:\/\/example.com\/path does not exist but
  http:\/\/example.com\/path\/ exists, the created 'WebServer'
  redirects it. http:\/\/example.com\/path\/ is mapped to
  \/somewhere\/path\/ by 'mapper' and index.html and index.html.en
  automatically added and try to read by 'obtain'.
  If Accept-Language is "xx" and "yy" in order,
  index.html.xx, index.html.yy, index.html and index.html.en
  are tried. The created 'WebServer' does not dynamically
  make index.html for a directory even if index.html does not
  exist for security reasons.
-}
basicServer :: BasicConfig -> WebServer
basicServer cnf mreq = case mreq of
    Nothing -> adjust <$> pure responseBadRequest
    Just req -> case reqMethod req of
      GET   -> adjust <$> processGET  cnf req
      HEAD  -> adjust <$> processHEAD cnf req
      POST  -> adjust <$> processPOST cnf req
      _     -> adjust <$> pure responseNotImplement
  where
    adjust = addServer . addPeerToLog
    addServer = insertField FkServer (serverName cnf)
    addPeerToLog rsp = rsp { rspLogMsg = logmsg}
    peer = peerAddr (tcpInfo cnf)
    logmsg = "[" ++ peer ++ "] " ++ maybe "" uri mreq
    uri req = "\"" ++ (S.unpack . toURLwoPort . reqURI) req ++ "\""

----------------------------------------------------------------

runAnyIO :: [IO (Maybe a)] -> IO a
runAnyIO [] = error "runAnyIO"
runAnyIO (a:as) = do
    mrsp <- a
    case mrsp of
      Nothing  -> runAnyIO as
      Just rsp -> return rsp

runAnyMaybeIO :: [IO (Maybe a)] -> IO (Maybe a)
runAnyMaybeIO []     = return Nothing
runAnyMaybeIO (a:as) = do
    mx <- a
    case mx of
      Nothing -> runAnyMaybeIO as
      Just _  -> return mx

processGET :: BasicConfig -> Request -> IO Response
processGET cnf req = do
    let uri = reqURI req
        langs = map ('.':) (languages req) ++ ["",".en"]
    runAnyIO [ tryGet cnf req uri langs
             , tryRedirect cnf uri langs
             , notFound ] -- always Just

processHEAD :: BasicConfig -> Request -> IO Response
processHEAD cnf req = do
    let uri = reqURI req
        langs = map ('.':) (languages req) ++ ["",".en"]
    runAnyIO [ tryHead cnf uri langs
             , tryRedirect cnf uri langs
             , notFound ] -- always Just

processPOST :: BasicConfig -> Request -> IO Response
processPOST = tryPost

languages :: Request -> [String]
languages req = maybe [] (parseLang . S.unpack) $ lookupField FkAcceptLanguage req

----------------------------------------------------------------

(>>|) :: Maybe a -> (a -> IO (Maybe b)) -> IO (Maybe b)
v >>| act =
    case v of
      Nothing -> return Nothing
      Just x  -> act x

(|>|) :: IO (Maybe a) -> (a -> IO (Maybe b)) -> IO (Maybe b)
a |>| act = do
    v <- a
    case v of
      Nothing -> return Nothing
      Just x  -> act x

(|||) :: Maybe Status -> Maybe Status -> Maybe Status
(|||) = mplus

----------------------------------------------------------------

ifModifiedSince :: Request -> Maybe UTCTime
ifModifiedSince = lookupAndParseDate FkIfModifiedSince

ifUnmodifiedSince :: Request -> Maybe UTCTime
ifUnmodifiedSince = lookupAndParseDate FkIfUnmodifiedSince

ifRange :: Request -> Maybe UTCTime
ifRange = lookupAndParseDate FkIfRange

lookupAndParseDate :: FieldKey -> Request -> Maybe UTCTime
lookupAndParseDate key req = lookupField key req >>= parseDate

tryGet :: BasicConfig -> Request -> URI -> [String] -> IO (Maybe Response)
tryGet cnf req uri langs = tryGet' $ mapper cnf uri
  where
    tryGet' None          = return Nothing
    tryGet' (File file)   = tryGetFile cnf req file langs
    tryGet' (PathCGI cgi) = tryGetCGI  cnf req cgi
    tryGet' (Handler hlr) = Just <$> hlr (Just req)

tryGetFile :: BasicConfig -> Request -> FilePath -> [String] -> IO (Maybe Response)
tryGetFile cnf req file langs
  | ".html" `isSuffixOf` file = runAnyMaybeIO $ map (tryGetFile' cnf req file) langs
  | otherwise                 = tryGetFile' cnf req file ""

tryGetFile' :: BasicConfig -> Request -> FilePath -> String -> IO (Maybe Response)
tryGetFile' cnf req file lang = do
    let file' = file ++ lang
    info cnf file' |>| \(size, mtime) -> do
      let ext = takeExtension file
          ct = selectContentType ext
          modified = utcToDate mtime
          mst = ifmodified req size mtime
            ||| ifunmodified req size mtime
            ||| ifrange req size mtime
            ||| unconditional req size mtime
      case mst of
        Just OK -> do
          val <- obtain cnf file' Nothing
          return . Just $ response OK val size ct modified
        Just st@(PartialContent skip len) -> do
          val <- obtain cnf file' $ Just (skip,len)
          let rangeSpec = S.pack $ printf "bytes %d-%d/*" skip (skip+len-1)
          return . Just $ insertField FkContentRange rangeSpec $ response st val len ct modified
        Just st ->
          return . Just $ response st L.empty 0 ct modified
        _       -> return Nothing -- never reached

ifmodified :: Request -> Integer -> UTCTime -> Maybe Status
ifmodified req size mtime = do
    date <- ifModifiedSince req
    if date /= mtime
       then unconditional req size mtime
       else Just NotModified -- xxx rspBody should be Nothing

ifunmodified :: Request -> Integer -> UTCTime -> Maybe Status
ifunmodified req size mtime = do
    date <- ifUnmodifiedSince req
    if date == mtime
       then unconditional req size mtime
       else Just PreconditionFailed

ifrange :: Request -> Integer -> UTCTime -> Maybe Status
ifrange req size mtime = do
    date <- ifRange req
    rng  <- lookupField FkRange req
    if date == mtime
       then Just OK
       else range size rng

unconditional :: Request -> Integer -> UTCTime -> Maybe Status
unconditional req size _ =
    maybe (Just OK) (range size) $ lookupField FkRange req

range :: Integer -> S.ByteString -> Maybe Status
range size rng = case skipAndSize (S.unpack rng) size of
  Nothing         -> Just RequestedRangeNotSatisfiable
  Just (skip,len) -> Just (PartialContent skip len)

----------------------------------------------------------------

tryHead :: BasicConfig -> URI -> [String] -> IO (Maybe Response)
tryHead cnf uri langs = tryHead' (mapper cnf uri)
  where
    tryHead' None        = return Nothing
    tryHead' (PathCGI _) = return Nothing
    tryHead' (Handler _) = return Nothing
    tryHead' (File file) = tryHeadFile cnf file langs

tryHeadFile :: BasicConfig -> FilePath -> [String] -> IO (Maybe Response)
tryHeadFile cnf file langs
  | ".html" `isSuffixOf` file = runAnyMaybeIO $ map (tryHeadFile' cnf file) langs
  | otherwise                 = tryHeadFile' cnf file ""

tryHeadFile' :: BasicConfig -> FilePath -> String -> IO (Maybe Response)
tryHeadFile' cnf file lang = do
    let ext = takeExtension file
        ct = selectContentType ext
        file' = file ++ lang
    minfo <- info cnf file'
    case minfo of
      Nothing     -> return Nothing
      Just (size,mt) -> return $ Just (responseOK ct size (utcToDate mt))

----------------------------------------------------------------

redirectURI :: URI -> Maybe URI
redirectURI uri =
   let path = uriPath uri
   in if "/" `S.isSuffixOf` path
      then Nothing
      else Just uri { uriPath = path `S.append` "/" }

tryRedirect :: BasicConfig -> URI -> [String] -> IO (Maybe Response)
tryRedirect cnf uri langs =
    redirectURI uri >>| \ruri -> tryRedirect' (mapper cnf ruri) ruri
  where
    tryRedirect' None           _ = return Nothing
    tryRedirect' (PathCGI _)    _ = return Nothing
    tryRedirect' (Handler _)    _ = return Nothing
    tryRedirect' (File file) ruri = runAnyMaybeIO $ map (tryRedirectFile cnf ruri file) langs

tryRedirectFile :: BasicConfig -> URI -> FilePath -> String -> IO (Maybe Response)
tryRedirectFile cnf ruri file lang = do
    let file' = file ++ lang
    minfo <- info cnf file'
    case minfo of
      Nothing -> return Nothing
      Just _  -> return $ Just (responseRedirect ruri)

----------------------------------------------------------------

tryPost :: BasicConfig -> Request -> IO Response
tryPost cnf req = case mapper cnf (reqURI req) of
    -- never reached to undefined
    PathCGI cgi -> fromMaybe undefined <$> tryGetCGI cnf req cgi
    _           -> return responseBadRequest

----------------------------------------------------------------

notFound :: IO (Maybe Response)
notFound = return $ Just responseNotFound

----------------------------------------------------------------

response :: Status -> L.ByteString -> Integer -> CT -> HttpDate -> Response
response st val len ct modified = makeResponse2 st (Just val) (Just len) kvs
  where
    kvs = [(FkContentType,ct),(FkLastModified,modified)]

----------------------------------------------------------------

responseOK :: CT -> Integer -> HttpDate -> Response
responseOK ct size modified = makeResponse2 OK (Just L.empty) (Just size) kvs
  where
    kvs = [(FkContentType,ct),(FkLastModified,modified)]

responseRedirect :: URI -> Response
responseRedirect rurl = makeResponse MovedPermanently [(FkLocation, toURL rurl)]

responseNotFound :: Response
responseNotFound = makeResponse NotFound []

----------------------------------------------------------------

responseBadRequest :: Response
responseBadRequest = makeResponse BadRequest []

responseNotImplement :: Response
responseNotImplement = makeResponse NotImplemented []
