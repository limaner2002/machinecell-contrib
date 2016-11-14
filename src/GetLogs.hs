{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module GetLogs where

import ClassyPrelude
import Control.Monad.Trans.Resource hiding (throwM)
import Control.Arrow
import Network.HTTP.Client
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Data.Conduit hiding (await, yield)
import MachineUtils hiding (filter)
import Network.HTTP.Types.Header
import Text.XML.HXT.Core hiding (trace, getNode)

getCookie :: MonadResource m => Kleisli m (Manager, String) CookieJar
getCookie = Kleisli (\(mgr, url) -> do
                        req <- parseUrlThrow url
                        resp <- http req mgr
                        return $ responseCookieJar resp
                    )

makeResp :: MonadResource m => Kleisli m (Manager, String) (Response (ResumableSource m ByteString))
makeResp = Kleisli (\(mgr, url) -> do
                       req <- parseUrlThrow url
                       http req mgr
                   )

getCookieJar :: Arrow a => a (Response body) CookieJar
getCookieJar = arr responseCookieJar

destroyCookies :: Arrow a => a CookieJar [Cookie]
destroyCookies = arr destroyCookieJar

extractSession :: Arrow a => a [Cookie] [Cookie]
extractSession = arr (filter (\c -> cookie_name c == "JSESSIONID"))

extractNode :: Arrow a => a [Cookie] [ByteString]
extractNode = arr (concat . fmap (drop 1 . splitSeq "." . cookie_value))

getNode :: MonadResource m => ByteString -> ProcessA (Kleisli m) (Event (Request, Manager)) (Event (ReleaseKey, Response BodyReader))
getNode nodeName = switch pred evt
  where
    pred = proc x -> do
      resp <- makeRequest -< x
      returnA -< (noEvent, resp)
    evt resp
      | any (== nodeName) (getNodeName resp) = evMap (const resp)
      | otherwise = getNode nodeName
    getNodeName = arr snd >>> arr (responseCookieJar) >>> destroyCookies >>> extractSession >>> extractNode

getNode' nodeName = doTill (any (== nodeName)) (getCookie >>> destroyCookies >>> extractSession >>> extractNode)

downloadLog :: MonadResource m => String -> Text -> Manager -> ProcessA (Kleisli m) (Event (ReleaseKey, Response BodyReader)) (Event (ReleaseKey, Response BodyReader))
downloadLog logBaseUrl logName mgr = machine (downloadLog_ logBaseUrl logName mgr) >>> makeRequest

downloadLog_ :: (MonadResource m, MonadThrow m) => String -> Text -> Manager -> (ReleaseKey, Response BodyReader) -> m (Request, Manager)
downloadLog_ logBaseUrl logName mgr (key, resp) = do
  req <- logReq logBaseUrl logName
  req' <- insertCookies cookieJar req
  return (req', mgr)
  where
    cookieJar = responseCookieJar resp

login :: (MonadResource m, MonadThrow m) => String -> Manager -> Text -> Text -> ProcessA (Kleisli m) (Event (ReleaseKey, Response BodyReader)) (Event (ReleaseKey, Response BodyReader))
login baseUrl mgr un pw = machine (loginReq baseUrl mgr un pw) >>> makeRequest

loginReq :: (MonadResource m, MonadThrow m) => String -> Manager -> Text -> Text -> (ReleaseKey, Response BodyReader) -> m (Request, Manager)
loginReq baseUrl mgr un pw (key, resp) = do
  req <- authReq baseUrl un pw csrfToken
  req' <- insertCookies cookieJar req
  return (req', mgr)
  where
    cookieJar = responseCookieJar resp
    csrfToken = fmap (\(name, value) -> (name, Just value)) (getCSRFToken cookieJar)

insertCookies :: MonadIO m => CookieJar -> Request -> m Request
insertCookies cookies req = do
  now <- liftIO $ getCurrentTime
  return $ fst $ insertCookiesIntoRequest req cookies now

getServerSession :: Kleisli (ResourceT IO) (Manager, String) [Cookie]
getServerSession = getCookie >>> destroyCookies >>> extractSession

passthroughK :: (Monad m) => (a -> m ()) -> Kleisli m a a
passthroughK act = proc a -> do
  _ <- Kleisli act -< a
  returnA -< a

printRequesting :: (MonadIO m) => Kleisli m (a, String) (a, String)
printRequesting = passthroughK (\(_, url) -> putStrLn $ "Making request to: " <> tshow url)

printIt :: (MonadIO m, Show a) => Kleisli m a a
printIt = passthroughK print

doTill :: ArrowChoice cat => (b -> Bool) -> cat a b -> cat a b
doTill pred  f = proc x -> do
  res <- f -< x
  case pred res of
    True -> returnA -< res
    False -> doTill pred f -< x

authReq :: MonadThrow m => String -> Text -> Text -> [(ByteString, Maybe ByteString)] -> m Request
authReq baseUrl un pw params = setRequestMethod "POST"
  <$> addRequestHeader "Content-Type" "application/x-www-form-urlencoded"
  <$> setQueryString (params <> authParams (encodeUtf8 un) (encodeUtf8 pw))
  <$> setRequestHeaders baseHeaders
  <$> parseUrlThrow (baseUrl <> "/suite/auth?appian_environment=tempo")

logReq :: MonadThrow m => String -> Text -> m Request
logReq logBaseUrl logName = setRequestHeaders baseHeaders
  <$> parseUrlThrow (logBaseUrl <> "/" <> unpack logName)

baseHeaders :: [(HeaderName, ByteString)]
baseHeaders = [ ("Accept-Language", "en-US,en;q=0.8")
              , ("Upgrade-Insecure-Requests", "1")
              , ("Accept-Encoding", "gzip, deflate, sdch, br")
              , ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/602.1.50 (KHTML, like Gecko) Version/10.0 Safari/602.1.50")
              , ("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
              , ("Connection", "keep-alive")
              ]

authParams :: ByteString -> ByteString -> [(ByteString, Maybe ByteString)]
authParams un pw = [ ("un", Just un)
           , ("pw", Just pw)
           , ("spring_security_remember_me", Just "on")
           ]

getCSRFToken :: MonadThrow m => CookieJar -> m (ByteString, ByteString)
getCSRFToken cookies = do
  let r = runLA (arrL destroyCookieJar >>> cookieHasName "__appianCsrfToken" >>> constA "X-APPIAN-CSRF-TOKEN" &&& arr (cookie_value))

  case r cookies of
    [t] -> return t
    _ -> throwM $ NoTokenException "There is not CSRF token present!"

data NoTokenException = NoTokenException Text

instance Show NoTokenException where
  show (NoTokenException msg) = "NoTokenException: " <> show msg

instance Exception NoTokenException

cookieHasName :: ArrowList a => ByteString -> a Cookie Cookie
cookieHasName str = isA (\c -> cookie_name c == str)

