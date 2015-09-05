{- 
 - ---------------------------------------------------------------------------- 
 - "THE BEER-WARE LICENSE" (Revision 42): 
 - <code@gregorkopf.de> wrote this file. As long as you retain this notice you 
 - can do whatever you want with this stuff. If we meet some day, and you 
 - think this stuff is worth it, you can buy me a beer in return. Gregor Kopf 
 - ---------------------------------------------------------------------------- 
 -} 

{-| A simple client for gpsd -}
module Network.Gpsd (  GPSContext, initGps, GPSPosition(..), getPosition
                     , getTime, getSpeed, waitForFix) where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM
import           Control.Concurrent
import           Network.Simple.TCP
import           Text.JSON
import           Text.JSON.Parsec hiding (getPosition)
import           Text.ParserCombinators.Parsec hiding (getPosition)
import           Text.ParserCombinators.Parsec.Char
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Control.Applicative hiding (many)
import           Control.Monad
import           System.IO
import           Data.Maybe

data GPSData = TPVData {
      gTime     :: Maybe String
    , gPosition :: Maybe GPSPosition
    , gSpeed    :: Maybe Double
   }

-- | A GPS position as returned by gpsd.
data GPSPosition = GPSPosition {
     gpsLongitude :: Double -- ^ The longitude
   , gpsLatitude  :: Double -- ^ The latitude
  } deriving (Show, Eq)

-- | A GPSContext represents your connection to the GPS daemon.
data GPSContext = GPSContext {
      curPosition :: TVar (Maybe GPSPosition)
    , curSpeed    :: TVar (Maybe Double)
    , curTime     :: TVar (Maybe String)
   }

parseJSON :: CharParser () (JSValue, String)
parseJSON = do
   val  <- p_value
   rest <- many $ noneOf ""
   return (val, rest)

getJson :: String -> Either String (JSValue, String)
getJson input = 
   case runParser parseJSON () "" input of
      Left err -> Left $ show err
      Right x  -> Right x

myReadJSON :: (JSON a) => JSValue -> Either String a
myReadJSON = resultToEither . readJSON

lookupJson :: (JSON a) => JSObject JSValue -> String -> Either String a
lookupJson obj key = 
   case (lookup key $ fromJSObject obj) of
      Just x  -> myReadJSON x
      Nothing -> Left $ "Unknown key " ++ key ++ " in " ++ show (showJSON obj)

handleTPV :: JSObject JSValue -> Either String GPSData
handleTPV obj =
   case (lookupJson obj "mode") :: Either String Int of
      Right 2 -> Right $ handleTPV' obj
      Right 3 -> Right $ handleTPV' obj
      Right x -> Left "No GPS Fix"
      Left  x -> Left x

handleTPV' :: JSObject JSValue -> GPSData
handleTPV' obj =
   let lon = get obj "lon"
       lat = get obj "lat"
       spd = get obj "speed"
       tim = get obj "time"
   in
   TPVData {
      gTime     = tim
    , gPosition = mkPos lon lat
    , gSpeed    = spd
   }
   where get obj x = case (lookupJson obj x) of
                        Left e  -> Nothing
                        Right v -> Just v
         mkPos lon lat = case (,) <$> lon <*> lat of
                           Just (lo, la) -> Just GPSPosition {  gpsLongitude = lo
                                                              , gpsLatitude = la
                                                             }
                           Nothing       -> Nothing

handleJson :: JSValue -> Either String GPSData
handleJson val = 
   case myReadJSON val of
      Left e -> Left e
      Right obj -> case (lookupJson obj "class") of
                     Left  e     -> Left e
                     Right "TPV" -> handleTPV obj
                     Right x     -> Left $ "Unknown object class " ++ x

performUpdate :: GPSData -> GPSContext -> IO ()
performUpdate info ctx = do
   case info of
      tpv @ (TPVData _ _ _) -> atomically $ updateTPV tpv ctx
      _ -> return ()

updateTPV :: GPSData -> GPSContext -> STM ()
updateTPV TPVData { gTime = t, gPosition = p, gSpeed = s} ctx = do
   updateField (curTime     ctx) t
   updateField (curSpeed    ctx) s
   updateField (curPosition ctx) p
   where updateField tv mv = case mv of
                               Just v  -> writeTVar tv (Just v)
                               Nothing -> return ()
updateTPV _ _ = return ()

-- | Gets the last known GPS position
getPosition :: GPSContext -> IO (Maybe GPSPosition)
getPosition ctx = readTVarIO (curPosition ctx)

-- | Gets the last known GPS time
getTime :: GPSContext -> IO (Maybe String)
getTime ctx = readTVarIO (curTime ctx)

-- | Gets the last known GPS speed
getSpeed :: GPSContext -> IO (Maybe Double)
getSpeed ctx = readTVarIO (curSpeed ctx)

-- | Waits for a GPS fix
waitForFix :: GPSContext -> IO ()
waitForFix ctx = atomically $ do
   p <- readTVar (curPosition ctx)
   s <- readTVar (curSpeed ctx)
   check $ isJust p
   check $ isJust s

-- | Initialize a connection to gpsd.
initGps ::    String -- ^ The host name to connect to
           -> Int -- ^ The port number
           -> IO GPSContext -- ^ The resulting GPSContext
initGps server port = do
   p <- newTVarIO Nothing
   s <- newTVarIO Nothing
   t <- newTVarIO Nothing
   let ctx = GPSContext { curPosition = p, curSpeed = s, curTime = t }
   (socket, _) <- connectSock server (show port)
   send socket $ C8.pack "?WATCH={\"enable\":true,\"json\":true}"
   buf <- newTVarIO $ ""
   forkIO $ forever $ do
      serverData <- recv socket 8192
      case serverData of
         Nothing -> do closeSock socket
                       error "Connection closed"
         Just sd -> atomically $ modifyTVar buf (flip (++) (C8.unpack sd))
      buffer <- readTVarIO buf
      rest <- process buffer ctx
      atomically $ writeTVar buf rest
   return ctx
   where process sd ctx = 
            case getJson sd of
                 Left err -> return sd
                 Right (js, rest) -> do case handleJson js of
                                          Left err -> return rest
                                          Right gpsData -> do performUpdate gpsData ctx
                                                              return rest
