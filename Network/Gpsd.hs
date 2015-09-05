module Network.Gpsd (  GPSContext, initGps, GPSPosition, getPosition
                     , getTime, getSpeed) where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM
import           Control.Concurrent
import           Network.Simple.TCP
import           Text.JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Control.Applicative
import           Control.Monad
import           System.IO

data GPSData = TPVData {
      gTime     :: Maybe String
    , gPosition :: Maybe GPSPosition
    , gSpeed    :: Maybe Double
   }

data GPSPosition = GPSPosition {
     gpsLongitude :: Double
   , gpsLatitude  :: Double
  }

data GPSContext = GPSContext {
      curPosition :: TVar (Maybe GPSPosition)
    , curSpeed    :: TVar (Maybe Double)
    , curTime     :: TVar (Maybe String)
   }

getJson :: (JSON a) => BS.ByteString -> Either String a
getJson = resultToEither . decode . C8.unpack

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

handleJson :: JSObject JSValue -> Either String GPSData
handleJson obj = 
   case (lookupJson obj "class") of
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

getPosition :: GPSContext -> IO (Maybe GPSPosition)
getPosition ctx = readTVarIO (curPosition ctx)

getTime :: GPSContext -> IO (Maybe String)
getTime ctx = readTVarIO (curTime ctx)

getSpeed :: GPSContext -> IO (Maybe Double)
getSpeed ctx = readTVarIO (curSpeed ctx)

initGps :: String -> Int -> IO GPSContext
initGps server port = do
   p <- newTVarIO Nothing
   s <- newTVarIO Nothing
   t <- newTVarIO Nothing
   let ctx = GPSContext { curPosition = p, curSpeed = s, curTime = t }
   (socket, _) <- connectSock server (show port)
   send socket $ C8.pack "?WATCH={\"enable\":true,\"json\":true}"
   forkIO $ forever $ do
      serverData <- recv socket 1024
      case serverData of
         Nothing -> do closeSock socket
                       error "Connection closed"
         Just sd -> process sd ctx
   return ctx
   where process sd ctx = 
            case getJson sd of
                 Left err -> return ()
                 Right js -> do case handleJson js of
                                  Left err -> hPutStrLn stderr $ "Error: " ++ err
                                  Right gpsData -> performUpdate gpsData ctx
