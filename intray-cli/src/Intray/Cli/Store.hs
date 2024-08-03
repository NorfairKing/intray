{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Store
  ( prettyShowItemAndWait,
  )
where

import Control.Monad.IO.Class
import qualified Data.ByteString as SB
import qualified Data.Text as T
import Data.Time
import Database.Persist
import Database.Persist.Sql
import Intray.API
import Intray.API.ItemType
import Intray.Cli.DB
import Intray.Cli.OptParse
import Network.URI
import Path
import Path.IO
import System.Process.Typed
import Text.Time.Pretty

prettyShowItemAndWait :: AutoOpen -> Path Abs Dir -> UTCTime -> Entity ClientItem -> IO ()
prettyShowItemAndWait aa cacheDir now (Entity cid ClientItem {..}) =
  let idString = show (fromSqlKey cid)
      timeStr = prettyTimestamp now clientItemCreated
      timeAgoString = prettyTimeAuto now clientItemCreated
      typedItem = TypedItem clientItemType clientItemContents
   in case typedItemCase typedItem of
        Left err -> putStrLn $ unlines ["Invalid item:", err]
        Right i -> do
          (contents, mp) <-
            case i of
              CaseTextItem t -> do
                let mp = parseURI (T.unpack t) >>= \uri -> makeAutoOpenConfig aa (show uri)
                pure (T.unpack t, mp)
              CaseImageItem it bs -> do
                let ext =
                      case it of
                        JpgImage -> ".jpg"
                        PngImage -> ".png"
                let fileName = idString ++ ext
                file <- resolveFile cacheDir fileName
                SB.writeFile (fromAbsFile file) bs
                let mp = makeAutoOpenConfig aa (fromAbsFile file)
                pure ("Image: " <> fromAbsFile file, mp)
          let waitFunc = case mp of
                Nothing -> id
                Just pc -> withProcessWait pc . const
          waitFunc $ liftIO $ putStrLn $ unlines [concat [timeStr, " (", timeAgoString, ")"], contents]

makeAutoOpenConfig :: AutoOpen -> String -> Maybe (ProcessConfig () () ())
makeAutoOpenConfig aa arg =
  case aa of
    DontAutoOpen -> Nothing
    AutoOpenWith cmd -> Just (autoOpenConfig cmd arg)

autoOpenConfig :: FilePath -> String -> ProcessConfig () () ()
autoOpenConfig cmd arg = shell $ unwords [cmd, arg]

prettyTimestamp :: UTCTime -> UTCTime -> String
prettyTimestamp now d =
  let year = (\(y, _, _) -> y) . toGregorian . utctDay
   in ( if year now == year d
          then formatTime defaultTimeLocale "%A %B %e at %H:%M"
          else formatTime defaultTimeLocale "%A %B %e %Y at %H:%M"
      )
        d
