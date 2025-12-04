module Main where

import qualified Data.Text as T
import           Data.OpenFeature.SuperpositionProvider (defaultProviderOptions, SuperpositionProviderOptions(..), RefreshOptions(..), newSuperpositionProvider, SuperpositionProvider, LogLevel(..))
import qualified Network.URI as Net
import qualified Data.OpenFeature.FeatureProvider as P
import qualified Data.OpenFeature.Api as OpenFeature
import qualified Data.OpenFeature.Client as OpenFeature
import qualified Data.OpenFeature.EvaluationContext as EC
import           Data.Aeson (toJSON)
import GHC.Conc.IO (threadDelay)


main :: IO ()
main = do
  getK1Value

getProvider :: String -> Text -> Text -> Int -> IO (Either Text SuperpositionProvider)
getProvider uri orgId workspaceId pollTime = do
  case Net.parseURI uri of
    Nothing -> pure $ Left "URI IS NOT PARSED"
    Just uriVal -> do
      let options = defaultProviderOptions
                  { orgId = orgId
                  , workspaceId = workspaceId
                  , endpoint = uriVal
                  , refreshOptions = Poll pollTime
                  , logLevel = LevelDebug
                  }
      newSuperpositionProvider options

getK1Value :: IO ()
getK1Value = do
  eProvider <- getProvider "http://localhost:8080" "localorg" "dev" 5
  case eProvider of
    Left err -> putStrLn $ "Error when getting provider: " <> T.unpack err
    Right provider -> do
      !_ <- P.initialize provider EC.defaultContext
      threadDelay 10000000
      OpenFeature.setDefaultProvider provider
      client <- OpenFeature.createClient
      v <- OpenFeature.getIntValue client (T.pack "k1") ( Just getContext )
      putStrLn $ show v

getContext :: EC.EvaluationContext
getContext = 
  EC.defaultContext 
    & EC.withCustomField "A" (toJSON ("Te"::String))
    & EC.withCustomField "B" (toJSON ("Re" :: String))
    & EC.withCustomField "C" (toJSON ("CT"::String))
    & EC.withCustomField "SM1" (toJSON ("CT1"::String))
    & EC.withCustomField "SM2" (toJSON ("CT2"::String))
