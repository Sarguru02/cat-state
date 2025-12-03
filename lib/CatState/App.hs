module CatState.App where

import           Data.OpenFeature.SuperpositionProvider (defaultProviderOptions, SuperpositionProviderOptions(..), RefreshOptions(..), newSuperpositionProvider, SuperpositionProvider)
import qualified Network.URI as Net
import qualified Data.OpenFeature.Api as OpenFeature
import qualified Data.OpenFeature.Client as OpenFeature
import qualified Data.OpenFeature.EvaluationContext as EC
import           Data.Aeson (toJSON)

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
                  }
      newSuperpositionProvider options

getClient :: IO (Either Text OpenFeature.Client)
getClient = do
  eProvider <- getProvider "http://localhost:8080" "localorg" "dev" 5
  case eProvider of
    Left err -> pure $ Left $ "Error when getting provider: " <> err
    Right provider -> do
      _ <- OpenFeature.setNamedProvider "CAC" provider
      _ <- putStrLn $ "Done setting up provider with name"
      client <- OpenFeature.createNamedClient "OF_CLIENT"
      pure $ Right client

useClient :: OpenFeature.Client -> IO ()
useClient client' = do
  let context = getContext
  let client = OpenFeature.setClientContext context client'
  evalResult <- OpenFeature.getIntValue client "k1" ( Just context )
  putStrLn $ show evalResult


getContext :: EC.EvaluationContext
getContext = 
  EC.defaultContext 
    & EC.withCustomField "A" (toJSON ("Te"::String))
    & EC.withCustomField "B" (toJSON ("Re" :: String))
    & EC.withCustomField "C" (toJSON ("CT"::String))
    & EC.withCustomField "SM1" (toJSON ("CT1"::String))
    & EC.withCustomField "SM2" (toJSON ("CT2"::String))
