module Main (main) where

import qualified Data.OpenFeature.Api as OpenFeature
import qualified Data.OpenFeature.Client as OpenFeature
import qualified Data.OpenFeature.FeatureProvider as OpenFeature
import qualified Data.OpenFeature.EvaluationContext as OpenFeature
import qualified Data.OpenFeature.SuperpositionProvider as Superposition
import Data.Text
import GHC.Conc.IO (threadDelay)
import qualified Network.URI as URI
import qualified Data.Aeson as Aeson

expectJust :: Maybe a -> a
expectJust (Just x) = x
expectJust Nothing  = error "expectJust: Nothing"

expectRight :: Either e a -> a
expectRight (Right x) = x
expectRight (Left _)  = error "expectRight: Left"

main :: IO ()
main = do
  let options =
        Superposition.defaultProviderOptions
          { Superposition.orgId = pack "localorg",
            Superposition.workspaceId = pack "dev",
            Superposition.endpoint = expectJust $ URI.parseURI "http://localhost:8080",
            Superposition.refreshOptions = Superposition.Poll 10,
            Superposition.logLevel = Superposition.LevelDebug
          }
  provider <- expectRight <$> Superposition.newSuperpositionProvider options
  !_ <- OpenFeature.initialize provider OpenFeature.defaultContext
  -- wait a few seconds...
  threadDelay 4000000
  OpenFeature.setDefaultProvider provider
  client <- OpenFeature.createClient
  readData client
  pure ()
  
readData :: OpenFeature.Client -> IO ()
readData client = do
    threadDelay 10000000
    let key = pack "config_35"
    v <- expectRight <$> OpenFeature.getStringValue client key mempty
    print v
    let context = OpenFeature.withCustomField "Tenant" (Aeson.toJSON ("tenant_3"::String)) OpenFeature.defaultContext
    v1 <- expectRight <$> OpenFeature.getStringValue client key (Just context)
    print v1
    _ <- readData client
    pure ()
