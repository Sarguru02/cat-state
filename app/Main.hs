module Main where

import qualified Data.Text as T
import           Data.OpenFeature.SuperpositionProvider (defaultProviderOptions, SuperpositionProviderOptions(..), RefreshOptions(..), newSuperpositionProvider, SuperpositionProvider, LogLevel(..))
import qualified Network.URI as Net
import qualified Data.ByteString.Lazy as BL
import qualified Data.OpenFeature.FeatureProvider as P
import qualified Data.OpenFeature.Api as OpenFeature
import qualified Data.OpenFeature.Client as OpenFeature
import qualified Data.OpenFeature.EvaluationContext as EC
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS qualified as HttpTLS
import           Data.Aeson (toJSON, eitherDecode, FromJSON, ToJSON, Value(..))
import Data.Map as Map
import GHC.Conc.IO (threadDelay)
import Io.Superposition.SuperpositionClient as SP
--- Create Default config
import qualified Io.Superposition.Model.CreateDefaultConfigInput as CDCI
import qualified Io.Superposition.Model.CreateDefaultConfigOutput as CDCO
import qualified Io.Superposition.Command.CreateDefaultConfig as CDC

-- Create Context??
import qualified Io.Superposition.Command.CreateContext as CC
import qualified Io.Superposition.Model.ContextPut as CP
import qualified Io.Superposition.Model.CreateContextInput as CCI
import qualified Io.Superposition.Model.CreateContextOutput as CCO

-- list contexts.
import qualified Io.Superposition.Command.ListContexts as LC
import qualified Io.Superposition.Model.ListContextsInput as LCI
import qualified Io.Superposition.Model.ListContextsOutput as LCO

-- list default configs.
import qualified Io.Superposition.Command.ListDefaultConfigs as LDC
import qualified Io.Superposition.Model.ListDefaultConfigsInput as LDCI
import qualified Io.Superposition.Model.ListDefaultConfigsOutput as LDCO

main :: IO ()
main = do
  getK1Value
  manager <- HttpTLS.newTlsManagerWith HttpTLS.tlsManagerSettings
  eClient <- createSuperpositionClient manager
  case eClient of 
    Left err -> putStrLn $ "Error bro " <> ( T.unpack err )
    Right client -> do
      -- result <- createDefaultConfig client sampleConfig "dev" "localorg"
      -- result <- loadAndCreateAllConfigs client "configs_new.json" "dev" "localorg"
      -- result <- listCacDefaultConfigs client "dev" "localorg"
      -- result <- listCacContexts client "dev" "localorg"
      -- result <- either (fail . T.unpack) (\ctx -> createCacContext client ctx "dev" "localorg") sampleContextPut
      result <- loadAndCreateAllContexts client "generated_contextputs.json" "dev" "localorg"
      print result
  pure ()

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

createSuperpositionClient :: Http.Manager -> IO (Either Text SP.SuperpositionClient)
createSuperpositionClient manager = do
  let cacHostUrl = "http://localhost:8080"
  case Net.parseURI (T.unpack cacHostUrl) of
    Just uri -> do
      let eitherErrOrClient = SP.build $ do
            SP.setEndpointuri uri
            SP.setHttpmanager manager
      case eitherErrOrClient of
        Left err     -> pure $ Left $ "Failed to create superposition client: " <> show err
        Right client -> pure $ Right $ client
    Nothing  -> pure $ Left $ "Failed to parse CAC host " <> show cacHostUrl


data CreateDefaultConfigRequest = CreateDefaultConfigRequest
  { key                     :: Text
  , value                   :: Value
  , schema                  :: Map Text Value
  , description             :: Text
  , changeReason            :: Text
  , functionName            :: Maybe Text
  , autocompleteFunctionName:: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON   CreateDefaultConfigRequest
instance FromJSON CreateDefaultConfigRequest

sampleConfig :: CreateDefaultConfigRequest
sampleConfig =
  CreateDefaultConfigRequest
    { key = "sample_config"
    , value = Number 42
    , schema = Map.fromList
        [ ("type", String "integer") ]
    , description = "Example default config"
    , changeReason = "Initial creation"
    , functionName = Nothing
    , autocompleteFunctionName = Nothing
    }


sampleContextPut :: Either T.Text CP.ContextPut
sampleContextPut = CP.build $ do
  CP.setContext (Map.fromList
    [ ("Sub_Merchant_1", String "Flipkart")
    ])

  CP.setOverride (Map.fromList
    [ ("k3", Number 69)
    ])

  CP.setDescription (Just "Autogenerated context update")

  CP.setChangeReason "Automated load-and-create"

createDefaultConfig :: SP.SuperpositionClient -> CreateDefaultConfigRequest -> Text -> Text -> IO (Either Text CDCO.CreateDefaultConfigOutput)
createDefaultConfig client createDefaultConfigReq workspace orgId = do
  let inputBuilder = do
        CDCI.setWorkspaceId workspace
        CDCI.setOrgId orgId
        CDCI.setKey (key createDefaultConfigReq)
        CDCI.setValue (value createDefaultConfigReq)
        CDCI.setSchema (schema createDefaultConfigReq)
        CDCI.setDescription (description createDefaultConfigReq)
        CDCI.setChangeReason (changeReason createDefaultConfigReq)
        CDCI.setFunctionName (functionName createDefaultConfigReq)
        CDCI.setAutocompleteFunctionName (autocompleteFunctionName createDefaultConfigReq)
  responseParser CDC.createDefaultConfig client inputBuilder

responseParser 
  :: Show e
  => (a -> b -> IO (Either e d))
  -> a
  -> b
  -> IO (Either Text d)
responseParser api client builder = do
  res <- api client builder
  case res of
    Left err -> pure $ Left (T.pack (show err))
    Right v  -> pure $ Right v

readDefaultConfigList :: FilePath -> IO (Either Text [CreateDefaultConfigRequest])
readDefaultConfigList fp = do
  bytes <- BL.readFile fp
  case eitherDecode bytes of
    Left err  -> pure $ Left ("Failed to parse list of CreateDefaultConfigRequest: " <> T.pack err)
    Right val -> pure $ Right val

createAllConfigsCollect 
  :: SP.SuperpositionClient 
  -> Text
  -> Text
  -> [CreateDefaultConfigRequest]
  -> IO [Either Text CDCO.CreateDefaultConfigOutput]
createAllConfigsCollect client workspace orgId reqs =
  mapM (\r -> createDefaultConfig client r workspace orgId) reqs


loadAndCreateAllConfigs
  :: SP.SuperpositionClient
  -> FilePath
  -> Text
  -> Text
  -> IO (Either Text [CDCO.CreateDefaultConfigOutput])
loadAndCreateAllConfigs client jsonPath workspace orgId = do
  eReqs <- readDefaultConfigList jsonPath
  case eReqs of
    Left err   -> pure (Left err)
    Right reqs -> do
      results <- createAllConfigsCollect client workspace orgId reqs
      pure (sequence results)

readContextPutRequests :: FilePath -> IO (Either Text [CP.ContextPut])
readContextPutRequests fp = do
  bytes <- BL.readFile fp
  case eitherDecode bytes of
    Left err  -> pure $ Left ("Failed to parse list of ContextPuts: " <> T.pack err)
    Right val -> pure $ Right val

createAllContextsCollect 
  :: SP.SuperpositionClient 
  -> Text
  -> Text
  -> [CP.ContextPut]
  -> IO [Either Text CCO.CreateContextOutput]
createAllContextsCollect client workspace orgId reqs =
  mapM (\r -> createCacContext client r workspace orgId) reqs

loadAndCreateAllContexts
  :: SP.SuperpositionClient
  -> FilePath
  -> Text        -- workspace
  -> Text        -- orgId
  -> IO (Either Text [CCO.CreateContextOutput])
loadAndCreateAllContexts client jsonPath workspace orgId = do
  eReqs <- readContextPutRequests jsonPath
  case eReqs of
    Left err -> pure (Left err)
    Right reqs -> do
      results <- createAllContextsCollect client workspace orgId reqs
      pure (sequence results)


listCacContexts :: SP.SuperpositionClient -> Text -> Text -> IO (Either Text LCO.ListContextsOutput)
listCacContexts client workspace orgId = do
  let inputBuilder = do
        LCI.setWorkspaceId workspace
        LCI.setOrgId orgId
        LCI.setAll' ( Just True )
  responseParser LC.listContexts client inputBuilder

createCacContext :: SP.SuperpositionClient -> CP.ContextPut  -> Text -> Text -> IO (Either Text CCO.CreateContextOutput)
createCacContext client contextPutReq workspace orgId = do
  let inputBuilder = do
        CCI.setWorkspaceId workspace
        CCI.setOrgId orgId
        CCI.setRequest contextPutReq
  responseParser CC.createContext client inputBuilder

listCacDefaultConfigs :: SP.SuperpositionClient -> Text -> Text -> IO (Either Text LDCO.ListDefaultConfigsOutput)
listCacDefaultConfigs client workspace orgId = do
  let inputBuilder = do
        LDCI.setWorkspaceId workspace
        LDCI.setOrgId orgId
        LDCI.setAll' ( Just True )
  responseParser LDC.listDefaultConfigs client inputBuilder
