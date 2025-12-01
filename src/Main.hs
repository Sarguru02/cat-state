module Main where

import Data.Text qualified as T
import Io.Superposition.SuperpositionClient qualified as SP
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as HttpTLS
import Network.URI qualified as URI

-----------------------------
-- Super position commands --
-----------------------------
import qualified Io.Superposition.Model.ListDefaultConfigsInput as LDCI
import qualified Io.Superposition.Command.ListDefaultConfigs as LDC
-- import qualified Io.Superposition.Model.ListDefaultConfigsOutput as LDCO

main :: IO ()
main = do
  manager <- HttpTLS.newTlsManagerWith HttpTLS.tlsManagerSettings
  eClient <- createClient manager
  case eClient of
    Left err -> putStrLn $ "Error occurred when creating client: " <> T.unpack err
    Right client -> useClient client

createClient :: Http.Manager -> IO (Either Text SP.SuperpositionClient)
createClient manager = do
  let host = "http://localhost:8080"

  case URI.parseURI (T.unpack host) of
    Just uri -> do
      pure $ SP.build $ do
        SP.setEndpointuri uri
        SP.setHttpmanager manager
    Nothing -> pure $ Left "Client cannot be created sorry"


-- get resolved config and print it.
useClient :: SP.SuperpositionClient -> IO ()
useClient client = do
  listDefaultConfigs client


listDefaultConfigs :: SP.SuperpositionClient -> IO ()
listDefaultConfigs client = do
  let inputBuilder = do
        LDCI.setWorkspaceId "dev"
        LDCI.setOrgId "localorg"
  responseParser LDC.listDefaultConfigs client inputBuilder

responseParser :: (Show e, Show d) => (a -> b -> IO (Either e d)) -> a -> b -> IO ()
responseParser api client builder = do
  eitherErrOrResponse <- api client builder
  case eitherErrOrResponse of
    Left err -> putStrLn $ "some error occurred when running api: " <> (T.unpack $ show err)
    Right res -> putStrLn $ show res
