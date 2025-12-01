module Main where

import Data.Text qualified as T
import Io.Superposition.SuperpositionClient qualified as SP
import Network.HTTP.Client qualified as Http
import Network.HTTP.Client.TLS qualified as HttpTLS
import Network.URI qualified as URI

main :: IO ()
main = do
  manager <- HttpTLS.newTlsManagerWith HttpTLS.tlsManagerSettings
  client <- createClient manager
  case client of
    Left err -> putStrLn $ "Error occurred when creating client: " <> T.unpack err
    Right _ -> putStrLn "Client created"

createClient :: Http.Manager -> IO (Either Text SP.SuperpositionClient)
createClient manager = do
  let url = "http://localhost:8080"
  let token = "blabla"

  case URI.parseURI (T.unpack url) of
    Just uri -> do
      pure $ SP.build $ do
        SP.setEndpointuri uri
        SP.setHttpmanager manager
        SP.setBasicauth (Just SP.BasicAuth {SP._creds = token})
    Nothing -> pure $ Left "Client cannot be created sorry"
