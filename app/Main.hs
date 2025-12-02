module Main where

import qualified Network.HTTP.Client.TLS as HttpTLS
import qualified Data.Text as T
import qualified CatState.App as App

main :: IO ()
main = do
  manager <- HttpTLS.newTlsManagerWith HttpTLS.tlsManagerSettings
  eClient <- App.createClient manager
  case eClient of
    Left err -> putStrLn $ "Error occurred when creating client: " <> T.unpack err
    Right client -> App.useClient client
