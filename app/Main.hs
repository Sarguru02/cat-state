module Main where

import qualified Data.Text as T
import qualified CatState.App as App

main :: IO ()
main = do
  eClient <- App.getClient
  case eClient of
    Left err -> putStrLn $ "Error occurrred: " <> T.unpack err
    Right client -> App.useClient client
