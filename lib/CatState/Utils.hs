module CatState.Utils where

import qualified Data.Text as T

printHello :: IO ()
printHello = do
  putStrLn "Hello world"

responseParser :: (Show e, Show d) => (a -> b -> IO (Either e d)) -> a -> b -> IO ()
responseParser api client builder = do
  eitherErrOrResponse <- api client builder
  case eitherErrOrResponse of
    Left err -> putStrLn $ "some error occurred when running api: " <> (T.unpack $ show err)
    Right res -> putStrLn $ show res
