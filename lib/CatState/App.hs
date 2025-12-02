module CatState.App where

import qualified Data.Text as T
import qualified Io.Superposition.SuperpositionClient as SP
import qualified Network.HTTP.Client as Http
import qualified Network.URI as URI

import           CatState.Utils (responseParser)

-----------------------------
-- Super position commands --
-----------------------------
import qualified Io.Superposition.Model.ListDefaultConfigsInput as LDCI
import qualified Io.Superposition.Command.ListDefaultConfigs as LDC
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
