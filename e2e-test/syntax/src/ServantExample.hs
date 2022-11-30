{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-# HLINT ignore #-}

-- | This example is taken from the servant cookbook
--
-- It is important because it has some top-level patterns
--
-- https://docs.servant.dev/en/stable/cookbook/structuring-apis/StructuringApis.html
module ServantExample where

import Data.Aeson
import Data.List
import Data.Proxy
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant
import Servant.API
import Servant.Client
import qualified Servant.Client.Streaming as S
import Servant.Types.SourceT (foreach)

main :: IO ()
main = pure ()

type API =
  "foo" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] String
    :<|> "bar" :> QueryParam "name" String :> Get '[JSON] String

server3 :: Server API
server3 = foo :<|> bar
  where
    foo :: Int -> Int -> Handler String
    foo x y = pure "hi"

    bar :: Maybe String -> Handler String
    bar mname = pure $ case mname of
      Nothing -> "ho"
      Just name -> name

fooClient ::
  -- | value for "x"
  Int ->
  -- | value for "y"
  Int ->
  ClientM String
barClient ::
  -- | an optional value for "name"
  Maybe String ->
  ClientM String
fooClient :<|> barClient = client api

api :: Proxy API
api = Proxy
