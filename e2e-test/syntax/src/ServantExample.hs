{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

-- | This example is taken from the servant cookbook
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

data Position = Position
  { xCoord :: Int,
    yCoord :: Int
  }
  deriving (Show, Generic)

instance FromJSON Position

newtype HelloMessage = HelloMessage {msg :: String}
  deriving (Show, Generic)

instance FromJSON HelloMessage

data ClientInfo = ClientInfo
  { clientName :: String,
    clientEmail :: String,
    clientAge :: Int,
    clientInterestedIn :: [String]
  }
  deriving (Generic)

instance ToJSON ClientInfo

data Email = Email
  { from :: String,
    to :: String,
    subject :: String,
    body :: String
  }
  deriving (Show, Generic)

instance FromJSON Email

type API =
  "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

emailForClient :: ClientInfo -> Email
emailForClient c = Email from' to' subject' body'
  where
    from' = "great@company.com"
    to' = clientEmail c
    subject' = "Hey " ++ clientName c ++ ", we miss you!"
    body' =
      "Hi " ++ clientName c ++ ",\n\n"
        ++ "Since you've recently turned "
        ++ show (clientAge c)
        ++ ", have you checked out our latest "
        ++ intercalate ", " (clientInterestedIn c)
        ++ " products? Give us a visit!"

server3 :: Server API
server3 =
  position
    :<|> hello
    :<|> marketing
  where
    position :: Int -> Int -> Handler Position
    position x y = return (Position x y)

    hello :: Maybe String -> Handler HelloMessage
    hello mname = return . HelloMessage $ case mname of
      Nothing -> "Hello, anonymous coward"
      Just n -> "Hello, " ++ n

    marketing :: ClientInfo -> Handler Email
    marketing clientinfo = return (emailForClient clientinfo)

position ::
  -- | value for "x"
  Int ->
  -- | value for "y"
  Int ->
  ClientM Position
hello ::
  -- | an optional value for "name"
  Maybe String ->
  ClientM HelloMessage
marketing ::
  -- | value for the request body
  ClientInfo ->
  ClientM Email
position :<|> hello :<|> marketing = client api

api :: Proxy API
api = Proxy
