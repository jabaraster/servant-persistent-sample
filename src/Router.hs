{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Router (
  app
) where

import           Control.Monad.Trans.Reader (runReaderT)
import           Data.Aeson                 (FromJSON, ToJSON, encode)
import           GHC.Generics               (Generic)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status

import           Database.Persist
import           DataStore
import           Handler
import           Servant
import           Servant.Utils.StaticFiles

type API = "users" :> Get '[JSON] [Entity User]
      :<|> "users" :> Capture "userId" (Key User) :> Get '[JSON] (Entity User)
      :<|> "users" :> ReqBody '[JSON] User :> PostCreated '[JSON] (Entity User)
--      :<|> "static" :> Raw

api :: Proxy API
api = Proxy

data ErrorMessage = ErrorMessage
  { message :: String
  } deriving (Show, Read, Generic)
instance ToJSON ErrorMessage
instance FromJSON ErrorMessage

-- serveStatic :: AppM Raw
-- serveStatic = serveDirectoryWebApp "./static"

res :: Status -> String -> AppM a
res status message = throwError $ ServantErr {
               errHTTPCode = statusCode status
             , errReasonPhrase = message
             , errBody = encode $ ErrorMessage message
             , errHeaders = [
                 (hContentType, "application/json")
               ]
             }

server :: ServerT API AppM
server = getUsers
    :<|> getUser'
    :<|> addUser'
--    :<|> serveStatic

getUser' :: Key User -> AppM (Entity User)
getUser' userId = do
    mUser <- getUser userId
    case mUser of
        Nothing -> res status404 "not found."
        Just user -> return user

addUser' :: User -> AppM (Entity User)
addUser' user = do
    mUser <- addUser user
    case mUser of
        Nothing -> res status400 "name is duplicated."
        Just user -> return user


nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server
