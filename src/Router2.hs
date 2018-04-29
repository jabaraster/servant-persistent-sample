{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Router2 where

import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (FromJSON, ToJSON, encode)
import           Database.Persist
import           DataStore
import           DataStore.Internal
import           GHC.Generics              (Generic)
import           Handler2
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai.Handler.Warp  (run)
import           Servant
import           Servant.Utils.StaticFiles
import           State

type API = "users" :> Get '[JSON] [Entity User]
      :<|> "users" :> Capture "userId" (Key User) :> Get '[JSON] (Entity User)
      :<|> "users" :> ReqBody '[JSON] User :> PostCreated '[JSON] (Entity User)
      :<|> "static" :> Raw

api :: Proxy API
api = Proxy

data ErrorMessage = ErrorMessage
  { message :: String
  } deriving (Show, Read, Generic)
instance ToJSON ErrorMessage
instance FromJSON ErrorMessage

res :: Status -> String -> Handler a
res status message = throwError $ ServantErr {
               errHTTPCode = statusCode status
             , errReasonPhrase = message
             , errBody = encode $ ErrorMessage message
             , errHeaders = [
                 (hContentType, "application/json")
               ]
             }

server :: State -> Server API
server s = (liftIO . getUsers) s
      :<|> getUser' s
      :<|> addUser' s
      :<|> serveDirectoryWebApp "./static"

getUser' :: State -> Key User -> Handler (Entity User)
getUser' s userId = do
    mUser <- liftIO $ getUser s userId
    case mUser of
        Nothing -> res status404 "not found."
        Just user -> return user

addUser' :: State -> User -> Handler (Entity User)
addUser' s user = do
    mUser <- liftIO $ addUser s user
    case mUser of
        Nothing -> res status400 "name is duplicated."
        Just user -> return user

app :: State -> Application
app s = serve api $ server s

main :: IO ()
main = do
    let port = 8000
    migrateDb
    pool <- pgPool
    putStrLn ("start server on " ++ (show port))
    run port $ app $ State pool
