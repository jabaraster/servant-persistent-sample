{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson                 (FromJSON, ToJSON, encode, toJSON)
import           GHC.Generics               (Generic)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai.Handler.Warp   (run)

import           Database.Persist
import           Database.Persist.Sql       (ConnectionPool, runSqlPool)
import           DataStore
import           DataStore.Internal
import           Servant

type GetUsers = Get '[JSON] [Entity User]
type GetUser = Capture "userId" (Key User) :> Get '[JSON] (Entity User)
type AddUser = ReqBody '[JSON] User :> PostCreated '[JSON] (Entity User)
type API = "users" :> GetUsers
      :<|> "users" :> GetUser
      :<|> "users" :> AddUser

api :: Proxy API
api = Proxy

data State = State
  { connPool :: ConnectionPool
  }

data ErrorMessage = ErrorMessage
  { message :: String
  } deriving (Show, Read, Generic)
instance ToJSON ErrorMessage
instance FromJSON ErrorMessage

type AppM = ReaderT State Handler

server :: ServerT API AppM
server = getUsers
    :<|> getUser
    :<|> addUser
  where
    res :: Status -> String -> AppM a
    res status message = throwError $ ServantErr {
                   errHTTPCode = statusCode status
                 , errReasonPhrase = message
                 , errBody = encode $ ErrorMessage message
                 , errHeaders = [
                     (hContentType, "application/json")
                   ]
                 }

    addUser :: User -> AppM (Entity User)
    addUser user = do
        State{connPool = pool} <- ask
        mUser <- liftIO $ insertUser pool user
        case mUser of
            Just user -> return user
            Nothing   -> res status400 "user name is duplicated."

    getUser :: Key User -> AppM (Entity User)
    getUser userId = do
        State{connPool = pool} <- ask
        mUser <- liftIO $ selectUser pool userId
        case mUser of
            Just user -> return user
            Nothing -> res status404 "not found."

    getUsers :: AppM [Entity User]
    getUsers = do
        State{connPool = pool} <- ask
        liftIO $ selectAllUsers pool

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server

main :: IO ()
main = do
    let port = 8080
    migrateDb
    pool <- pgPool
    putStrLn ("start server on " ++ (show port))
    run port $ app $ State pool
