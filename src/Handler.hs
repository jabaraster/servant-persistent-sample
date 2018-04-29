module Handler (
    State(..)
  , AppM
  , addUser
  , getUser
  , getUsers
) where

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Database.Persist           (Entity)
import           DataStore
import           Servant
import           State

type AppM = ReaderT State Handler

addUser :: User -> AppM (Maybe (Entity User))
addUser user = do
    State{connPool = pool} <- ask
    liftIO $ insertUser pool user

getUser :: Key User -> AppM (Maybe (Entity User))
getUser userId = do
    State{connPool = pool} <- ask
    liftIO $ selectUser pool userId

getUsers :: AppM [Entity User]
getUsers = do
    State{connPool = pool} <- ask
    liftIO $ selectAllUsers pool
