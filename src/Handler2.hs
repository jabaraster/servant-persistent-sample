module Handler2 where

import           Database.Persist       (Entity)
import           DataStore
import           State

addUser :: State -> User -> IO (Maybe (Entity User))
addUser s user = insertUser (connPool s) user

getUser :: State -> Key User -> IO (Maybe (Entity User))
getUser s userId = selectUser (connPool s) userId

getUsers :: State -> IO [Entity User]
getUsers s = selectAllUsers (connPool s)
