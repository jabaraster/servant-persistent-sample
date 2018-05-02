module Handler2 where

import           Database.Persist       (Entity)
import           DataStore
import           State

addUser :: State -> EUser -> IO (Maybe (Entity EUser))
addUser s user = insertUser (connPool s) user

getUser :: State -> Key EUser -> IO (Maybe (Entity EUser))
getUser s userId = selectUser (connPool s) userId

getUsers :: State -> IO [Entity EUser]
getUsers s = selectAllUsers (connPool s)
