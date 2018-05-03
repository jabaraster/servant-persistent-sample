module Handler2 where

import           Control.Lens     ((^.))
import           Database.Persist
import           DataStore
import           State

addUser :: State -> EUser -> IO (Maybe (Entity EUser))
addUser s user = runDb (connPool s) $ do
    mInDb <- getBy $ UniqueUserName (user^.eUserName)
    case mInDb of
        Just inDb -> pure Nothing
        Nothing   -> insert user >>= pure . Just . (flip Entity) user

getUser :: State -> Key EUser -> IO (Maybe (Entity EUser))
getUser s = runDb (connPool s) . getEntity

getUsers :: State -> IO [Entity EUser]
getUsers s = runDb (connPool s) $ selectList [] []

updateUser :: State -> Key EUser -> EUser -> IO (Maybe (Entity EUser))
updateUser s key user = runDb (connPool s) $ do
    mInDb <- getBy $ UniqueUserName (user^.eUserName)
    case mInDb of
        Nothing -> replace key user >> (pure $ Just $ Entity key user)
        Just (Entity inDbKey _) ->
            if inDbKey /= key
              then pure Nothing
              else replace key user >> (pure $ Just $ Entity key user)
