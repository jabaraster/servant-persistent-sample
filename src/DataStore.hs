{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DataStore where

import           Control.Lens           ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
-- import qualified Database.Esqueleto     as E
import           Database.Persist
import           Database.Persist.Sql   (ConnectionPool, runSqlPool)
import           Database.Persist.TH    (mkMigrate, mkPersist, mpsGenerateLenses, persistLowerCase, share, sqlSettings)
import           DataStore.Internal
import           GHC.Generics

share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name Text
    age Int
    UniqueUserName name
    deriving Eq Show
Person json
    name String
    age Int Maybe
    deriving Eq Show Generic
|]

runDb pool action = flip runSqlPool pool action

-- putPersons :: E.SqlPersist m ()
-- putPersons = E.select $
--                  E.from $ \person -> do
--                  return person

getUsers :: ConnectionPool -> IO [Entity User]
getUsers pool = runDb pool $ selectList [] []

getUser :: ConnectionPool -> Key User -> IO (Maybe (Entity User))
getUser pool = runDb pool . getEntity

insertUser :: ConnectionPool -> User -> IO (Maybe (Entity User))
insertUser pool user = runDb pool $ do
    mInDb <- getBy $ UniqueUserName $ user^.userName
    case mInDb of
      Just inDb -> pure Nothing
      Nothing   -> do
                     key <- insert user
                     pure $ Just $ Entity key user

migrateDb :: IO ()
migrateDb = doMigration migrateAll
