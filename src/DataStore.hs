{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE QuasiQuotes                #-}

module DataStore where

import Control.Lens                 ((^.))
import Data.Text                    (Text)
import Database.Persist
import Database.Persist.Sql         (ConnectionPool
                                   , runSqlPool
                                    )
import Database.Persist.TH          (mkMigrate
                                   , mkPersist
                                   , persistLowerCase
                                   , share
                                   , sqlSettings
                                   , mpsGenerateLenses
                                    )
import DataStore.Internal
import GHC.Generics

share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name Text
    age Int
    UniqueUserName name
    deriving Eq Show Generic
|]

getUsers :: ConnectionPool -> IO [Entity User]
getUsers pool = flip runSqlPool pool $ selectList [] []

getUser :: ConnectionPool -> Key User -> IO (Maybe (Entity User))
getUser pool = flip runSqlPool pool . getEntity

insertUser :: ConnectionPool -> User -> IO (Maybe (Entity User))
insertUser pool user = flip runSqlPool pool $ do
    mInDb <- getBy $ UniqueUserName $ user^.userName
    case mInDb of
      Just inDb -> pure Nothing
      Nothing   -> do
                     key <- insert user
                     pure $ Just $ Entity key user

migrateDb :: IO ()
migrateDb = doMigration migrateAll
