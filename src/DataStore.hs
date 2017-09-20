{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE QuasiQuotes                #-}

module DataStore where

import           Data.Text                    (Text)
import           Database.Persist
import           Database.Persist.TH          (mkMigrate
                                             , mkPersist
                                             , persistLowerCase
                                             , share
                                             , sqlSettings
                                              )
import           DataStore.Internal
import           GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name Text
    age Int
    deriving Eq Show Generic
Employee json
    name Text
    deriving Eq Show Generic
|]

selectUsers :: IO [Entity User]
selectUsers = runDB $ selectList [] []

getUser :: Key User -> IO (Maybe (Entity User))
getUser = runDB . getEntity

insertUser :: User -> IO (Entity User)
insertUser user = do
    key <- runDB $ insert user
    pure $ Entity key user

migrate :: IO ()
migrate = doMigration migrateAll
