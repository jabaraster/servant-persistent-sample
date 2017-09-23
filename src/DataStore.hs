{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE QuasiQuotes                #-}

module DataStore where

import           Control.Lens
import           Data.Text                    (Text)
import           Database.Persist
import           Database.Persist.TH          (mkMigrate
                                             , mkPersist
                                             , persistLowerCase
                                             , share
                                             , sqlSettings
                                             , mpsGenerateLenses
                                              )
import           DataStore.Internal
import           GHC.Generics

share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
User json
    name Text
    age Int
    UniqueUserName name
    deriving Eq Show Generic
|]

selectUsers :: IO [Entity User]
selectUsers = runDB' $ selectList [] []

getUser :: Key User -> IO (Maybe (Entity User))
getUser = runDB . getEntity

insertUser :: User -> IO (Maybe (Entity User))
insertUser user = runDB $ do
    mInDb <- getBy $ UniqueUserName $ user^.userName
    case mInDb of
      Just inDb -> pure Nothing
      Nothing   -> do
                     key <- insert user
                     return $ Just $ Entity key user

migrate :: IO ()
migrate = do
    pgConf >>= print
    doMigration migrateAll
