{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DataStore where

import           Control.Lens           ((^.))
import           Data.Text              (Text)
import           Database.Persist
import           Database.Persist.Sql   (ConnectionPool, runSqlPool)
import           Database.Persist.TH    (mkMigrate, mkPersist, mpsGenerateLenses, persistLowerCase, share, sqlSettings)
import           DataStore.Internal

share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "migrateAll"] [persistLowerCase|
EUser json
    name Text
    age Int
    UniqueUserName name
    deriving Eq Show
EPerson json
    name String
    age Int Maybe
    deriving Eq Show
|]

runDb pool action = flip runSqlPool pool action

selectAllUsers :: ConnectionPool -> IO [Entity EUser]
selectAllUsers pool = runDb pool $ selectList [] []

selectUser :: ConnectionPool -> Key EUser -> IO (Maybe (Entity EUser))
selectUser pool = runDb pool . getEntity

insertUser :: ConnectionPool -> EUser -> IO (Maybe (Entity EUser))
insertUser pool user = runDb pool $ do
    mInDb <- getBy $ UniqueUserName $ user^.eUserName
    case mInDb of
      Just inDb -> pure Nothing
      Nothing   -> do
                     key <- insert user
                     pure $ Just $ Entity key user

migrateDb :: IO ()
migrateDb = doMigration migrateAll
