{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module DataStore.Internal where

import           Control.Monad.Logger         (runStdoutLoggingT)
import           Control.Monad.Trans.Reader   (runReaderT)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Yaml.Config             (loadYamlSettings
                                             , useEnv
                                              )
import           Database.Persist.Postgresql  (PostgresConf(..)
                                             , withPostgresqlConn
                                             , createPostgresqlPool
                                              )
import           Database.Persist.Sql         (Migration
                                             , ConnectionPool
                                             , runMigration
                                              )

-- loadYamlSettingsを使うと環境変数で設定を書き換えるのが楽になる
pgConf :: IO PostgresConf
pgConf = loadYamlSettings ["conf/database-setting.yml"] [] useEnv

pgPool :: IO ConnectionPool
pgPool = do
    conf <- pgConf
    runStdoutLoggingT $ createPostgresqlPool (pgConnStr conf) (pgPoolSize conf)

doMigration :: Migration -> IO ()
doMigration action = do
    conf <- pgConf
    runStdoutLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runReaderT $ runMigration action
