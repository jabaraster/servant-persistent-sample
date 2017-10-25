{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module DataStore.Internal where

import           Control.Monad.IO.Class       (liftIO
                                             , MonadIO
                                              )
import           Control.Monad.Logger         (NoLoggingT
                                             , runNoLoggingT
                                             , runStdoutLoggingT
                                             , runStderrLoggingT
                                             , LoggingT
                                             , runLoggingT
                                              )
import           Control.Monad.Trans.Reader   (ReaderT
                                             , runReaderT
                                             , ask
                                             , asks
                                              )
import           Control.Monad.Trans.Resource (ResourceT
                                             , runResourceT
                                              )
import           Data.Aeson
import qualified Data.ByteString.Lazy         as B
import           Data.Maybe
import           Data.Yaml.Config             (loadYamlSettings
                                             , useEnv
                                              )
import           Database.Persist.Postgresql  (PostgresConf(..)
                                             , withPostgresqlConn
                                             , createPostgresqlPool
                                              )
import           Database.Persist.Sql         (SqlPersistT
                                             , Migration
                                             , SqlBackend
                                             , PersistQuery
                                             , PersistEntityBackend
                                             , BaseBackend
                                             , PersistEntity
                                             , PersistQueryRead
                                             , ConnectionPool
                                             , runSqlConn
                                             , runSqlPool
                                             , runMigration
                                              )

-- loadYamlSettingsを使うと環境変数で設定を書き換えるのが楽になる
pgConf :: IO PostgresConf
pgConf = loadYamlSettings ["conf/database-setting.yml"] [] useEnv

doMigration :: Migration -> IO ()
doMigration action = do
    conf <- pgConf
    runStdoutLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runReaderT $ runMigration action

pgPool :: IO ConnectionPool
pgPool = do
    conf <- pgConf
    runStdoutLoggingT $ createPostgresqlPool (pgConnStr conf) (pgPoolSize conf)

runDB :: SqlPersistT (ResourceT (LoggingT IO)) a -> IO a
runDB action = do
    conf <- pgConf
    -- runNoLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runSqlConn f
    runStdoutLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runSqlConn action

runDB' :: SqlPersistT (ResourceT (LoggingT IO)) a -> IO a
runDB' action = do
    pool <- pgPool
    runStdoutLoggingT $ runResourceT $ runSqlPool action pool

