module DataStore.Internal (
    runDB
  , runDB'
  , pgConf
  , pgPool
  , doMigration
) where

import           Control.Monad.Logger         (NoLoggingT
                                             , runNoLoggingT
                                             , runStdoutLoggingT
                                             , runStderrLoggingT
                                             , LoggingT
                                             , runLoggingT
                                              )
import           Control.Monad.Trans.Reader   (ReaderT
                                             , runReaderT
                                              )
import           Control.Monad.Trans.Resource (ResourceT
                                             , runResourceT
                                              )
import           Data.Aeson
import qualified Data.ByteString.Lazy         as B
import           Data.Maybe
import           Data.Yaml.Config
import           Database.Persist.Postgresql  (PostgresConf(..)
                                             , withPostgresqlConn
                                             , createPostgresqlPool
                                              )
import           Database.Persist.Sql         (SqlPersistT
                                             , Migration
                                             , ConnectionPool
                                             , runSqlConn
                                             , runSqlPool
                                             , runMigration
                                              )

-- loadYamlSettingsを使うと環境変数で設定を書き換えるのが楽になる
pgConf :: IO PostgresConf
pgConf = loadYamlSettings ["conf/database-setting.yml"] [] useEnv

doMigration :: Migration -> IO ()
doMigration proc = do
    conf <- pgConf
    runNoLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runReaderT $ runMigration proc

pgPool :: IO ConnectionPool
pgPool = do
    conf <- pgConf
    runStdoutLoggingT $ createPostgresqlPool (pgConnStr conf) (pgPoolSize conf)

runDB :: SqlPersistT (ResourceT (LoggingT IO)) a -> IO a
runDB action = do
    conf <- pgConf
    -- runNoLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runSqlConn f
    -- flip runLoggingT logFunc $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runSqlConn f
    runStdoutLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runSqlConn action

runDB' :: SqlPersistT (ResourceT (LoggingT IO)) a -> IO a
runDB' f = undefined
    -- runStdoutLoggingT . runResourceT . runReaderT . runSqlPool f 

logFunc = undefined
