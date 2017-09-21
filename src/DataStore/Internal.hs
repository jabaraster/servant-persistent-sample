module DataStore.Internal (
    runDB
  , pgConf
  , doMigration
) where

import           Control.Monad.Logger         (NoLoggingT
                                             , runNoLoggingT
                                             , runStdoutLoggingT
                                             , runStderrLoggingT
                                             , LoggingT
                                             , runLoggingT
                                              )
import           Control.Monad.Trans.Reader   (runReaderT)
import           Control.Monad.Trans.Resource (ResourceT
                                             , runResourceT
                                              )
import           Data.Aeson
import qualified Data.ByteString.Lazy         as B
import           Data.Maybe
import           Data.Yaml.Config
import           Database.Persist.Postgresql  (PostgresConf(..)
                                             , withPostgresqlConn
                                              )
import           Database.Persist.Sql         (SqlPersistT
                                             , Migration
                                             , ConnectionPool
                                             , runSqlConn
                                             , runMigration
                                              )

-- loadYamlSettingsを使うと環境変数で設定を書き換えるのが楽になる
pgConf :: IO PostgresConf
pgConf = loadYamlSettings ["conf/database-setting.yml"] [] useEnv

doMigration :: Migration -> IO ()
doMigration proc = do
    conf <- pgConf
    runNoLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runReaderT $ runMigration proc

runDB :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB f = do
    conf <- pgConf
    runNoLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runSqlConn f
    -- flip runLoggingT logFunc $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runSqlConn f
    -- runStdoutLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runSqlConn f

logFunc = undefined
