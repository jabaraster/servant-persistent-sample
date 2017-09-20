module DataStore.Internal (
    runDB
  , pgConf
  , doMigration
) where

import           Control.Monad.Logger         (NoLoggingT
                                             , runNoLoggingT
                                             , runStdoutLoggingT
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
import           Database.Persist.Postgresql  (PostgresConf(..)
                                             , withPostgresqlConn
                                              )
import           Database.Persist.Sql         (SqlPersistT
                                             , Migration
                                             , ConnectionPool
                                             , runSqlConn
                                             , runMigration
                                              )

pgConf :: IO PostgresConf
pgConf = B.readFile "conf/database-setting.json" >>= pure . fromJust . decode

doMigration :: Migration -> IO ()
doMigration proc = do
    conf <- pgConf
    runNoLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runReaderT $ runMigration proc

runDB :: SqlPersistT (ResourceT (LoggingT IO)) a -> IO a
runDB f = do
    conf <- pgConf
    -- runNoLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runSqlConn f
    -- flip runLoggingT logFunc $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runSqlConn f
    runStdoutLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runSqlConn f

logFunc = undefined
