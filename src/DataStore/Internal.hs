module DataStore.Internal (
    runDB
  , pgConf
  , doMigration
) where

import           Control.Monad.Logger         (NoLoggingT
                                             , runNoLoggingT
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
                                             , ConnectionPool
                                             , runSqlConn
                                             , runMigration
                                              )

pgConf :: IO PostgresConf
pgConf = B.readFile "conf/database-setting.json" >>= pure . fromJust . decode

-- doMigration :: IO ()
doMigration f = do
    conf <- pgConf
    runNoLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runReaderT $ runMigration f

runDB :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runDB f = do
    conf <- pgConf
    runNoLoggingT $ runResourceT $ withPostgresqlConn (pgConnStr conf) $ runSqlConn f
