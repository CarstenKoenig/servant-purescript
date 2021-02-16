{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeApplications      #-}



module Main where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Logger               (runStderrLoggingT)
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader         hiding (ask)
import           Counter.WebAPI
import           Data.Aeson
import qualified Data.ByteString.Lazy               as B
import           Data.IORef
import           Data.Monoid
import           Data.Proxy
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as T
import qualified Data.Text.IO                       as T
import           Data.Typeable
import           GHC.Generics                       (Generic)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API
import System.Directory (getCurrentDirectory)


data CounterData = CounterData {
    _counter    :: IORef Int
  }

makeLenses ''CounterData


type HandlerConstraint m = (MonadIO m, MonadReader CounterData m)

getCounter ::  HandlerConstraint m => m Int
getCounter = liftIO . readIORef =<< view counter


putCounter :: HandlerConstraint m => CounterAction -> m Int
putCounter action = do
  r <- liftIO . flip atomicModifyIORef' (doAction action) =<< view counter
  return r
  where
    doAction (CounterAdd val) c = (c+val, c+val)
    doAction (CounterSet val) _ = (val, val)

counterHandlers :: ServerT CounterAPI (ReaderT CounterData Handler)
counterHandlers = getCounter :<|> putCounter

-- | We use servant's `enter` mechanism for handling Authentication ...
--   We throw an error if no secret was provided or if it was invalid - so our
--   handlers don't have to care about it.
toServant :: CounterData -> Maybe AuthToken -> ReaderT CounterData Handler a -> Handler a
toServant cVar (Just (VerySecret "topsecret")) m = runReaderT m cVar
toServant _ (Just (VerySecret secret)) _ = throwError $ err401 { errBody = "Your secret is valid not! - '" <> (B.fromStrict . T.encodeUtf8) secret <> "'!"  }
toServant _ _ _ = throwError $ err401 { errBody = "You have to provide a valid secret, which is topsecret!" }

counterServer :: CounterData -> Maybe AuthToken -> Server CounterAPI
counterServer cVar secret = hoistServer (Proxy :: Proxy CounterAPI) (toServant cVar secret) counterHandlers

fullServer :: FilePath -> CounterData -> Server FullAPI
fullServer wd cVar = counterServer cVar :<|> serveDirectoryFileServer (wd <> "/frontend/dist")

main :: IO ()
main = do
    cd <- CounterData <$> newIORef 0
    wd <- getCurrentDirectory
    putStrLn $ wd <> "/frontend/dist"
    run 8081 $ serve (Proxy @FullAPI) (fullServer wd cd)
