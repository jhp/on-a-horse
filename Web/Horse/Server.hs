{-#LANGUAGE Arrows, RankNTypes, ScopedTypeVariables, OverloadedStrings, NoMonomorphismRestriction #-}

module Web.Horse.Server where

import Web.Horse.Forms
import Web.Cookie (parseCookiesText)
import Data.Time.Clock
import Data.Maybe
import Data.Monoid
import Data.List
import Data.CaseInsensitive (mk, original)
import Control.Monad.IO.Class
import Data.Function
import Network.Wai.Handler.Warp
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Parse
import Web.Horse.Forms.Types
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Text as T
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SBC
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import Control.Concurrent.MVar
import System.Random (randomIO)
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List.Split (splitOn)
import Control.Arrow.Transformer.Automaton
import Control.Arrow.Transformer.Automaton.Maybe
import Control.Arrow.Transformer.Automaton.Monad
import Control.Arrow.Transformer.Reader
import Control.Arrow.Transformer.LabeledArrow
import qualified Data.Map as M

type Url = [String]

runHorse f = runHorse1 g
    where
      g = runReader $ runLabeledArrow $ f 

runHorse1 :: MaybeAutomaton (Kleisli IO) (Url, FormIn) String -> IO Application
runHorse1 f = do
  mv <- newMVar []
  return $ runWeb mv f

sessionTarget = 150

runWeb
  :: MVar [(String, MVar (Automaton (Kleisli IO) (Url, FormIn) String))]
    -> MaybeAutomaton (Kleisli IO) (Url, FormIn) String
    -> Application
runWeb mv f0 req = do
   liftIO $ compact mv
   mv_sess <- liftIO $ getSessionMVar mv req
   inp <- extractFormInputs req
   case mv_sess of
     Just mv_sess -> liftIO $ modifyMVar mv_sess $ \sess -> do
                       (x,y) <- runKleisli (auto sess) inp
                       return (y, asResponse x [])
     Nothing -> liftIO $ do
       (resp,f') <- runKleisli (mAut f0) inp
       case f' of
         Nothing -> return (asResponse resp [])
         Just f' -> do
                     (newSess :: Int) <- abs <$> randomIO
                     var <- newMVar f'
                     modifyMVar_ mv $ return . ((show newSess,var) :)
                     let cookie = ("Set-Cookie", mconcat [sessionName, "=", show newSess, "; path=/"])
                     return (asResponse resp [cookie])
                     

compact mv = modifyMVar_ mv $ \lst ->
  case length lst > (2 * sessionTarget) of
    True -> return (take sessionTarget $ nubBy ((==) `on` fst) lst)
    False -> return lst

getSessionMVar
 :: MVar [(String, MVar (Automaton (Kleisli IO) (Url, FormIn) String))]
    -> Request
    -> IO (Maybe (MVar (Automaton (Kleisli IO) (Url, FormIn) String)))
getSessionMVar mv req = modifyMVar mv $ \lst -> do
  case lookup sessionName (getRequestCookies req) of
    Just sess -> case lookup sess lst of
                   Just val -> return ((sess,val):lst, Just val)
                   Nothing -> return (lst, Nothing)
    Nothing -> return (lst, Nothing)

sessionName :: [Char]
sessionName = "HaskellOnAHorse"

--extractFormInputs :: Request -> ResourceT IO (Url, FormIn)
extractFormInputs req = do
  let queryParams = map (\(x,y) -> (x,fromMaybe (SBC.pack "") y)) (queryString req)
  (params, _) <- parseRequestBody lbsBackEnd req
  return (map T.unpack (pathInfo req), FormIn $ (map (\(x,y) -> (SBC.unpack x, SBC.unpack y)) (queryParams ++ params)))

asResponse :: String -> [(String, String)] -> Response
asResponse out hdrs = responseLBS ok200 (map (\(x,y) -> (mk (SBC.pack x), SBC.pack y)) (typ:len:hdrs)) (fromString out)
    where
      typ = ("Content-Type", "text/html")
      len = ("Content-Length", show $ LB.length $ fromString out)


getRequestCookies :: Request -> [(String, String)]
getRequestCookies req = 
    map (\(x,y) -> (T.unpack x, T.unpack y)) 
            $ parseCookiesText 
                  $ fromMaybe "" (lookup "Cookie" (requestHeaders req))
