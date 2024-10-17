{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module XHR (start) where

-- slightly adapted from https://github.com/dmjio/miso/blob/master/examples/xhr/Main.hs

#ifdef wasi_HOST_OS
import GHC.Wasm.Prim
#else
import Data.JSString (JSString)
import Language.Javascript.JSaddle (fromJSString, toJSString)
#endif

import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Map               as M
import           Data.Maybe
import qualified Data.Text              as T
import           GHC.Generics

import           Miso                   hiding (defaultOptions)
import           Miso.String

-- | Model
data Model
  = Model
  { info :: Maybe APIInfo
  } deriving (Eq, Show)

-- | Action
data Action
  = FetchGitHub
  | SetGitHub APIInfo
  | NoOp
  deriving (Show, Eq)

-- | Main entry point
start :: JSM ()
start = do
  startApp App { model = Model Nothing
               , initialAction = NoOp
               , mountPoint = Nothing
               , ..
               }
    where
      update = updateModel
      events = defaultEvents
      subs   = []
      view   = viewModel
      logLevel = Off

-- | Update your model
updateModel :: Action -> Model -> Effect Action Model
updateModel FetchGitHub m = m <# do
  SetGitHub <$> getGitHubAPIInfo
updateModel (SetGitHub apiInfo) m =
  noEff m { info = Just apiInfo }
updateModel NoOp m = noEff m

-- | View function, with routing
viewModel :: Model -> View Action
viewModel Model {..} = view
  where
    view = div_ [ style_ $ M.fromList [
                  (pack "text-align", pack "center")
                , (pack "margin", pack "200px")
                ]
               ] [
        h1_ [class_ $ pack "title" ] [ text $ pack "Miso XHR Example" ]
      , button_ attrs [
          text $ pack "Fetch JSON from https://api.github.com via XHR"
          ]
      , case info of
          Nothing -> div_ [] [ text $ pack "No data" ]
          Just APIInfo{..} ->
            table_ [ class_ $ pack "table is-striped" ] [
              thead_ [] [
                tr_ [] [
                  th_ [] [ text $ pack "URLs"]
                ]
              ]
            , tbody_ [] [
                tr_ [] [ td_ [] [ text current_user_url ] ]
              , tr_ [] [ td_ [] [ text emojis_url ] ]
              , tr_ [] [ td_ [] [ text emails_url ] ]
              , tr_ [] [ td_ [] [ text events_url ] ]
              , tr_ [] [ td_ [] [ text gists_url ] ]
              , tr_ [] [ td_ [] [ text feeds_url ] ]
              , tr_ [] [ td_ [] [ text followers_url ] ]
              , tr_ [] [ td_ [] [ text following_url ] ]
              ]
            ]
          ]
      where
        attrs = [ onClick FetchGitHub
                , class_ $ pack "button is-large"
                ] ++ [ disabled_ True | isJust info ]

data APIInfo
  = APIInfo
  { current_user_url                     :: MisoString
  , current_user_authorizations_html_url :: MisoString
  , authorizations_url                   :: MisoString
  , code_search_url                      :: MisoString
  , commit_search_url                    :: MisoString
  , emails_url                           :: MisoString
  , emojis_url                           :: MisoString
  , events_url                           :: MisoString
  , feeds_url                            :: MisoString
  , followers_url                        :: MisoString
  , following_url                        :: MisoString
  , gists_url                            :: MisoString
  , hub_url                              :: MisoString
  , issue_search_url                     :: MisoString
  , issues_url                           :: MisoString
  , keys_url                             :: MisoString
  , notifications_url                    :: MisoString
  , organization_repositories_url        :: MisoString
  , organization_url                     :: MisoString
  , public_gists_url                     :: MisoString
  , rate_limit_url                       :: MisoString
  , repository_url                       :: MisoString
  , repository_search_url                :: MisoString
  , current_user_repositories_url        :: MisoString
  , starred_url                          :: MisoString
  , starred_gists_url                    :: MisoString
  , user_url                             :: MisoString
  , user_organizations_url               :: MisoString
  , user_repositories_url                :: MisoString
  , user_search_url                      :: MisoString
  } deriving (Show, Eq, Generic)

instance FromJSON APIInfo where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

getGitHubAPIInfo :: JSM APIInfo
getGitHubAPIInfo = do
  resp <- liftIO $
    T.pack . fromJSString <$> js_fetch (toJSString ("https://api.github.com" :: String))
  case eitherDecodeStrictText resp :: Either String APIInfo of
    Left s  -> error s
    Right j -> pure j

#ifdef wasi_HOST_OS
-- We use the WASM JS FFI here to access the more modern fetch API. If you want
-- your code to eg also work when compiling with non-cross GHC and using
-- jsaddle-warp, you can use fetch or XMLHttpRequest via JSaddle, for example
-- via ghcjs-dom, servant-jsaddle or servant-client-js.
foreign import javascript safe "const r = await fetch($1); return r.text();"
  js_fetch :: JSString -> IO JSString
#else
js_fetch :: JSString -> IO JSString
js_fetch = error "not implemented"
#endif
