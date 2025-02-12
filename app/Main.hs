{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}

#ifdef wasi_HOST_OS

module MyMain (main) where

import App
import GHC.Wasm.Prim
import Language.Javascript.JSaddle.Wasm qualified as JSaddle.Wasm

foreign export javascript "hs_start" main :: JSString -> IO ()

main :: JSString -> IO ()
main e = JSaddle.Wasm.run (start e).app

#else

module Main (main) where

import App
import Data.Text.Lazy qualified as T
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.Warp
import Network.Wai.Application.Static
import System.Environment

{- TODO
work out how to live-reload on changes to stylesheet
maybe don't use `dist` version...
maybe just a matter of passing right flag to GHCID?
in theory I shouldn't even need to do that - just force a page refresh
is that something we can hook in to?

open jsaddle PR and use it here as a SRP

punt:
somehow DRY to match static HTML files
-}
main :: IO ()
main =
    getArgs >>= \case
        -- Note that `debug` works with `cabal repl` but not `cabal run`.
        -- The best workflow is to run `ghcid -c "cabal repl ghc-wasm-miso-examples" -W -T ':main simplecounter'`.
        [arg] ->
            let app =
                    start
                        -- "2048"
                        (toJSString arg)
                -- we can't use multiline syntax alongside CPP...
                -- "<meta charset='utf-8'>\n\
                -- \<meta name='viewport' content='width=device-width, initial-scale=1'>\n\
                -- \<title>2048 | Miso example via GHC WASM</title>\n\
                -- \<link rel='stylesheet' href='2048/main.css'/>\n\
                -- \"
                header =
                    encodeUtf8 $
                        T.unlines $
                            [ "<title>" <> app.name <> "</title>"
                            ]
                                <> map
                                    (\s -> "<link rel='stylesheet' href='" <> s <> "'/>")
                                    app.stylesheets
             in
                -- can't work out how to get non-`debug` version working
                -- but I think I prefer this anyway
                 debugOr
                    (Just header)
                    8000
                    app.app
                    (staticApp (defaultWebAppSettings "frontend/dist"))
        _ -> fail "bad args: specify an example, e.g. 2048"

#endif
