module Main where

import Prelude
import React as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Control.Monad.Aff (liftEff')
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (try)
import Control.Monad.Trans (lift)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.HTMLAnchorElement (download)
import DOM.HTML.HTMLInputElement (files)
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Either (either)
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import Node.FS (FS)
import Node.FS.Sync (readdir)
import Partial.Unsafe (unsafePartial)
import React.DOM (text, li', ul', input)
import Unsafe.Coerce (unsafeCoerce)

type State = {dir :: String, names :: Array String}

data Action
  = SetEditText String |
  UpdateFiles String | Update

render :: T.Render State _ Action
render perform props state _ =
  let handleKeyPress :: Int -> String -> _
      handleKeyPress 13 text = perform $ UpdateFiles text
      handleKeyPress 27 _    = perform $ SetEditText ""
      handleKeyPress _  _    = pure unit
  in
    [
      input [RP.placeholder "directory",
               RP.value state.dir,
               RP.onChange \e -> perform (SetEditText (unsafeCoerce e).target.value),
               RP.onKeyUp \e -> handleKeyPress (unsafeCoerce e).keyCode (unsafeCoerce e).target.value
               ] [],
      ul' (map (\file -> li' [text file, input [RP.value "test", RP.onClick (\_ -> performAction Update) ] []]) props.names)
    ]


performAction :: forall e. T.PerformAction (fs :: FS, console :: CONSOLE | e) State _ Action
performAction (SetEditText s)           _ _ = void do
  T.cotransform $ _ { dir = s }
performAction (Update)                  _ _ = void do
  T.cotransform $ _ { dir = "" }
performAction (UpdateFiles s)           _ _ = do
   filenames <- lift ( liftEff (either (const []) id <$> try (readdir s)))
   lift (liftEff $ log s)
   lift (liftEff $ log (show filenames))
   void $ T.cotransform $ _ { names = filenames }


dirListingComponent :: T.Spec _ State _ Action
dirListingComponent = T.simpleSpec performAction render

main :: Eff (fs :: FS, dom :: DOM) Unit
main = void do
  let dir = "/home/markus"
  fileNames <- either (const []) id <$> try (readdir dir)
  let state = {dir: dir, names: fileNames}
  let component = T.createClass dirListingComponent state
  document <- DOM.window >>= DOM.document
  container <-
    unsafePartial
    (fromJust <<< toMaybe
    <$> DOM.querySelector "body"
    (DOM.htmlDocumentToParentNode document))
  RDOM.render
    (R.createFactory component (state))
    container
