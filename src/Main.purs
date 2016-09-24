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
import React.DOM (text, li', ul', input, button)
import Unsafe.Coerce (unsafeCoerce)
import Data.Int (toNumber)

type State = {a :: Int, b :: Int, dir :: String, names :: Array String}

data Action
  = SetEditText String |
  UpdateFiles String | Update | IncrementA | IncrementB

render :: T.Render State _ Action
render perform props state _ =
  let handleKeyPress :: Int -> String -> _
      handleKeyPress 13 text = perform $ UpdateFiles text
      handleKeyPress 27 _    = perform $ SetEditText ""
      handleKeyPress _  _    = pure unit
  in
    [
      button [ RP.className "btn btn-success"
                   , RP.onClick \_ -> perform IncrementB
                   ]
                   [ text (show (toNumber props.b))]
        , button [ RP.className "btn btn-danger"
                   , RP.onClick \_ -> perform IncrementA
                   ]
                   [ text (show (toNumber props.a)) ]
    ]


performAction :: forall e. T.PerformAction (fs :: FS, console :: CONSOLE | e) State _ Action
performAction IncrementA                _ _ = void do
  T.cotransform $ _ { a = 20 }
performAction IncrementB                _ _ = void do
  T.cotransform $ _ { b = 20 }
performAction (SetEditText s)           _ _ = void do
  T.cotransform $ _ { dir = s }
performAction (Update)                  _ _ = void do
  T.cotransform $ _ { dir = "" }
performAction (UpdateFiles s)           _ p = do
   lift (liftEff $ log (show p.names))
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
  let state = {dir: dir, names: fileNames, a: 0 , b :0}
  let component = T.createClass dirListingComponent state
  document <- DOM.window >>= DOM.document
  container <-
    unsafePartial
    (fromJust <<< toMaybe
    <$> DOM.querySelector "#container"
    (DOM.htmlDocumentToParentNode document))
  RDOM.render
    (R.createFactory component (state))
    container
