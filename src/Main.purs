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
--import Data.Either (either, Either)
import Data.Either
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import Node.FS (FS)
import Node.FS.Sync (readdir)
import Partial.Unsafe (unsafePartial)
import React.DOM (text, li', ul', input, button)
import Unsafe.Coerce (unsafeCoerce)
import Data.Tuple
import Data.Lens
import Data.Traversable

type InputState = String

data InputAction = SetEditText String

renderInput :: T.Render InputState _ Action
renderInput perform props state _ =
  let handleKeyPress :: Int -> String -> _
      handleKeyPress 13 text = perform $ pure $ Update text
--      handleKeyPress 27 _    = perform $ (Left SetEditText "")
      handleKeyPress _  _    = pure unit
  in
    [
      input [RP.placeholder "directory",
               RP.value state,
               RP.onChange \e -> perform (Left (SetEditText (unsafeCoerce e).target.value)),
               RP.onKeyUp \e -> handleKeyPress (unsafeCoerce e).keyCode (unsafeCoerce e).target.value
               ] []
    ]

performInputAction :: forall e. T.PerformAction _ InputState _ Action
performInputAction (Left (SetEditText s))           _ _ = void do
  T.cotransform \state -> s
performInputAction _                     _ _ = pure unit


inputSpec :: T.Spec _ InputState _ Action
inputSpec = T.simpleSpec performInputAction renderInput

type FilesState = Array String

data FilesAction = Update String

type State = Tuple InputState FilesState

type Action = Either InputAction FilesAction

renderFiles :: T.Render State _ FilesAction
renderFiles perform props state _ =
      [
      button [RP.onClick \_ -> perform (Update (fst state))
               ] [text "Update"],
      ul' (map (\file -> li' [text file]) (snd state))
    ]

performFilesAction :: forall e. T.PerformAction (fs :: FS| e) State _ FilesAction
performFilesAction (Update dir)           a b = void do
  filenames <- lift ( liftEff (either (const []) id <$> try (readdir dir)))
  T.cotransform \(Tuple x y) -> (Tuple x filenames)
--   filenames <- lift ( liftEff (either (const []) id <$> try (readdir dir)))
--   void $ T.cotransform _ (Tuple dir filenames)


filesSpec :: T.Spec _ State _ FilesAction
filesSpec = T.simpleSpec performFilesAction renderFiles



komponent :: T.Spec _ State _ Action
komponent = T.focus _1 id inputSpec <> T.focus id _Right filesSpec

main :: Eff (fs :: FS, dom :: DOM) Unit
main = void do
  let dir = "/home/markus"
  fileNames <- either (const []) id <$> try (readdir dir)
  let state = (Tuple  dir fileNames)
  let component = T.createClass komponent state
  document <- DOM.window >>= DOM.document
  container <-
    unsafePartial
    (fromJust <<< toMaybe
    <$> DOM.querySelector "#container"
    (DOM.htmlDocumentToParentNode document))
  RDOM.render
    (R.createFactory component (state))
    container
