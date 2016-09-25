module Main where



{--
import React as R
import React.DOM.Props as RP
import ReactDOM as RDOM
import Control.Monad.Aff (liftEff')
import Control.Monad.Aff.Class (liftAff)

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)



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
-- import React.DOM
import Unsafe.Coerce (unsafeCoerce)
import Data.Int (toNumber)


--}

import Prelude

import Control.Monad.Eff.Exception (try, EXCEPTION)
import Control.Monad.Eff (Eff)

import Signal.Channel (CHANNEL)

import Pux (renderToDOM, fromSimple, start)
import Pux.Html (Html, text, button, span, div)
import Pux.Html.Events (onClick)

import Data.Int (toNumber)


data Action = IncrementA | IncrementB

type State = {a :: Int, b :: Int}


update :: Action -> State -> State
--update IncrementA state = {a = 20, b = 20}
update IncrementA state = state { a = state.a + 1}
update IncrementB state = state { b = state.b + 1}

view :: State -> Html Action
view state =
  div
    []
    [ button [ onClick (const IncrementA) ] [ text "IncrementA" ]
    , span [] [ text (show (toNumber state.a)) ]
    ,  span [] [ text (show (toNumber state.b)) ]
    , button [ onClick (const IncrementB) ] [ text "IncrementB" ]
    ]

main :: forall e. Eff (err :: EXCEPTION, channel :: CHANNEL | e) Unit
main = do
  let init = { a : 0 , b :  0 }
  app <- start
    { initialState: init
    , update: fromSimple update
    , view: view
    , inputs: []
    }

  renderToDOM "#container" app.html
