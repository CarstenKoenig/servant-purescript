-- | TODO: This example could use a rewrite ;-)
module Main where

import Effect (Effect)
import Effect.Aff
import Control.Monad.Except.Trans
import Control.Monad.Reader.Trans
import Counter.ServerTypes
import Counter.WebAPI
import Data.Either
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe
import Prelude
import Servant.PureScript.Ajax
import Servant.PureScript.Settings
import Data.Array as Array
import Control.Bind ((<=<))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Foldable (foldr, fold, traverse_)
import Data.List (List(Nil, Cons))
import Data.Tuple (Tuple(Tuple))
import Foreign.Generic (defaultOptions)
import Pux (start, EffModel, noEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, span, div, p)
import Text.Smolder.Markup ((#!), text)

data Action = Increment
            | Decrement
            | Update Int
            | ReportError AjaxError
            | SubscriberLog String
            | Nop

type State = {
    counter :: Int
  , lastError :: Maybe AjaxError
  , settings :: MySettings
  , subscriberLog :: List String
  }

type MySettings = SPSettings_ SPParams_

type APIEffect a = ReaderT MySettings (ExceptT AjaxError Aff) a

foldp :: Action -> State -> EffModel State Action
foldp Increment state = runEffectActions state [Update <$> putCounter (CounterAdd 1)]
foldp Decrement state = runEffectActions state [Update <$> putCounter (CounterAdd (-1))]
foldp (Update val) state  = noEffects state { counter = val }
foldp (ReportError err ) state = noEffects state { lastError = Just err }
foldp (SubscriberLog msg) state = noEffects state { subscriberLog = Cons msg state.subscriberLog }
foldp Nop state = noEffects state

view :: State -> HTML Action
view state =
  div do
    div do
      button #! onClick (const Increment) $ text "+"
      span $ text (show state.counter)
      button #! onClick (const Decrement) $text "-"
    div do
      span $ text $ "Error: " <> maybe "Nothing" errorToString state.lastError
      div do
        text "Log: "
        div do
          (traverse_ (p <<< text) $ state.subscriberLog)

runEffectActions :: State -> Array (APIEffect Action) -> EffModel State Action
runEffectActions state effects = { state : state, effects : map (runEffect state.settings) effects }

runEffect :: MySettings -> APIEffect Action -> Aff (Maybe Action)
runEffect settings m = do
    er <- runExceptT $ runReaderT m settings
    case er of
      Left err -> pure $ Just $ ReportError err
      Right v -> pure $ Just v

main :: Effect Unit
main = do
  let settings = defaultSettings $ SPParams_ { authToken : VerySecret "topsecret"
                                             , baseURL : "http://localhost:8081/"
                                             }
  let initState = { counter : 0, settings : settings, lastError : Nothing, subscriberLog: Nil }
  app <- start
    { initialState: initState
    , foldp: foldp
    , view: view
    , inputs: []
    }
  renderToDOM "#app" app.markup app.input