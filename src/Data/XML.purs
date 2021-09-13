module Data.XML
  ( Xml
  , parse
  , stringify
  , node
  , text
  , attr
  , childrenByTag
  , directChildrenByTag
  -- unsafe functions
  , createElement
  , setAttribute
  , setTextContent
  , appendChild
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect, foreachE)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)

foreign import _parse :: ∀ a. Fn3 (String -> a) (Xml -> a) String a
foreign import _stringify :: Fn1 Xml String

foreign import _getAttribute :: Fn2 String Xml (Nullable String)
foreign import _getContent :: Fn1 Xml (Nullable String)
foreign import _getElementsByTag :: Fn2 String Xml (Array Xml)
foreign import _getChildrenByTag :: Fn2 String Xml (Array Xml)

foreign import _createElement :: EffectFn1 String Xml
foreign import _appendChild :: EffectFn2 Xml Xml Unit
foreign import _setAttribute :: EffectFn3 String String Xml Unit
foreign import _setTextContent :: EffectFn2 String Xml Unit

foreign import data Xml :: Type

instance Show Xml where
  show = stringify

parse :: String -> Either String Xml
parse = runFn3 _parse Left Right

stringify :: Xml -> String
stringify = _stringify

text :: Xml -> Maybe String
text = toMaybe <<< runFn1 _getContent

attr :: String -> Xml -> Maybe String
attr name = toMaybe <<< runFn2 _getAttribute name

childrenByTag :: String -> Xml -> Array Xml
childrenByTag name = runFn2 _getElementsByTag name

directChildrenByTag :: String -> Xml -> Array Xml
directChildrenByTag name = runFn2 _getElementsByTag name

node :: String -> Array Xml -> Xml
node name docs = unsafePerformEffect do
  el <- createElement name
  foreachE docs (flip appendChild el)
  pure el

createElement :: String -> Effect Xml
createElement = runEffectFn1 _createElement

setAttribute :: String -> String -> Xml -> Effect Unit
setAttribute name = runEffectFn3 _setAttribute name

setTextContent :: String -> Xml -> Effect Unit
setTextContent content = runEffectFn2 _setTextContent content

appendChild :: Xml -> Xml -> Effect Unit
appendChild child target = runEffectFn2 _appendChild child target
