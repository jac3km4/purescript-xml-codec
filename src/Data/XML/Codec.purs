module Data.XML.Codec
  ( Codec
  , Error(..)
  , History
  , NodeCodec
  , RecordCodec
  , NodeFragment
  , Node
  , codec
  , content
  , attr
  , tag
  , tags
  , int
  , decode
  , encode
  , rec
  , recNewt
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either, note')
import Data.Functor.Invariant (class Invariant, imap)
import Data.Functor.Monoidal (class Monoidal, class Semigroupal)
import Data.Int as Int
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.XML (Xml)
import Data.XML as XML
import Effect (foreachE)
import Effect.Unsafe (unsafePerformEffect)
import Prim.RowList as RL
import Record.Invariant (class SequenceRecord, sequenceIR)

newtype Codec i o a = Codec
  { read :: History -> i -> Either Error a
  , write :: a -> o
  }

instance Invariant (Codec i o) where
  imap fn fn' (Codec fa) = Codec
    { read: \hs -> map fn <<< fa.read hs
    , write: fa.write <<< fn'
    }

instance Semigroup o => Semigroupal (Codec i o) where
  fproduct (Codec fa) (Codec fb) = Codec
    { read: \hs inp -> Tuple <$> fa.read hs inp <*> fb.read hs inp
    , write: \(Tuple a b) -> fa.write a <> fb.write b
    }

instance Monoid o => Monoidal (Codec i o) where
  funit = Codec { read: const $ const $ pure unit, write: const mempty }

newtype Error = Error
  { history :: History
  , cause :: String
  }

derive newtype instance Eq Error
derive newtype instance Show Error

type History = Array String
type NodeCodec = Codec Xml (Array NodeFragment)
type RecordCodec = Codec Xml (Array Node)

data NodeFragment
  = Attr String String
  | Content String

data Node = Node String (Array NodeFragment)

-- | Create a codec from decode and encode functions
codec :: ∀ a. (History -> Xml -> Either Error a) -> (a -> NodeFragment) -> NodeCodec a
codec read write = Codec { read, write: pure <<< write }

-- | Retrieve text content of the current node
content :: NodeCodec String
content = codec (\hs -> note' (error hs) <<< XML.text) Content
  where
  error history _ = Error { history, cause: "Node content is missing" }

-- | Retrieve a named attribute of the current node
attr :: String -> NodeCodec String
attr name = codec (\hs -> note' (error (Array.snoc hs name)) <<< XML.attr name) (Attr name)
  where
  error history _ = Error { history, cause: "Attribute is missing" }

-- | Retrieve the first direct child by tag of the current node
tag :: ∀ a. String -> NodeCodec a -> RecordCodec a
tag name (Codec fa) = Codec
  { read: \hs ->
      bindFlipped (fa.read (Array.snoc hs name))
        <<< note' (error hs)
        <<< Array.head
        <<< XML.directChildrenByTag name
  , write: pure <<< Node name <<< fa.write
  }
  where
  error history _ = Error { history, cause: "No node found for tag " <> name }

-- | Retrieve all direct children by tag of the current node
tags :: ∀ a. String -> NodeCodec a -> RecordCodec (Array a)
tags name (Codec fa) = Codec
  { read
  , write: map (Node name <<< fa.write)
  }
  where
  read hs =
    let
      hs' = Array.snoc hs name
    in
      traverse (fa.read hs') <<< XML.directChildrenByTag name

-- | Parse output of the codec into an integer
int :: ∀ i o. Codec i o String -> Codec i o Int
int (Codec fa) = Codec
  { read: \hs -> bindFlipped (note' (error hs) <<< Int.fromString) <<< fa.read hs
  , write: fa.write <<< show
  }
  where
  error history _ = Error { history, cause: "Invalid int" }

-- | Parse XML string using a codec
decode :: ∀ a. RecordCodec a -> String -> Either Error a
decode (Codec fa) str = fa.read [] =<< lmap error (XML.parse str)
  where
  error cause = Error { history: [], cause }

-- | Encode a record into an array of XML nodes
encode :: ∀ a. RecordCodec a -> a -> Array Xml
encode (Codec fa) val = mkNode <$> fa.write val
  where
  mkNode (Node tagName frags) = unsafePerformEffect do
    el <- XML.createElement tagName
    foreachE frags \frag ->
      case frag of
        Attr name txt -> XML.setAttribute name txt el
        Content txt -> XML.setTextContent txt el
    pure el

-- | Turn a record of codecs into a codec of a record
rec
  :: ∀ row row' rl m
   . RL.RowToList row rl
  => SequenceRecord rl row row' m
  => Record row
  -> m (Record row')
rec = sequenceIR

-- | Turn a record of codecs into a codec of a record wrapped with a newtype
recNewt
  :: ∀ row row' rl m nt
   . RL.RowToList row rl
  => SequenceRecord rl row row' m
  => Newtype nt (Record row')
  => Record row
  -> m nt
recNewt = imap wrap unwrap <<< sequenceIR
