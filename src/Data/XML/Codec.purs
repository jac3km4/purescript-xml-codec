module Data.XML.Codec
  ( Codec
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
import Data.Either (Either, note, note')
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
  { read :: i -> Either String a
  , write :: a -> o
  }

instance Invariant (Codec i o) where
  imap fn fn' (Codec fa) = Codec
    { read: map fn <<< fa.read
    , write: fa.write <<< fn'
    }

instance Semigroup o => Semigroupal (Codec i o) where
  fproduct (Codec fa) (Codec fb) = Codec
    { read: \inp -> Tuple <$> fa.read inp <*> fb.read inp
    , write: \(Tuple a b) -> fa.write a <> fb.write b
    }

instance Monoid o => Monoidal (Codec i o) where
  funit = Codec { read: const $ pure unit, write: const mempty }

type NodeCodec = Codec Xml (Array NodeFragment)
type RecordCodec = Codec Xml (Array Node)

data NodeFragment
  = Attr String String
  | Content String

data Node = Node String (Array NodeFragment)

codec :: ∀ a. (Xml -> Either String a) -> (a -> NodeFragment) -> NodeCodec a
codec read write = Codec { read, write: pure <<< write }

content :: NodeCodec String
content = codec (note "Node has no content" <<< XML.text) Content

attr :: String -> NodeCodec String
attr name = codec (note "Node has no attribute" <<< XML.attr name) (Attr name)

tag :: ∀ a. String -> NodeCodec a -> RecordCodec a
tag name (Codec fa) = Codec
  { read: bindFlipped fa.read
      <<< note' (\_ -> "No node for tag " <> name)
      <<< Array.head
      <<< XML.directChildrenByTag name
  , write: pure <<< Node name <<< fa.write
  }

tags :: ∀ a. String -> NodeCodec a -> RecordCodec (Array a)
tags name (Codec fa) = Codec
  { read: traverse fa.read <<< XML.directChildrenByTag name
  , write: map (Node name <<< fa.write)
  }

int :: ∀ i o. Codec i o String -> Codec i o Int
int (Codec fa) = Codec
  { read: bindFlipped (note "Invalid int" <<< Int.fromString) <<< fa.read
  , write: fa.write <<< show
  }

decode :: ∀ a. RecordCodec a -> String -> Either String a
decode (Codec fa) str = fa.read =<< XML.parse str

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

rec
  :: ∀ row row' rl m
   . RL.RowToList row rl
  => SequenceRecord rl row row' m
  => Record row
  -> m (Record row')
rec = sequenceIR

recNewt
  :: ∀ row row' rl m nt
   . RL.RowToList row rl
  => SequenceRecord rl row row' m
  => Newtype nt (Record row')
  => Invariant m
  => Record row
  -> m nt
recNewt = imap wrap unwrap <<< sequenceIR
