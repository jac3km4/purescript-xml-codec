
# purescript-xml-codec
A lightweight XML parser/encoder library.

## synopsis
This library offers a bidirectional codec API on top of the standard Web XML API.
It relies on `DOMParser` and `XMLSerializer` which are available by default in browser environments.
For usage in Node, you'll need to install a polyfill: `npm install xmldom`.

## usage
Basic example (from `test/Main.purs`):

```purs
newtype Employee = Employee
  { name :: String
  , job :: { position :: String, salary :: Int }
  }

derive instance Newtype Employee _
derive newtype instance Eq Employee
derive newtype instance Show Employee

-- codecs are defined by defining records that map every field to a codec
-- `rec` applies to records and `recNewt` applies to newtype-wrapped records
-- all codecs are Invariant Monoidals, offering generic combinators like `imap` and `fproduct`
-- this will parse XML input like: '<guy><job position="boss" salary="120"/><name>John</name></guy>'
employeeCodec :: C.RecordCodec Employee
employeeCodec = C.recNewt
  { name: C.tag "name" C.content
  , job: C.tag "job" positionCodec
  }
  where
  positionCodec = C.rec
    { position: C.attr "position"
    , salary: C.int $ C.attr "salary"
    }
```
