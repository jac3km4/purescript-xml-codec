module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (class Newtype)
import Data.XML as XML
import Data.XML.Codec as C
import Effect (Effect)
import Test.Assert (assertEqual)

newtype Employee = Employee
  { name :: String
  , job :: { position :: String, salary :: Int }
  }

derive instance Newtype Employee _
derive newtype instance Eq Employee
derive newtype instance Show Employee

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

main :: Effect Unit
main = do
  testOne
  testTwo

  where
  validEmployee = """<employee><job position="boss" salary="120"/><name>John</name></employee>"""
  invalidEmployee = """<employee><job position="boss" saraly="120"/><name>John</name></employee>"""

  testOne = do
    let (employee :: Either C.Error Employee) = C.decode employeeCodec validEmployee
    assertEqual { actual: employee, expected: Right $ Employee { name: "John", job: { position: "boss", salary: 120 } } }
    let xml = map (XML.stringify <<< XML.node "employee" <<< C.encode employeeCodec) employee
    assertEqual { actual: xml, expected: Right validEmployee }

  testTwo = do
    let (employee :: Either C.Error Employee) = C.decode employeeCodec invalidEmployee
    assertEqual { actual: employee, expected: Left (C.Error { history: [ "job", "salary" ], cause: "Attribute is missing" }) }
