import Promise
import Util
import Debug.Trace

%default total

-- For creating connections

data Pool : Type where [external]

%foreign """
node:lambda: () => {
  const { Pool, Client } = require('pg')
  // pools will use environment variables
  // for connection information
  const pool = new Pool({
    user: 'postgres',
    host: '127.0.0.1',
    database: 'foo',
    password: 'mysecretpassword',
    port: 5432,
  })
  return pool
}
"""
prim__get_pool : PrimIO Pool

-- for querying
getPool : IO Pool
getPool = primIO $ prim__get_pool

||| Result returned from a database query
public export
data Result : Type where [external]

%foreign promisifyPrim """
(pool, q) => {
  return pool.query({text: q, rowMode: 'array'}).then(res => {console.log(res); return res})
}
"""
prim__query : Pool -> String -> promise Result

query : Pool -> String -> Promise Result
query p s = promisify $ prim__query p s


-- JS syntax has not been verified
%foreign "node:lambda:x=>{console.log('count:'+x.rowCount);return x.rowCount}"
prim__rowCount : Result -> Bits32

-- JS syntax has not been verified
%foreign "node:lambda:(x)=>{return x.fields.length}"
prim__columnCount : Result -> Bits32

-- JS syntax has not been verified
%foreign "node:lambda:x=>x.fields[0].dataTypeID"
prim__dataTypeId : Result -> Bits32

-- JS syntax has not been verified
%foreign "node:lambda:(x,y)=>{return y.fields[x].dataTypeID}"
prim__dataTypeIdAt : Bits32 -> Result -> Bits32

-- JS syntax has not been verified
%foreign "node:lambda:x=>x.fields[0].dataTypeModifier"
prim__dataTypeModifier : Result -> Int32

-- JS syntax has not been verified
%foreign "node:lambda:(x,y)=>y.fields[x].dataTypeModifier"
prim__dataTypeModifierAt : Result -> Int32

-- JS syntax has not been verified
%foreign "node:lambda:(r,x,y)=>{console.log('r:'+r);console.log('x:'+x);console.log('y:'+y);return r.rows[x][y]}"
prim__valueAtAt : Result -> Bits32 -> Bits32 -> AnyPtr

||| A universeof supported types
public export
data Universe : Type where
  Str    : Universe
  Num    : Universe
  BigInt : Universe
  Opt    : Universe -> Universe


||| Convert info from the results to a type we know about
raw_toUniverse :  (typeId : Bits32)
               -> (modifier : Int32)
               -> Maybe Universe
raw_toUniverse typeId modifier =
  case typeId of
       -- SELECT typname, oid, typarray FROM pg_type ORDER BY oid;
       23 => Just Num
       1043 => Just Str
       -- TIMESTAMPTZ: 1184
       x => trace "TypeID not found: \{show x}" Nothing

universeAt : Bits32 -> Result -> Maybe Universe
universeAt n r = raw_toUniverse (prim__dataTypeIdAt n r) (prim__dataTypeModifier r)

public export
IdrisType : Universe -> Type
IdrisType Str     = String
IdrisType Num     = Double
IdrisType BigInt  = Integer
IdrisType (Opt x) = Maybe (IdrisType x)

-- Convert a raw pointer to a value matching of the
-- matching type (return Maybe or Either if this might fail)
marshall : AnyPtr -> (u : Universe) -> Maybe $ IdrisType u
marshall x Str = Just $ believe_me x
marshall x Num = Just $ believe_me x
marshall x BigInt = Just $ believe_me x
marshall x (Opt y) = trace "foo2" Nothing

-- Alex attempt

getTypeOfColumns : (Result) -> Maybe $ List Universe
getTypeOfColumns r =
  let n = prim__columnCount r in
  case trace "n:\{show n}" n of
       0 => Just []
       n => traverse ((flip universeAt) r) [ 0 .. n-1 ]

parseRow : List Universe -> (Result) -> Bits32 -> Maybe (List (u ** (IdrisType u)))
parseRow xs r count =
  let columnsCount = length xs
      rowAt = prim__valueAtAt r (count)
      row = map rowAt [0 .. (the Bits32 (cast $ columnsCount)-1)]
  in do
  go xs row
where
  go : List Universe -> List AnyPtr -> Maybe (List (u ** (IdrisType u)))
  go [] [] = Just []
  go (u :: xs) (p :: ys) =
    let v = marshall p u in
    do
    Just $ (u ** !v) :: !(go xs ys)
  go [] (x :: ys) = trace "foo4" Nothing
  go (x :: xs) [] = trace "foo5" Nothing


getAll : (r : Result) -> Maybe (List (List (u ** (IdrisType u))))
getAll r = do
  us <- getTypeOfColumns r
  let rowCount = prim__rowCount r
  traverse (parseRow us r) [0 .. rowCount-1]
