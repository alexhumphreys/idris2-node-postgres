module PG.Postgres

import Data.Buffer
import Control.Monad.Trans
import Control.Monad.Either
import Control.Monad.Maybe
import Debug.Trace

import Data.List.Quantifiers
import Generics.Derive
import JSON

import Promise
-- import PG.Util
import Debug.Trace

%language ElabReflection

%default covering

-- For creating connections

public export
data Pool : Type where [external]

%foreign """
node:lambda: () => {
  const { Pool, Client } = require('pg')
  const pool = new Pool()
  //{
    //user: 'postgres',
    //host: '127.0.0.1',
    //database: 'foo',
    //password: 'mysecretpassword',
    //port: 5432,
  //}
  return pool
}
"""
prim__get_pool : PrimIO Pool

-- for querying
export
getPool : HasIO io => io Pool
getPool = primIO $ prim__get_pool

||| Result returned from a database query
public export
data Result : Type where [external]


{-
-- TODO no idea what that e parameter is doing in the foreign function call
-- but it doesn't work without it, there's `undefined` passed as the first param to this function
%foreign """
node:lambda:(e, pool, q) => {
  return pool.query({text: q, rowMode: 'array'}).then(res => {console.log(res); return res})
}
"""
prim__query : Pool -> String -> Promise e IO Result

public export
query : Pool -> String -> Promise e IO Result
query p s = prim__query p s
-}

%foreign """
node:lambda:(x) => {
  console.log(x)
}
"""
prim__print_result : Result -> Promise e IO ()

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
  Bool_   : Universe
  Str    : Universe
  Num    : Universe
  Text   : Universe
  BigInt : Universe
  Opt    : Universe -> Universe


||| Convert info from the results to a type we know about
raw_toUniverse :  (typeId : Bits32)
               -> (modifier : Int32)
               -> Maybe Universe
raw_toUniverse typeId modifier =
  case typeId of
       -- SELECT typname, oid, typarray FROM pg_type ORDER BY oid;
       16 => Just Bool_
       23 => Just Num
       25 => Just Text
       1043 => Just Str
       -- TIMESTAMPTZ: 1184
       x => trace "TypeID not found: \{show x}" Nothing

universeAt : Bits32 -> Result -> Maybe Universe
universeAt n r = raw_toUniverse (prim__dataTypeIdAt n r) (prim__dataTypeModifier r)

public export
IdrisType : Universe -> Type
IdrisType Bool_   = Bool
IdrisType Str     = String
IdrisType Num     = Double
IdrisType Text    = String
IdrisType BigInt  = Integer
IdrisType (Opt x) = Maybe (IdrisType x)

-- this is just an HList
public export
data Row : List Type -> Type where
  Nil  : Row []
  (::) : (v : t) -> (vs : Row ts) -> Row (t :: ts)

public export
0 RowTypes : List Universe -> List Type
RowTypes []        = []
RowTypes (u :: us) = IdrisType u :: RowTypes us

public export
0 RowU : List Universe -> Type
RowU = Row . RowTypes

public export
0 Table : List Universe -> Type
Table = List . RowU

-- Convert a raw pointer to a value matching of the
-- matching type (return Maybe or Either if this might fail)
marshall : AnyPtr -> (u : Universe) -> Maybe $ IdrisType u
marshall x Bool_ = Just $ believe_me x
marshall x Str = Just $ believe_me x
marshall x Num = Just $ believe_me x
marshall x Text = Just $ believe_me x
marshall x BigInt = Just $ believe_me x
marshall x (Opt y) = trace "foo2" Nothing

||| returns list of the types of the columns of the result row
getTypeOfColumns : (Result) -> Maybe $ List Universe
getTypeOfColumns r =
  let n = prim__columnCount r in
  case trace "n:\{show n}" n of
       0 => Just []
       n => traverse ((flip universeAt) r) [ 0 .. n-1 ]

parseRow : (us : List Universe) -> (Result) -> Bits32 -> Maybe (RowU us)
parseRow xs r count =
  let columnsCount = length xs
      rowAt = prim__valueAtAt r (count)
      row = map rowAt [0 .. (the Bits32 (cast $ columnsCount)-1)]
  in do
  go xs row
where
  go : (us : List Universe) -> List AnyPtr -> Maybe (RowU us)
  go [] [] = Just $ []
  go (u :: xs) (p :: ys) =
    let v = marshall p u in
    do
    Just $ !v :: !(go xs ys)
  go [] (x :: ys) = trace "foo4" Nothing
  go (x :: xs) [] = trace "foo5" Nothing

public export
getAll : (r : Result) -> Maybe (us ** Table us)
getAll r = do
  us <- getTypeOfColumns r
  let rowCount = prim__rowCount r
  let parse = parseRow us r
  {-
  parsed <- traverse parse [0 .. rowCount-1]
  pure $ (us ** parsed)
  -}
  case rowCount of
       0 => do
         trace ("count 0") pure (us ** [])
       x => do
         parsed <- trace "count \{show x}" $ traverse parse [0 .. rowCount-1]
         pure $ (us ** parsed)

data Values : Type where [external]

%foreign "node:lambda: () => []"
prim__emptyValues : Values

%foreign "node:lambda: (v,vs) => [v, ...vs]"
prim__cons : AnyPtr -> Values -> Values

convert : (us : List Universe) -> RowU us -> Values
convert [] [] = prim__emptyValues
convert (x :: xs) (v :: vs) = prim__cons (believe_me v) (convert xs vs)

%foreign """
node:lambda:(e, pool, q, ar) => {
  return pool.query({text: q, values: ar, rowMode: 'array'}).then(res => {console.log(res); return res})
}
"""
prim__query' : Pool -> String -> Values -> Promise e IO Result

public export
query' : Pool -> String -> (us : List Universe) -> RowU us -> Promise e IO Result
query' p s us vs = prim__query' p s $ convert us vs

%foreign """
node:lambda: () => {
  const { Pool, Client } = require('pg')
  const pool = new Pool(
  {
    user: 'postgres',
    host: '127.0.0.1',
    database: 'foo',
    password: 'admin',
    port: 5432,
  }
  )
  return pool
}
"""
prim__get_pool_ : PrimIO Pool

-- for querying
export
getPool_ : HasIO io => io Pool
getPool_ = primIO $ prim__get_pool

%foreign """
node:lambda:(e, pool, q, resolve, reject) => {
  return pool.query({text: q, rowMode: 'array'})
    .then(res => {
      console.log(res);
      resolve(res)();
    })
    .catch(err => {
      console.log(err);
      reject(err)();}
    );
}
"""
prim__query : Pool -> String -> (Result -> PrimIO ()) -> (e -> PrimIO ()) -> PrimIO ()

covering
public export
query : Pool -> String -> Promise e IO Result
query p s = promise $ \resolve', reject' => primIO $ prim__query p s (\r => trace "here" $ toPrim $ resolve' r) (\e => trace "there" $ toPrim $ reject' e)

public export
record ParticipantStock where
  constructor MkParticipantStock
  id : Int
  participantId : Int
  stockId : Int
  amount : Int

%runElab derive "ParticipantStock" [Generic, Meta, Show, Eq, RecordToJSON, RecordFromJSON]

export
fetchPart : Pool -> Int -> Promise String IO (ParticipantStock)
fetchPart pool i = do
  res <- query pool "SELECT * FROM participantStocks"
  Just part <- succeed $ getPart res| Nothing => reject $ "couldn't parse game \{show i}"
  pure part
where
  getPart : Result -> Maybe (ParticipantStock)
  getPart x = head' !(try partFromRow $ !(getAll x))
    where
      try : ((us : List Universe) -> (RowU us) -> Maybe z)
          -> (us : List Universe ** List (Row (RowTypes us))) -> Maybe (List z)
      try f (fst ** []) = Just []
      try f (fst ** (y :: xs)) = Just $ !(f fst y) :: !(try f (fst ** xs))
      partFromRow : (us : List Universe) -> (RowU us) -> Maybe ParticipantStock
      partFromRow ([Num, Num, Num, Num]) ([x, y, z, w]) = Just $ MkParticipantStock (cast x) (cast y)(cast z) (cast w)
      partFromRow _ _ = Nothing

foo : Result -> String
foo x =
  case getAll x of
       Nothing => "was nothing"
       (Just (([] ** snd))) => "was empty list"
       (Just (((Bool_ :: xs) ** snd))) => "was type_5"
       (Just (((Str :: xs) ** snd))) => "was type_6"
       (Just (((Num :: xs) ** snd))) => "was type_7"
       (Just (((Text :: xs) ** snd))) => "was type_8"
       (Just (((BigInt :: xs) ** snd))) => "was type_9"
       (Just ((((Opt y) :: xs) ** snd))) => "was type_10"

covering
mainJS : Pool -> Promise String IO ()
mainJS pool = do
  putStrLn "here2"
  {-
  r <- query pool "SELECT * FROM users"
  let output = foo r
  putStrLn output
  -}
  r <- fetchPart pool 1
  putStrLn $ show r
  pure ()

covering
main : IO ()
main = do
  pool <- getPool_
  putStrLn "here"
  runPromise {e=String} (\x => trace "main1" $ pure ()) (\_ => trace "main2" $ pure ()) $ mainJS pool
  pure () -- resolve prom (\x => putStrLn "Promise: \{show x}") (\err => putStrLn ("Error: " ++ err))
