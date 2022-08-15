module PG.Main

import Data.Buffer
import Control.Monad.Trans
import Control.Monad.Either
import Control.Monad.Maybe
import Debug.Trace

import Data.List.Quantifiers
import Generics.Derive
import JSON

import Promise
import PG.Postgres
import Debug.Trace

%language ElabReflection

%default covering

record ParticipantStock where
  constructor MkParticipantStock
  id : Int
  participantId : Int
  stockId : Int
  amount : Int

%runElab derive "ParticipantStock" [Generic, Meta, Show, Eq, RecordToJSON, RecordFromJSON]

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
