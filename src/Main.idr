import Promise
import Util
import Postgres
import Debug.Trace

data Pool : Type where [external]
data Query : Type where [external]

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

getPool : IO Pool
getPool = primIO $ prim__get_pool

%foreign promisifyPrim """
(pool, q) => {
  return pool.query({text: q, rowMode: 'array'}).then(res => {console.log(res); return res.command})
}
"""
prim__query : Pool -> String -> promise String

%foreign promisifyPrim """
(pool, q) => {
  return pool.query({text: q, rowMode: 'array'}).then(res => {console.log(res); return res})
}
"""
prim__query2 : Pool -> String -> promise Result

query : Pool -> String -> Promise String
query p s = promisify $ prim__query p s

query2 : Pool -> String -> Promise Result
query2 p s = promisify $ prim__query2 p s

debug2 : (List (u ** (IdrisType u))) -> ()
debug2 [] = trace "end of row" ()
debug2 ((MkDPair Str snd) :: xs) =
  trace ("STR:" ++ snd) debug2 $ xs
debug2 ((MkDPair Num snd) :: xs) =
  trace ("Num:" ++ show snd) debug2 $ xs
debug2 ((MkDPair BigInt snd) :: xs) =
  trace ("BigInt:" ++ show snd) debug2 $ xs
debug2 ((MkDPair (Opt x) snd) :: xs) = ()

go : Maybe (List (List (u ** (IdrisType u)))) -> ()
go Nothing = trace ("empty list1") ()
go (Just []) = trace ("empty list2") ()
go (Just (x)) =
  let y = map debug2 x in
  trace (show y) ()

debug' : Maybe (List (u ** (IdrisType u))) -> ()
debug' Nothing = ()
debug' (Just []) = ()
debug' (Just ((MkDPair Str snd) :: xs)) =
  trace ("STR:" ++ snd) debug' $ Just xs
debug' (Just ((MkDPair Num snd) :: xs)) =
  trace ("Num:" ++ show snd) debug' $ Just xs
debug' (Just ((MkDPair BigInt snd) :: xs)) =
  trace ("BigInt:" ++ show snd) debug' $ Just xs
debug' (Just ((MkDPair (Opt x) snd) :: xs)) = ()

debug : Maybe (DPair Universe (\u => List (IdrisType u))) -> ()
debug Nothing = ()
debug (Just (MkDPair Str [])) = trace ("STR") ()
debug (Just (MkDPair Str (x :: xs))) = trace ("STR->\{x}<-STR..." ++ show xs) ()
debug (Just (MkDPair Num [])) = trace ("Num" ++ ?jjj) ()
debug (Just (MkDPair Num (x :: xs))) = trace ("Num->\{show x}<-NUM..." ++ show xs) ()
debug (Just (MkDPair BigInt snd)) = trace ("BigInt" ++ show snd) ()
debug (Just (MkDPair (Opt x) snd)) = trace ("here") ()

mainJS : Pool -> Promise String
mainJS pool = do
  q <- query pool "SELECT NOW()"
  r <- query2 pool "SELECT address,headcount,technologies FROM educba"
  -- let x = fromResult' r
  -- lift $ debug' x
  let y = getAll r
  lift $ go y
  pure "res: \{q}"

main : IO ()
main = do
  pool <- getPool
  let prom = mainJS pool
  resolve prom (\x => putStrLn x) (\err => putStrLn ("Error: " ++ err))
  putStrLn "done"
