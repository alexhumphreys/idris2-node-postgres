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
  const pool = new Pool()
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

debug : Maybe (DPair Universe (\u => List (IdrisType u))) -> ()
debug Nothing = ()
debug (Just (MkDPair Str snd)) = trace (show snd) ()
debug (Just (MkDPair Num snd)) = trace (show snd) ()
debug (Just (MkDPair BigInt snd)) = trace (show snd) ()
debug (Just (MkDPair (Opt x) snd)) = trace ("here") ()

mainJS : Pool -> Promise String
mainJS pool = do
  q <- query pool "SELECT NOW()"
  r <- query2 pool "SELECT headcount,address FROM educba"
  let x = fromResult r
  lift $ debug x
  pure "res: \{q}"

main : IO ()
main = do
  pool <- getPool
  let prom = mainJS pool
  resolve prom (\x => putStrLn x) (\err => putStrLn ("Error: " ++ err))
  putStrLn "done"
