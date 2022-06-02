import Promise
import Util

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

query : Pool -> String -> Promise String
query p s = promisify $ prim__query p s

mainJS : Pool -> Promise String
mainJS pool = do
  q <- query pool "SELECT NOW()"
  r <- query pool "SELECT NOW()"
  pure "res: \{q} \{r}"

main : IO ()
main = do
  pool <- getPool
  let prom = mainJS pool
  resolve prom (\x => putStrLn x) (\err => putStrLn ("Error: " ++ err))
  putStrLn "done"
