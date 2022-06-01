module Main

import JS.Util

import Control.Monad.Trans

%hide JS.Util.prim__consoleLog

public export
record Callbacks e (m : Type -> Type) a where
  constructor MkCallbacks
  onSucceded : a -> m ()
  onFailed : e -> m ()

public export
record Promise e (m : Type -> Type) a where
  constructor MkPromise
  continuation : Callbacks e m a -> m ()

export
Functor (Promise e m) where
  map f (MkPromise ca) = MkPromise $ \cb => ca $ MkCallbacks (cb.onSucceded . f) cb.onFailed

mutual

  export
  Applicative (Promise e m) where
    pure a = MkPromise $ \cb => cb.onSucceded a
    fn <*> pa = fn >>= \f => map f pa

  export
  Monad (Promise e m) where
    (>>=) (MkPromise conta) f = MkPromise $ \cb => conta $ MkCallbacks
        { onSucceded = \a =>
            let MkPromise contb = f a
            in contb cb
        , onFailed = cb.onFailed
        }

export
MonadTrans (Promise e) where
  lift ma = MkPromise $ \cb => ma >>= cb.onSucceded

export
HasIO m => HasIO (Promise e m) where
  liftIO = lift . liftIO

export
fail : e -> Promise e m a
fail e = MkPromise $ \cb => cb.onFailed e

export
data DBPool : Type where [external]

data Pool : Type where [external]

data Connection : Type where [external]

data QueryPromise : Type where [external]

{-
%foreign "node:lambda: (server, port) => server.listen(port)"
ffi_listen : Server -> Int -> PrimIO ()

export
(.listen) : HasIO io => Server -> Int -> io ()
(.listen) server port = primIO $ ffi_listen server port
-}

{-
// promise
client
  .query('SELECT NOW() as now')
  .then(res => console.log(res.rows[0]))
  .catch(e => console.error(e.stack))
-}

%foreign "node:lambda: () => require('pg').Pool"
ffi_require_db_pool : () -> PrimIO DBPool

require_db_pool : IO DBPool
require_db_pool = primIO $ ffi_require_db_pool ()

%foreign "node:lambda: (Pool) => new Pool()"
ffi_new_pool : DBPool -> PrimIO Pool

newPool : DBPool -> IO Pool
newPool pool = primIO $ ffi_new_pool pool

%foreign "node:lambda: (db) => db.connect()"
ffi_connect : Pool -> PrimIO Connection

connect : Pool -> IO Connection
connect pool = primIO $ ffi_connect pool

%foreign "node:lambda: (x, f) => x.then(f)"
ffi_then : QueryPromise -> (String -> IO ()) -> PrimIO ()

then' : QueryPromise -> (String -> IO ()) -> IO ()
then' q f = primIO $ ffi_then q f

%foreign "node:lambda: (x, q) => x.query(q)"
ffi_query : a -> String -> PrimIO QueryPromise

query : Connection -> String -> IO QueryPromise
query conn str = primIO $ ffi_query conn str

%foreign "node:lambda:x=>console.log(x)"
prim__consoleLog : String -> PrimIO ()

%foreign """
node:lambda: () => {
  const { Pool, Client } = require('pg')
  // pools will use environment variables
  // for connection information
  const pool = new Pool()
  return pool
}
"""
prim__big : PrimIO Pool

bigPool : IO Pool
bigPool = primIO $ prim__big

%foreign """
node:lambda: (pool, q) => {
  return pool.query(q)
}
"""
prim__use_pool : Pool -> String -> PrimIO ()

usePool : Pool -> String -> IO ()
usePool p s = primIO $ prim__use_pool p s

%foreign """
node:lambda: (x, f) => {
  return x.then(f())
}
"""
prim__another_then : a -> (String -> IO ()) -> PrimIO ()

anotherThen : a -> (String -> IO ()) -> IO ()
anotherThen x f = primIO $ prim__another_then x f

consoleLog' : String -> IO ()
consoleLog' s = primIO $ prim__consoleLog s

mainJS : IO ()
mainJS = do
  pool <- bigPool
  q <- usePool pool "SELECT NOW()"
  anotherThen q (\cb => consoleLog'("here"))
  pure ()

main : IO ()
main = do
  mainJS
  putStrLn "hello world"
