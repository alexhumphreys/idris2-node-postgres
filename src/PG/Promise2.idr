||| from https://github.com/idris-community/inigo
module PG.Promise

public export
data Promise : (e : Type) -> (Type -> Type) -> Type -> Type where
  MkPromise : ((a -> m ()) -> (e -> m ()) -> m ()) -> Promise e m a

export
Functor (Promise e m) where
  map f (MkPromise cmd) = MkPromise (\succ => \err => cmd (\x => succ (f x)) err)

mutual
  export
  Applicative (Promise e m) where
    pure x = MkPromise (\succ => \err => succ x)
    x <*> y = x >>= (\f => f <$> y)

  export
  Monad (Promise e m) where
    (MkPromise cmd) >>= f = MkPromise (\succ =>
                                        \err =>
                                                cmd (\x =>
                                                          let (MkPromise cmd_) = (f x)
                                                          in cmd_ succ err
                                                    ) err
                                      )

export
resolve : Promise e m a -> (a -> m ()) -> (e -> m ()) -> m ()
resolve (MkPromise cmd) ok err =
  cmd ok err

export
run : Show e => HasIO io => Promise e io a -> io ()
run p =
  resolve p (\_ => pure ()) (\err => putStrLn ("Error: " ++ show err))

-- I can fold these, but that's a bit of an issue since
-- they will end up running sequentially, which is really
-- not the intent here, but for now...
export
all : List (Promise e m a) -> Promise e m (List a)
all promises =
  doAll promises
  where
    doAll : List (Promise e m a) -> Promise e m (List a)
    doAll (p :: ps) =
      do
        x <- p
        rest <- doAll ps
        pure (x :: rest)
    doAll [] = pure []

export
lift : a -> Promise e m a
lift x = MkPromise (\ok => \err => ok x)

export
liftIO : IO a -> Promise e IO a
liftIO x = MkPromise (\ok => \err => x >>= ok)

export
parallel : Monad m => Promise e m a -> Promise e m a -> Promise e m a
parallel (MkPromise s1) (MkPromise s2) = MkPromise $ \err => \cb => do
  s1 err cb
  s2 err cb

public export
promise : Type -> Type -> Type
promise e a = (a -> IO ()) -> (e -> IO ()) -> PrimIO ()

export
promisify : promise e a -> Promise e IO a
promisify prim =
  MkPromise (\ok, err => primIO $ prim ok err)

export
boolToInt : Bool -> Int
boolToInt False = 0
boolToInt True = 1
