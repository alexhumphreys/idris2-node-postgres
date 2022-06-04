import PG.Promise
import PG.Postgres
import Debug.Trace

data Country =
  MkCountry String Nat
Show Country where
  show (MkCountry x k) = "MkCountry \{x} \{show k}"

countryFromRow : (us : List Universe) -> (RowU us) -> Maybe Country
countryFromRow ([Str, Num]) ([v, x]) = Just $ MkCountry v (cast x)
countryFromRow (x :: _) _ = Nothing
countryFromRow ([]) _ = Nothing

debug1 : (List (u ** (IdrisType u))) -> ()
debug1 [] = trace "end of row" ()
debug1 ((MkDPair Bool_ snd) :: xs) =
  trace ("Bool:" ++ show snd) debug1 $ xs
debug1 ((MkDPair Str snd) :: xs) =
  trace ("STR:" ++ snd) debug1 $ xs
debug1 ((MkDPair Num snd) :: xs) =
  trace ("Num:" ++ show snd) debug1 $ xs
debug1 ((MkDPair Text snd) :: xs) =
  trace ("Text:" ++ show snd) debug1 $ xs
debug1 ((MkDPair BigInt snd) :: xs) =
  trace ("BigInt:" ++ show snd) debug1 $ xs
debug1 ((MkDPair (Opt x) snd) :: xs) = ()

go1 : Maybe (List (List (u ** (IdrisType u)))) -> ()
go1 Nothing = trace ("empty list1") ()
go1 (Just []) = trace ("empty list2") ()
go1 (Just (x)) =
  let y = map debug1 x in
  trace (show y) ()

debug2 : (us : List Universe) -> (RowU us) -> ()
debug2 [] [] = trace ("empty row") ()
debug2 (Bool_ :: xs) (v :: vs) =
  trace ("Bool: \{show v}") $ debug2 xs vs
debug2 (Str :: xs) (v :: vs) =
  trace ("String: \{v}") $ debug2 xs vs
debug2 (Num :: xs) (v :: vs) =
  trace ("Num: \{show v}") $ debug2 xs vs
debug2 (Text :: xs) (v :: vs) =
  trace ("Text: \{show v}") $ debug2 xs vs
debug2 (BigInt :: xs) (v :: vs) =
  trace ("BigInt: \{show v}") $ debug2 xs vs
debug2 ((Opt x) :: xs) (v :: vs) =
  trace ("ignoring Opt for now") $ debug2 xs vs

go2 : Maybe (us ** Table us) -> ()
go2 Nothing = ?go3_rhs_0
go2 (Just (MkDPair fst snd)) =
  let z = map (debug2 fst) snd
  in
  trace (show z) ()

tryCountry : Maybe (us ** Table us) -> Maybe (List Country)
tryCountry Nothing = Nothing
tryCountry (Just (MkDPair fst snd)) = traverse (countryFromRow fst) snd

mainJS : Pool -> Promise String IO ()
mainJS pool = do
  -- q <- query pool "SELECT NOW()" -- fails because time isn't implimented
  -- lift $ go3 $ getAll q
  r <- query pool "SELECT address,headcount,technologies FROM educba"
  lift $ go2 $ getAll r
  _ <- query pool "INSERT INTO board(country,total) VALUES ('germany',10),('france',10);"
  b <- query pool "SELECT country,total FROM board"
  countries <- lift $ getAll b
  ls <- lift $ tryCountry countries
  lift $ trace ("Board: \{show ls}") ()
  pure ()

main : IO ()
main = do
  pool <- getPool
  let prom = mainJS pool
  resolve prom (\x => putStrLn "Promise: \{show x}") (\err => putStrLn ("Error: " ++ err))
  putStrLn "main: done"
