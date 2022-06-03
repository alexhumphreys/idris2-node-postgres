import Promise
import Postgres
import Debug.Trace

printStuff : (List (u ** (IdrisType u))) -> ()
printStuff [] = trace "end of row" ()
printStuff ((MkDPair Str snd) :: xs) =
  trace ("STR:" ++ snd) printStuff $ xs
printStuff ((MkDPair Num snd) :: xs) =
  trace ("Num:" ++ show snd) printStuff $ xs
printStuff ((MkDPair BigInt snd) :: xs) =
  trace ("BigInt:" ++ show snd) printStuff $ xs
printStuff ((MkDPair (Opt x) snd) :: xs) = ()

go : Maybe (List (List (u ** (IdrisType u)))) -> ()
go Nothing = trace ("empty list1") ()
go (Just []) = trace ("empty list2") ()
go (Just (x)) =
  let y = map printStuff x in
  trace (show y) ()

mainJS : Pool -> Promise String
mainJS pool = do
  q <- query pool "SELECT NOW()"
  lift $ go $ getAll q
  r <- query pool "SELECT address,headcount,technologies FROM educba"
  lift $ go $ getAll r
  pure "done"

main : IO ()
main = do
  pool <- getPool
  let prom = mainJS pool
  resolve prom (\x => putStrLn "Promise: \{x}") (\err => putStrLn ("Error: " ++ err))
  putStrLn "main: done"
