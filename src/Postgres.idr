%default total

||| Result returned from a database query
public export
data Result : Type where [external]

-- JS syntax has not been verified
%foreign "node:lambda:x=>x.rowCount"
prim__rowCount : Result -> Bits32

-- JS syntax has not been verified
%foreign "node:lambda:x=>x.fields.length"
prim__columnCount : Result -> Bits32

-- JS syntax has not been verified
%foreign "node:lambda:x=>x.fields[0].dataTypeID"
prim__dataTypeId : Result -> Bits32

-- JS syntax has not been verified
%foreign "node:lambda:(x,y)=>{console.log(x);console.log(y.fields);y.fields[x].dataTypeID}"
prim__dataTypeIdAt : Bits32 -> Result -> Bits32

-- JS syntax has not been verified
%foreign "node:lambda:x=>x.fields[0].dataTypeModifier"
prim__dataTypeModifier : Result -> Int32

-- JS syntax has not been verified
%foreign "node:lambda:(x,y)=>y.fields[x].dataTypeModifier"
prim__dataTypeModifierAt : Result -> Int32

-- JS syntax has not been verified
%foreign "node:lambda:(x,y,r)=>{return r.rows[x][y]}"
prim__valueAtAt : Bits32 -> Bits32 -> Result -> AnyPtr

-- JS syntax has not been verified
%foreign "node:lambda:(x,y)=>{return y.rows[x]}"
prim__valueAt : Bits32 -> Result -> AnyPtr

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
       23 => Just Num
       1043 => Just Str
       _ => Nothing

universe : Result -> Maybe Universe
universe r = raw_toUniverse (prim__dataTypeId r) (prim__dataTypeModifier r)

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
marshall : AnyPtr -> (u : Universe) -> IdrisType u
marshall x Str = believe_me x
marshall x Num = believe_me x
marshall x BigInt = believe_me x
marshall x (Opt y) = ?kjkj

marshall' : AnyPtr -> (u : Universe) -> Maybe $ IdrisType u
marshall' x Str = Just $ believe_me x
marshall' x Num = Just $ believe_me x
marshall' x BigInt = Just $ believe_me x
marshall' x (Opt y) = Nothing

extractAt : Result -> (u : Universe) -> Bits32 -> IdrisType u
extractAt r u n = marshall (prim__valueAt n r) u

-- TODO the row isn't an IdrisType u, it's a (List $ IdrisType u)
-- can't look up universeAt using the rows, universe at is about the columns
extractAt' : Result -> Bits32 -> Maybe (u ** IdrisType u)
extractAt' r n = do
  x <- universeAt n r
  let foo = (prim__valueAt n r)
  y <- marshall' foo x
  Just (x ** y)

extractAll' : Result -> Maybe $ List (u ** IdrisType u)
extractAll' r =
  let n = prim__rowCount r in
  go r $ cast (n-1)
where
  go : Result -> Nat -> Maybe $ List (u ** IdrisType u)
  go x 0 = Just []
  go x (S k) =
    Just $ !(extractAt' r $ cast (S k)) :: !(go x (k))

extractAll : Result -> (u : Universe) -> List (IdrisType u)
extractAll r u = case prim__rowCount r of
  0 => []
  -- You may want to make this tail recursive if you expect
  -- large result sets.
  n => map (extractAt r u) [0 .. n-1]

||| Extract the rows from a result.
fromResult : (r : Result) -> Maybe (u ** List (IdrisType u))
fromResult r = case universe r of
  Just u  => Just (u ** extractAll r u)
  Nothing => Nothing

||| Extract the rows from a result.
fromResult' : (r : Result) -> Maybe $ List (u ** (IdrisType u))
fromResult' = extractAll'

-- Alex attempt

getColumnCount : (Result) -> Bits32
getColumnCount r = prim__columnCount r

getTypeOfColumns : (Result) -> Maybe $ List Universe
getTypeOfColumns r =
  let n = getColumnCount r in
  case n of
       0 => Just []
       n => traverse ((flip universeAt) r) [ 0 .. n-1 ]

parseRow : List Universe -> (Result) -> Bits32 -> Maybe (List (u ** (IdrisType u)))
parseRow xs r count =
  let columnsCount = length xs
      row = prim__valueAtAt (count-1) in
  do
  -- extractAt' : Result -> Bits32 -> Maybe (u ** IdrisType u)
  ?parseRow_rhs_1


getAll' : List Universe -> (r : Result) -> Maybe (List (List (u ** (IdrisType u))))
getAll' xs r =
  let rowCount = prim__rowCount r
  in do
  traverse (parseRow xs r) [0 .. rowCount-1]

getAll : (r : Result) -> Maybe (List (List (u ** (IdrisType u))))
getAll r = do
  ty <- getTypeOfColumns r
  getAll' ty r
