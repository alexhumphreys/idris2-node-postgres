%default total

||| Result returned from a database query
public export
data Result : Type where [external]

-- JS syntax has not been verified
%foreign "node:lambda:x=>x.rowCount"
prim__rowCount : Result -> Bits32

-- JS syntax has not been verified
%foreign "node:lambda:x=>x.fields[0].dataTypeID"
prim__dataTypeId : Result -> Bits32

-- JS syntax has not been verified
%foreign "node:lambda:x=>x.fields[0].dataTypeModifier"
prim__dataTypeModifier : Result -> Int32

-- JS syntax has not been verified
%foreign "node:lambda:(x,y)=>y.rows[x]"
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

public export
IdrisType : Universe -> Type
IdrisType Str     = String
IdrisType Num     = Double
IdrisType BigInt  = Integer
IdrisType (Opt x) = Maybe (IdrisType x)

-- Convert a raw pointer to a value matching of the
-- matching type (return Maybe or Either if this might fail)
marshall : AnyPtr -> (u : Universe) -> IdrisType u
marshall x Str = ?marshall_rhs_0
marshall x Num = ?marshall_rhs_1
marshall x BigInt = ?marshall_rhs_2
marshall x (Opt y) = ?marshall_rhs_3


extractAt : Result -> (u : Universe) -> Bits32 -> IdrisType u
extractAt r u n = marshall (prim__valueAt n r) u

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
