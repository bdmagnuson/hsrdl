
import Parser (Identifier, Array)
import qualified Data.Map.Strict as M

type ElemPath = [(Identifier, Maybe Array)]

data PropType =
     PropLitT
   | PropNumT
   | PropBoolT
   | PropRefT
   | PropPathT
   | PropEnumT EnumDef deriving (Show)

data PropRHS =
     PropLit  String
   | PropNum  Integer
   | PropBool Bool
   | PropRef  ElemPath Identifier
   | PropPath ElemPath
   | PropEnum EnumDef Identifier deriving (Show)

data Property = Property {
   ptype    :: PropType,
   pdefault :: Maybe PropRHS
} deriving (Show)

data PropDefs = PropDefs {
   field   :: M.Map Identifier Property,
   reg     :: M.Map Identifier Property,
   regfile :: M.Map Identifier Property,
   addrmap :: M.Map Identifier Property
} deriving (Show)

data EnumDef = EnumDef {
   name   :: String,
   values :: M.Map Identifier Integer
} deriving (Show)

accessType = EnumDef "access_type" (M.fromList [("rw", 0), ("wr", 1), ("r", 2), ("w", 3), ("na", 4)])

defFalse     = Property PropBoolT (Just (PropBool False))
defZero      = Property PropBoolT (Just (PropNum 0))
defOne       = Property PropBoolT (Just (PropNum 1))
defNothing a = Property a Nothing
defEnum a d  = Property (PropEnumT a) (Just (PropEnum a d))

p_we            = ("we",            defFalse)
p_hw            = ("hw",            defEnum accessType "r")
p_sw            = ("sw",            defEnum accessType "rw")
p_name          = ("name",          defNothing PropLitT)
p_desc          = ("desc",          defNothing PropLitT)
p_reset         = ("reset",         defZero)
p_counter       = ("counter",       defFalse)
p_rclr          = ("rclr",          defFalse)
p_rset          = ("rset",          defFalse)
p_hwclr         = ("hwclr",         defFalse)
p_woclr         = ("woclr",         defFalse)
p_woset         = ("woset",         defFalse)
p_wel           = ("wel",           defFalse)
p_swacc         = ("swacc",         defFalse)
p_swmod         = ("swmod",         defFalse)
p_singlepulse   = ("singlepulse",   defFalse)
p_incrvalue     = ("incrvalue",     defOne)
p_decrvalue     = ("decrvalue",     defOne)
p_incrsaturate  = ("incrsaturate",  defNothing PropNumT)
p_decrsaturate  = ("decrsaturate",  defNothing PropNumT)
p_incrthreshold = ("incrthreshold", defNothing PropNumT)
p_decrthreshold = ("decrthreshold", defNothing PropNumT)

propDefs = PropDefs {
     field = M.fromList [
       p_we, p_hw, p_sw, p_name, p_desc, p_reset, p_counter, p_rclr, p_rset, p_hwclr
     , p_woclr, p_woset, p_wel, p_swacc, p_swmod, p_singlepulse, p_incrvalue
     , p_decrvalue, p_incrsaturate, p_decrsaturate, p_incrthreshold, p_decrthreshold]
   , reg  = M.fromList [
      p_desc, p_name]
   , regfile = M.fromList [
      p_desc, p_name]
   , addrmap = M.fromList [
      p_desc, p_name]
}



