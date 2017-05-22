{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Props (
       defDefs
     , getPropType
     , checktype
     , addProperty
     , typeOf
     , isEnum
     , getEnumValues
     ) where

import qualified Data.Map.Strict as M
import Control.Lens

import Types

typeOf :: PropRHS -> PropType
typeOf (PropNum  _)  = PropNumT
typeOf (PropLit  _)  = PropLitT
typeOf (PropBool _)  = PropBoolT
typeOf (PropRef _ _) = PropRefT
typeOf (PropIntr _ _) = PropIntrT
typeOf (PropEnum _) = PropEnumT

checktype :: PropType -> PropRHS -> Bool
checktype x y = x == typeOf y

accessType = EnumDef (M.fromList [("rw", 0), ("wr", 1), ("r", 2), ("w", 3), ("na", 4)])

--intrType = EnumDef "intr_type" (M.fromList [("level", 0), ("nonsticky", 1), ("posedge", 2), ("negedge", 3), ("bothedge", 4)])

defFalse     = Property PropBoolT (Just (PropBool False))
defZero      = Property PropNumT (Just (PropNum 0))
defOne       = Property PropNumT (Just (PropNum 1))
defNothing a = Property a Nothing
defEnum d    = Property PropEnumT (Just (PropEnum d))

isEnum prop = any (== prop) ["hw", "sw", "priority", "precedence", "addressing"]

getEnumValues "hw" | (EnumDef m) <- accessType = M.keys m
getEnumValues "sw" | (EnumDef m) <- accessType = M.keys m

p_intr          = ("intr",          defNothing PropIntrT)
p_we            = ("we",            defFalse)
p_hw            = ("hw",            defEnum "r")
p_sw            = ("sw",            defEnum "rw")
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
p_sharedextbus  = ("sharedextbus",  defFalse)
p_singlepulse   = ("singlepulse",   defFalse)
p_incrvalue     = ("incrvalue",     defOne)
p_decrvalue     = ("decrvalue",     defOne)
p_incrsaturate  = ("incrsaturate",  defNothing PropNumT)
p_decrsaturate  = ("decrsaturate",  defNothing PropNumT)
p_incrthreshold = ("incrthreshold", defNothing PropNumT)
p_decrthreshold = ("decrthreshold", defNothing PropNumT)

getPropType :: String -> Maybe PropType
getPropType p = do
    compProps <- M.lookup Field defDefs
    prop <- M.lookup p compProps
    return $ prop ^. ptype

addProperty p n = foldl (addCompProperty n) p (ctypes n)

addCompProperty n p c = p & (at c . _Just . at (name n)) ?~ Property (propType n) (value n)

defDefs = M.fromList [
   (Field,   M.fromList [ p_we
                        , p_hw
                        , p_sw
                        , p_name
                        , p_desc
                        , p_reset
                        , p_counter
                        , p_rclr
                        , p_rset
                        , p_hwclr
                        , p_woclr
                        , p_woset
                        , p_wel
                        , p_swacc
                        , p_swmod
                        , p_singlepulse
                        , p_incrvalue
                        , p_decrvalue
                        , p_incrsaturate
                        , p_decrsaturate
                        , p_incrthreshold
                        , p_decrthreshold
                        , p_intr
                        ]),

   (Reg,     M.fromList [ p_desc
                        , p_name
                        , p_intr
                        , p_sharedextbus
                        ]),

   (Regfile, M.fromList [ p_desc
                        , p_name
                        , p_sharedextbus
                        ]),

   (Addrmap, M.fromList [ p_desc
                        , p_name
                        , p_sharedextbus
                        ])]




