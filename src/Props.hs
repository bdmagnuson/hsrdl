{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Props (
       defDefs
     , getTypeString
     , getPropType
     , checktype
     , addProperty
     ) where

import qualified Data.Map.Strict as M
import Control.Lens

import Types

getTypeString (PropLit  _)   = "string"
getTypeString (PropNum  _)   = "number"
getTypeString (PropBool _)   = "boolean"
getTypeString (PropRef  _ _) = "property reference"
getTypeString (PropPath _)   = "component path"
getTypeString (PropEnum _ _) = "enumeration"

checktype :: Maybe PropType -> PropRHS -> Bool
checktype Nothing _  = False
checktype (Just PropNumT)  (PropNum  _) = True
checktype (Just PropLitT)  (PropLit  _) = True
checktype (Just PropBoolT) (PropBool _) = True
checktype (Just PropRefT)  (PropRef  _ _) = True
checktype _ _ = False

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

getPropType :: String -> Maybe PropType
getPropType p = do
    compProps <- M.lookup Field defDefs
    prop <- M.lookup p compProps
    return $ ptype prop

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
                          ]),

   (Reg,     M.fromList [ p_desc
                          , p_name
                          ]),

   (Regfile, M.fromList [ p_desc
                          , p_name
                          ]),

   (Addrmap, M.fromList [ p_desc
                          , p_name
                          ])]




