{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.SRDL.Props (
       defDefs
     , getPropType
     , checktype
     , typeOf
     , isEnum
     , getEnumValues
     , isPropSet
     , isPropActive
     , exMap
     , assignProp
     , getNumProp
     , getBoolProp
     , getEnumProp
     , calcAccess
     , buildPropPath
     , buildPropTraversal
     , setPostProp
     , setProp
     ) where

import qualified Data.Map.Strict as M
import Control.Lens
import Data.List (delete)

import Data.Functor.Foldable
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, isJust, fromJust)
import Control.Monad (msum)
import Debug.Trace
import Data.List (foldl')
import qualified Data.HashMap.Strict as HM

import Language.SRDL.Types

typeOf :: PropRHS -> PropType
typeOf (PropNum  _)    = PropNumT
typeOf (PropLit  _)    = PropLitT
typeOf (PropBool _)    = PropBoolT
typeOf (PropRef _ _)   = PropRefT
typeOf (PropIntr _ _)  = PropIntrT
typeOf (PropEnum _)    = PropEnumT

checktype :: [PropType] -> PropRHS -> Bool
checktype x y = typeOf y `elem` x

accessType = EnumDef (M.fromList [("rw", 0), ("wr", 1), ("r", 2), ("w", 3), ("na", 4)])

assignProp k v = HM.insert k (Just v)

defFalse     = Property [PropBoolT] (Just (PropBool False))
defNum n     = Property [PropNumT] (Just (PropNum n))
defLit n     = Property [PropLitT] (Just (PropLit n))
defNothing a = Property a Nothing
defEnum d    = Property [PropEnumT] (Just (PropEnum d))
--defIntr      = Property [PropIntrT] (Just (PropIntr NonSticky NonIntr))

addRef (Property xs d) = Property (xs ++ [PropRefT]) d

isEnum prop = prop `elem` (["hw", "sw", "priority", "precedence", "addressing"] :: [Text])

getEnumValues "hw" | (EnumDef m) <- accessType = M.keys m
getEnumValues "sw" | (EnumDef m) <- accessType = M.keys m

p_intr          = ("intr",          defNothing [PropIntrT])
p_hw            = ("hw",            defEnum "r")
p_sw            = ("sw",            defEnum "rw")
p_name          = ("name",          defNothing [PropLitT])
p_desc          = ("desc",          defNothing [PropLitT])
p_reset         = ("reset",         defNum 0)
p_fieldwidth    = ("fieldwidth",    defNum 0)
p_counter       = ("counter",       defFalse)
p_rclr          = ("rclr",          defFalse)
p_wclr          = ("wclr",          defFalse)
p_rset          = ("rset",          defFalse)
p_wset          = ("wset",          defFalse)
p_hwclr         = ("hwclr",         defFalse)
p_woclr         = ("woclr",         defFalse)
p_nonsticky     = ("nonsticky",     defFalse)
p_sticky        = ("sticky",        defFalse)
p_stickybit     = ("stickybit",     defFalse)
p_woset         = ("woset",         defFalse)
p_we            = ("we",            addRef defFalse)
p_wel           = ("wel",           defFalse)
p_swacc         = ("swacc",         defFalse)
p_swmod         = ("swmod",         defFalse)
p_sharedextbus  = ("sharedextbus",  defFalse)
p_singlepulse   = ("singlepulse",   defFalse)
p_regwidth      = ("regwidth",      defNum 32)
p_incrsaturate  = ("incrsaturate",  defNothing [PropNumT])
p_decrsaturate  = ("decrsaturate",  defNothing [PropNumT])
p_incrthreshold = ("incrthreshold", defNothing [PropNumT])
p_decrthreshold = ("decrthreshold", defNothing [PropNumT])
p_incr          = ("incr",          defNothing [PropRefT])
p_incrwidth     = ("incrwidth",     defNothing [PropNumT])
p_decrwidth     = ("decrwidth",     defNothing [PropNumT])
p_incrvalue     = ("incrvalue",     defNothing [PropNumT])
p_decrvalue     = ("decrvalue",     defNothing [PropNumT])
p_enable        = ("enable",        defNothing [PropRefT])
p_enablemask    = ("enablemask",    defNothing [PropRefT])
p_haltenable    = ("haltenable",    defNothing [PropRefT])
p_haltmask      = ("haltmask",      defNothing [PropRefT])


isPropSet :: Fix ElabF -> Text -> Bool
isPropSet f p = isJust $ f ^? _Fix . eprops . ix p . _Just

isPropActive f p =
   case f ^? _Fix . eprops . ix p . _Just of
      Nothing -> False
      Just (PropBool False) -> False
      _ ->  True

getPropType :: Text -> Maybe PropType
getPropType p = do
    compProps <- M.lookup Field defDefs
    prop <- M.lookup p compProps
    return $ head (prop ^. ptype)

defDefs = M.fromList [
   (Field,   M.fromList [ p_we
                        , p_hw
                        , p_sw
                        , p_incr
                        , p_name
                        , p_fieldwidth
                        , p_desc
                        , p_reset
                        , p_counter
                        , p_rclr
                        , p_rset
                        , p_wclr
                        , p_wset
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
                        , p_incrwidth
                        , p_decrwidth
                        , p_incrvalue
                        , p_decrvalue
                        , p_intr
                        , p_sticky
                        , p_nonsticky
                        , p_stickybit
                        , p_enable
                        , p_enablemask
                        , p_haltenable
                        , p_haltmask
                        ]),

   (Reg,    M.fromList [ p_desc
                        , p_name
                        , p_intr
                        , p_sharedextbus
                        , p_regwidth
                        ]),

   (Regfile, M.fromList [ p_desc
                        , p_name
                        , p_sharedextbus
                        ]),

   (Addrmap, M.fromList [ p_desc
                        , p_name
                        , p_sharedextbus
                        ])]



exMap :: M.Map Text [Text]
exMap = foldl' M.union M.empty (map f exSets)
  where f x = foldl' (.) id [M.insert k (delete k x) | k <- x] M.empty
        exSets =
          [ ["activehigh", "activelow"]
          , ["woclr", "woset"]
          , ["we", "wel"]
          , ["hwenable", "hwmask"]
          , ["counter", "intr"]
          , ["counter"]
          , ["incrvalue", "incrwidth"]
          , ["decrvalue", "decrwidth"]
          , ["nonsticky", "sticky", "stickybit"]
          , ["enable", "mask"]
          , ["haltenable", "haltmask"]
          , ["bigendian", "littleendian"]
          , ["lsb0", "msb0"]
          , ["async", "sync"]
          , ["dontcompare", "dontest"]
          , ["level", "negedge", "posedge", "bothedge"]]

getNumProp :: Text -> Fix ElabF -> Integer
getNumProp k e =
   case e ^? _Fix . eprops . ix k of
      Just (Just (PropNum n)) -> n
      Just _ -> error $ T.unpack (k <> " is not a numeric property")
      Nothing -> error $ T.unpack (k <> " is not a valid property for " <> n)
   where n = e ^. _Fix . ename

getBoolProp :: Text -> Fix ElabF -> Bool
getBoolProp k e =
   case e ^? _Fix . eprops . ix k of
      Just (Just (PropBool n)) -> n
      Just a -> trace (show (e ^? _Fix . eprops . ix "fqname", show a, k)) (error "you dun f'd up")
      Nothing -> error $ T.unpack (k <> " is not a valid property")

getEnumProp :: Text -> Fix ElabF -> Text
getEnumProp k e =
   case e ^? _Fix . eprops . ix k of
      Just (Just (PropEnum n)) -> n
      Just _ -> error $ T.unpack (k <> " is not a boolean property")
      Nothing -> error $ T.unpack (k <> " is not a valid property")

calcAccess f =
  case (readAccess, writeAccess) of
    (Normal,     Disallowed) -> RO
    (Normal,     Normal)     -> RW
    (Clear,      Disallowed) -> RC
    (Set,        Disallowed) -> RS
    (Clear,      Normal)     -> WRC
    (Set,        Normal)     -> WRS
    (Normal,     Set)        -> WS
    (Normal,     Clear)      -> WC
    (Clear,      Set)        -> WSRC
    (Set,        Clear)      -> WCRS
    (Normal,     OneClear)   -> W1C
    (Normal,     OneSet)     -> W1S
    (Normal,     ZeroClear)  -> W0C
    (Normal,     ZeroSet)    -> W0S
    (Clear,      OneSet)     -> W1SRC
    (Set,        OneClear)   -> W1CRS
    (Clear,      ZeroSet)    -> W0SRC
    (Set,        ZeroClear)  -> W0CRS
    (Disallowed, Normal)     -> WO
    (Disallowed, Clear)      -> WOC
    (Disallowed, Set)        -> WOS
    _                        -> error "Unexpected RW access combination"
  where
    readAccess = case getEnumProp "sw" f of
                   "w"  -> Disallowed
                   "na" -> Disallowed
                   _    -> fromMaybe Normal $ msum $ map (\(p, e) -> if getBoolProp p f
                                                                      then Just e
                                                                      else Nothing
                                                          ) [ ("rclr", Clear)
                                                            , ("rset", Set) ]
    writeAccess = case getEnumProp "sw" f of
                    "r"  -> Disallowed
                    "na" -> Disallowed
                    _    -> fromMaybe Normal $ msum $ map (\(p, e) -> if getBoolProp p f
                                                                      then Just e
                                                                      else Nothing
                                                          ) [ ("wclr", Clear)
                                                            , ("wset", Set)
                                                            , ("woclr", OneClear)
                                                            , ("woset", OneSet)]

type instance IxValue (Fix ElabF) = Fix ElabF
type instance Index (Fix ElabF) = Text

instance Ixed (Fix ElabF) where
    ix k f m = case break (\x -> unfix x ^. ename == k) (unfix m ^. einst) of
                (_, []) -> pure m
                (i, l:ls) -> f l <&> \x -> Fix $ unfix m & einst .~ (i ++ (x:ls))

buildTraversal e x = foldl' (>>=) (Right $ Traversal id) (map f x)
  where f y x =
         case e ^? (runTraversal x . ix y) of
            Nothing -> Left y
            Just _  -> Right $ Traversal $ runTraversal x . ix y

buildPropTraversal e path = buildTraversal e (concatMap buildPropPath path)

buildPropPath (PathElem s Nothing) = [s]
buildPropPath (PathElem s (Just (ArrWidth w))) = [s, (T.pack . show) w]

setPostProp :: (a ~ Fix ElabF) => (ReifiedTraversal a a a a, Text, PropRHS) -> Fix ElabF -> Fix ElabF
setPostProp (t, prop, rhs) e = e & runTraversal t . _Fix . eprops %~ assignProp prop rhs

setProp n p e = e & _Fix . eprops %~ assignProp n p
