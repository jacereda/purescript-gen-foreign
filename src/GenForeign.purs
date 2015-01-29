module GenForeign where

import Debug.Trace
import Data.Maybe
import Data.Array(head, tail, take, drop, concat)
import Data.String(joinWith)
import Data.String.Unsafe(charAt, charCodeAt)
import Data.String.Regex(match, regex)
import qualified Data.Array.Unsafe as UA
import SimpleTemplates


data S = S String String
data A = A String [AA]
data AA = AA String String String
data E = E String [EE]
data EE = EE String String
data RR = RR String String String
data R = R String [RR]

quoted :: String -> String
quoted s = "\"" ++ s ++ "\""

escField :: String -> String
escField s | s == "type" = quoted s
escField s | s == "data" = quoted s
escField s = s

prefixes :: String -> String -> [String] -> String
prefixes fp np s = fp ++ f ++ joinWith "" ((\x -> np ++ x) <$> n)
  where f :: String
        f = case (head s) of
          Just x -> x
          Nothing -> ""
        n :: [String]
        n = drop 1 s

et :: String -> String -> String
et op t =
  case match are t of 
    Just a -> op ++ "Array $ " ++ et op (UA.head $ UA.tail a)
    otherwise -> case match mre t of
      Just a -> op ++ "Maybe $ " ++ et op (UA.head $ UA.tail a)
      otherwise -> op ++ t
  where ref = { unicode : false,
                sticky : false,
                multiline : false,
                ignoreCase : false,
                global : false }
        are = regex "^\\[(.*)\\]$" ref
        mre = regex "^Maybe (.*)$" ref

dumpA :: A -> String
dumpA (A new as) = template """
data {new} =
{alts}

enc{new} :: {new} -> JS
{encs}

dec{new} :: JS -> {new}
{decs}

lines{new} :: {new} -> [String]
{lines}
"""
                   [ Replace "{new}" new
                   , Replace "{alts}" alts
                   , Replace "{encs}" encs
                   , Replace "{decs}" decs
                   , Replace "{lines}" lines
    ]
  where alts = prefixes "    " "\n  | " $ alt <$> as
        alt (AA n t _) = template "{n} {par}"
                         [ Replace "{n}" n
                         , Replace "{par}" (par t)
                         ]
        encs = prefixes "" "\n" $ enc <$> as
        enc (AA n t _) = template "enc{new} {var} = {venc}"
                         [ Replace "{n}" n
                         , Replace "{new}" new
                         , Replace "{var}" (var n t)
                         , Replace "{venc}" (seq "enc" t)
                         ]
        decs = prefixes "" "\n" $ dec <$> as
        dec (AA n t chk) = template "dec{new} o | {chk} o = {vdec} "
                           [ Replace "{n}" n
                           , Replace "{new}" new
                           , Replace "{chk}" chk
                           , Replace "{vdec}" (vdec n t)
                           ]
        lines = prefixes "" "\n" $ line <$> as
        line (AA n t _) = template "lines{new} {var} = prefixLines \"{n} \" \"\" $ {lt}"
                          [ Replace "{n}" n
                          , Replace "{new}" new
                          , Replace "{var}" (var n t)
                          , Replace "{lt}" (lt n t)
                          ]
        par t = if istype t then t else ""
        var n t = if istype t then "(" ++ n ++ " o)" else n
        seq n t = if istype t then "(" ++ (et "enc" t) ++ ") o" else "encString " ++ quoted t
        vdec n t = if istype t then n ++ " $ (" ++ (et "dec" t) ++ ") o" else n
        lt n t = if istype t then "(" ++ (et "lines" t) ++ ") o" else "[]"
        istype t = c >= 65 && c <= 91
          where c = charCodeAt 0 t

                       

dumpS :: S -> String
dumpS (S new old) = template """
type {new} = {old}

enc{new} :: {new} -> JS
enc{new} = enc{old}

dec{new} :: JS -> {new}
dec{new} = dec{old}

def{new} :: {new}
def{new} = def{old}

lines{new} :: {new} -> [String]
lines{new} = lines{old}
"""
                    [ Replace "{new}" new
                    , Replace "{old}" old
                    ]
                    
dumpE :: E -> String
dumpE (E new ees) = template """
data {new} =
{defs}

enc{new} :: {new} -> JS
{encs}

dec{new} :: JS -> {new}
dec{new} o = case fromJS o of
{decs}

lines{new} :: {new} -> [String]
{lines}
"""
                    [ Replace "{new}" new
                    , Replace "{defs}" defs
                    , Replace "{encs}" encs
                    , Replace "{decs}" decs
                    , Replace "{lines}" lines
                    ]
  where defs = prefixes "    " "\n  | " $ def <$> ees
        def (EE t n) = t
        encs = prefixes "" "\n" $ enc <$> ees
        enc (EE t n) = template "enc{new} {t} = encString \"{n}\""
                       [ Replace "{t}" t
                       , Replace "{n}" n
                       , Replace "{new}" new
                       ]
        decs = prefixes "" "\n" $ dec <$> ees
        dec (EE t n) = template "  \"{n}\" -> {t}"
                       [ Replace "{t}" t
                       , Replace "{n}" n
                       ]
        lines = prefixes "" "\n" $ line <$> ees
        line (EE t _) = template "lines{new} {t} = linesString \"{t}\""
                        [ Replace "{new}" new
                        , Replace "{t}" t
                        ]

dumpT :: R -> String
dumpT (R new fs) = template """
type {new} = {
{fields}
  }

enc{new} :: {new} -> JS
enc{new} o = encRecord {
{encs}
  }

dec{new} :: JS -> {new}
dec{new} o = {
{decs}
  }

def{new} :: {new}
def{new} = {
{defs}
  }

lines{new} :: {new} -> [String]
lines{new} o = 
     ["{new} {"]
{lines}
  <> ["  }"]

show{new} o = joinWith "\n" $ lines{new} o
"""
  [ Replace "{new}" new
  , Replace "{fields}" fields
  , Replace "{encs}" encs
  , Replace "{decs}" decs
  , Replace "{defs}" defs
  , Replace "{lines}" lines
  ]
  where fields = prefixes "    " "\n  , " $ field <$> fs
        field (RR n t _) = template "{n} :: {t}"
          [ Replace "{n}" n
          , Replace "{t}" t
          ]
        encs = prefixes "    " "\n  , " $ enc <$> fs
        enc (RR n t _) = template "{n} : ({et}) o.{n}" 
          [ Replace "{n}" n
          , Replace "{et}" (et "enc" t)
          ]
        decs = prefixes "    " "\n  , " $ dec <$> fs
        dec (RR n t _) = template "{n} : ({et}) (jsField o \"{n}\")" 
          [ Replace "{n}" n
          , Replace "{et}" (et "dec" t)
          ]
        defs = prefixes "    " "\n  , " $ def <$> fs
        def (RR n _ v) = template "{n} : {v}" 
          [ Replace "{n}" n
          , Replace "{v}" v
          ]
        lines = prefixes "  <> " "\n  <> " $ (line0 hfs) : (line <$> tfs)
          where hfs = case head fs of 
                        Just t -> t
                tfs = case tail fs of 
                        Just t -> t
                        Nothing -> []
        line0 (RR n t _) = template "prefixLines \"    {n} : \" \"  \" (({t}) o.{n})" 
          [ Replace "{t}" (et "lines" t)
          , Replace "{n}" n
          ]
        line (RR n t _) = template "prefixLines \"  , {n} : \" \"  \" (({t}) o.{n})" 
          [ Replace "{t}" (et "lines" t)
          , Replace "{n}" n
          ]

dumpH = ["""

module GeneratedForeign where

import GenForeign.Private
import Data.Maybe
import Data.String(joinWith)
"""]

