module GenForeign.Private where

import Data.Maybe
import Data.Function
import Data.Array
import qualified Data.Array.Unsafe as U

foreign import data JS :: *

foreign import evalString """
function evalString(s) { return eval(s); }
""" :: String -> JS

foreign import toJS
"""
function toJS(o) {
  return o;
}""" :: forall a. a -> JS

foreign import fromJS
"""
function fromJS(o) {
  return o;
}""" :: forall a. JS -> a

foreign import nul "var nul = null;" :: JS

foreign import jsFieldImpl
"""
function jsFieldImpl(o, f) {
  var v = o[f];
  if (typeof v == 'undefined')
    return null;
  else
    return v;
}""" :: forall a. Fn2 JS String JS
jsField = runFn2 jsFieldImpl

foreign import isAny
"""
function isAny(o) {
  return true;
}""" :: JS -> Boolean

foreign import isNull
"""
function isNull(o) {
  return o == null;
}""" :: JS -> Boolean

foreign import isArray
"""
function isArray(o) {
  return Object.prototype.toString.call(o) === '[object Array]';
}""" :: JS -> Boolean

foreign import isArrayArray
"""
function isArrayArray(o) {
  return isArray(o) && isArray(o[0]);
}""" :: JS -> Boolean

foreign import isString
"""
function isString(o) {
  return typeof(o) == 'string';
}""" :: JS -> Boolean

foreign import isNumber
"""
function isNumber(o) {
  return typeof(o) == 'number';
}""" :: JS -> Boolean

foreign import isBoolean
"""
function isBoolean(o) {
  return typeof(o) == 'boolean';
}""" :: JS -> Boolean

foreign import isObject
"""
function isObject(o) {
  return typeof(o) == 'object';
}""" :: JS -> Boolean

foreign import encRecord
"""
function encRecord(o) {
  var r = {};
  for (var k in o) {
    var v = o[k];
    if (v != null)
      r[k] = v;
  }
  return r;
}""" :: forall a. {|a} -> JS

prefixLines :: String -> String -> [String] -> [String]
prefixLines p0 pn a = first : rest
  where first = p0 ++ U.head a
        rest = ((++) pn) <$> U.tail a

prefixLinesA :: String -> String -> String -> [[String]] -> [[String]]
prefixLinesA p0 pn pi a = first : rest
  where first = case head a of
          Just h -> prefixLines p0 pi h
          Nothing -> [""]
        rest = case tail a of
          Just t -> prefixLines pn pi <$> t
          Nothing -> []

encMaybe :: forall a. (a -> JS) -> Maybe a -> JS
encMaybe f (Just a) = f a
encMaybe _ Nothing = nul

decMaybe :: forall a. (JS -> a) -> JS -> Maybe a
decMaybe f o = if isNull o then
                 Nothing
               else
                 Just $ f o

linesMaybe :: forall a. (a -> [String]) -> Maybe a -> [String]
linesMaybe f (Just a) = prefixLines "Just " "" $ f a
linesMaybe _ Nothing = ["Nothing"]

encArray :: forall a. (a -> JS) -> [a] -> JS
encArray f a = toJS $ f <$> a

decArray :: forall a. (JS -> a) -> JS -> [a]
decArray f a = f <$> fromJS a

defArray :: forall a. [a]
defArray = []

linesArray :: forall a. (a -> [String]) -> [a] -> [String]
linesArray f a = ["["] <> (concat $ prefixLinesA "    " "  , " "    " $ f <$> a) <> ["  ]"]

encString :: String -> JS
encString = toJS

decString :: JS -> String
decString = fromJS

defString :: String
defString = ""

linesString :: String -> [String]
linesString o = [show o]

encNumber :: Number -> JS
encNumber = toJS

decNumber :: JS -> Number
decNumber = fromJS

defNumber :: Number
defNumber = 0

linesNumber :: Number -> [String]
linesNumber o = [show o]

encBoolean :: Boolean -> JS
encBoolean = toJS

decBoolean :: JS -> Boolean
decBoolean = fromJS

defBoolean = false

linesBoolean :: Boolean -> [String]
linesBoolean o = [show o]

linesMaybeString :: Maybe String -> [String]
linesMaybeString o = linesMaybe linesString o
