{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Elm.Wrapper.Quote
  ( js , jsFile
  ----
  , JS()
  , onText
  , jsText
  , textJS
  , simpleJS
  , onJS
  , toJS
  , ppJS
  , writeJS
  , runtime
  ----
  , MdlName()
  , modulePaths
  ----
  , Javascript(..)
  , JavascriptUrl
  , RawJS(..)
  , ToJavascript(..)
  , Text()
  ) where

import Text.Julius (Javascript(..),JavascriptUrl,RawJS(..),ToJavascript(..))
import qualified Text.Julius as JS
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Char
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy         as T
import qualified Data.Text.Lazy.Builder as T
import qualified Data.Text.Lazy.IO      as T
import GHC.Exts (IsString(..))
import Data.List (inits)

-- QQ {{{

julius :: QuasiQuoter
julius = barify JS.julius

js :: QuasiQuoter
js = julius { quoteExp = wrapFn . quoteExp julius }
  where
  wrapFn :: Q Exp -> Q Exp
  wrapFn = appE (conE 'JS)

jsFile :: QuasiQuoter
jsFile = quoteFile js

-- }}}

-- JSBuilder {{{

type JSBuilder = JavascriptUrl ()

textJS_ :: Text -> JSBuilder
textJS_ = const . toJavascript . rawJS

jsText_ :: JSBuilder -> Text
jsText_ = JS.renderJavascript . simpleJS

onText :: (Text -> Text) -> JSBuilder -> JSBuilder
onText f = textJS_ . f . jsText_

simpleJS :: JSBuilder -> Javascript
simpleJS = ($ \_ _ -> undefined)

ppJSBuilder :: JSBuilder -> IO ()
ppJSBuilder = T.putStrLn . jsText_

writeJSBuilder :: FilePath -> JSBuilder -> IO ()
writeJSBuilder fp = T.writeFile fp . jsText_

toJSBuilder :: ToJavascript a => a -> JSBuilder
toJSBuilder = const . toJavascript

-- }}}

-- JS {{{

newtype Runtime = RT JSBuilder

runtime :: Runtime
runtime = RT $ textJS_ "_elm"

instance ToJavascript Runtime where
  toJavascript (RT t) = simpleJS t

newtype JS = JS JSBuilder

instance ToJavascript JS where
  toJavascript (JS t) = simpleJS t

onJS :: (JSBuilder -> JSBuilder) -> JS -> JS
onJS f (JS t) = JS $ f t

toJS :: ToJavascript a => a -> JS
toJS = JS . toJSBuilder

jsText :: JS -> Text
jsText (JS b) = jsText_ b

textJS :: Text -> JS
textJS = JS . textJS_

ppJS :: ToJavascript a => a -> IO ()
ppJS = ppJSBuilder . toJSBuilder . toJS

writeJS :: ToJavascript a => FilePath -> a -> IO ()
writeJS f = writeJSBuilder f . toJSBuilder . toJS

-- }}}

-- MdlName {{{

newtype MdlName = MdlName [Text]

instance ToJavascript MdlName where
  toJavascript (MdlName path) = JS.Javascript
    $ T.fromLazyText
    $ T.intercalate "." path

instance IsString MdlName where
  fromString = fromPath . fromString

modulePaths :: MdlName -> [MdlName]
modulePaths (MdlName mdl) =
  [ MdlName path
  | path <- inits mdl
  , not $ null path
  ]

fromPath :: Text -> MdlName
fromPath (T.splitOn "." -> path)
  | not (null path) && all (not . T.null) path
  = MdlName path
  | otherwise
  = error "Empty module name"

-- }}}

-- Barify {{{

barify :: QuasiQuoter -> QuasiQuoter
barify (QuasiQuoter
  { quoteExp  = e
  , quotePat  = p
  , quoteType = t
  , quoteDec  = d
  }) = QuasiQuoter
  { quoteExp  = e . barLines
  , quotePat  = p . barLines
  , quoteType = t . barLines
  , quoteDec  = d . barLines
  }

barLines :: String -> String
barLines = foldLines . onTail (map $ dropWhile isSpace) . lines
  where
  unbar :: String -> String
  unbar = \case
    '|':ln -> ln
    ln     -> ln
  foldLines :: [String] -> String
  foldLines = \case
    []  -> ""
    [unbar -> l] -> l
    (unbar -> l) : ls@(l' : _) -> case l' of
      '|':_ -> l ++ "\n" ++ foldLines ls
      _     -> l ++ " "  ++ foldLines ls

-- }}}

-- Util {{{

onTail :: ([a] -> [a]) -> [a] -> [a]
onTail f as = case as of
  []    -> []
  a:as' -> a : f as'

-- }}}

