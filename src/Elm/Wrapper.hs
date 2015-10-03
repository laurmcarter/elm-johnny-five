{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Elm.Wrapper where

import Elm.Wrapper.Quote

import qualified Data.Text.Lazy as T
import Data.Maybe (fromMaybe)

-- Module {{{

compileNative :: Module -> IO ()
compileNative mdl = do

data Module = Module
  { mdlNm      :: MdlName
  , mdlInit    :: JS
  , mdlFns     :: [JSFun]
  , mdlExports :: [(Text,JS)]
  }

instance ToJavascript Module where
  toJavascript (Module mdl prep funs exps) = toJavascript
    [js|Elm.#{mdl} = {};
       |Elm.#{mdl}.make = #{main};
       |]
    where
    main = [jsText $ toJS runtime]
       %-> [js|#{rtModule mdl}
              |if (#{runtime}.#{mdl}.values) return #{runtime}.#{mdl}.values;
              |#{toJS prep}
              |#{joinJS funs}
              |return #{runtime}.#{mdl}.values = #{jsObject exps};
              |]
    rtModule :: MdlName -> JS
    rtModule = joinJS . map go . modulePaths
      where
      go path = [js|#{runtime}.#{path} = #{runtime}.#{path} || {};|]

-- }}}

-- JSFun {{{

data JSFun = (:->) [Text] JS (Maybe Text)

instance ToJavascript JSFun where
  toJavascript ((:->) as b nm) = toJavascript
    [js|function#{rawJS nm'}#{argList as} #{jsBlock b}|]
    where
    nm' = fromMaybe "" $ fmap (T.cons ' ') nm

argList :: [Text] -> JS
argList = betweenJS "(" ")"
  . intercalateJS ", "
  . map textJS

(@@) :: Text -> (Maybe Text -> a) -> a
(@@) = flip ($) . Just
infix 1 @@

(%->) :: [Text] -> JS -> JSFun
as %-> b = (:->) as b Nothing
infix 2 %->

-- }}}

-- JS Builders {{{

importAs :: Text -> MdlName -> JS
importAs v mdl = [js|var #{rawJS v} = new Elm.#{mdl}.make(#{runtime});|]

jsList :: ToJavascript a => [a] -> JS
jsList = betweenJS "[" "]"
  . intercalateJS " , "
  . map toJS

jsObject :: ToJavascript a => [(Text,a)] -> JS
jsObject = betweenJS "{" "}"
  . intercalateJS " , "
  . map (jsPair textJS " : ")

jsPair :: ToJavascript a => (lab -> JS) -> Text -> (lab,a) -> JS
jsPair lab s (l,a) = [js|#{lab l}#{rawJS s}#{a}|]

jsBlock :: ToJavascript a => a -> JS
jsBlock a =
  [js|{
     |#{indent $ toJS a}
     |}|]

-- }}}

-- Util {{{

intercalateJS :: ToJavascript b => Text -> [b] -> JS
intercalateJS s = go
  where
  go :: ToJavascript a => [a] -> JS
  go = \case
    []   -> [js||]
    [a]  -> [js|#{a}|]
    a:as -> [js|#{a}#{rawJS s}#{go as}|]

betweenJS :: ToJavascript a => Text -> Text -> a -> JS
betweenJS o c m = [js|#{rawJS o}#{m}#{rawJS c}|]

joinJS :: ToJavascript a => [a] -> JS
joinJS = intercalateJS "\n"

indent :: JS -> JS
indent = onJS $ onText $ onLines $ map $ T.append "  "

onLines :: ([Text] -> [Text]) -> Text -> Text
onLines f = T.unlines . f . T.lines

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

-- }}}

