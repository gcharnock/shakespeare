
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.JHamlet where

import Text.Shakespeare.Base
import Text.Hamlet.Parse
import Language.Haskell.TH.Syntax hiding (Module)
import Language.Haskell.TH.Quote
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Data.Ratio

docFromString :: HamletSettings -> String -> [Doc]
docFromString set s =
    case parseDoc set s of
        Error s' -> error s'
        Ok (_, d) -> d


jshamlet :: QuasiQuoter
jshamlet = QuasiQuoter
  { quoteExp = jhamletFromString defaultHamletSettings,
    quotePat = undefined, 
    quoteType = undefined,
    quoteDec = undefined  }

escapeForJavascript :: Text -> Text
escapeForJavascript s = "\"" <> T.foldr f "" s <> "\""
  where f '\n' acc = '\\' `T.cons` 'n' `T.cons` acc
        f '\r' acc = '\\' `T.cons` 'r' `T.cons` acc
        f '\'' acc = '\\' `T.cons` '\'' `T.cons` acc        
        f '"'  acc = '\\' `T.cons` '"' `T.cons` acc        
        f c    acc = c `T.cons` acc
  
jhamletFromString :: HamletSettings -> String -> Q Exp
jhamletFromString settings s = let docs = docFromString settings s in
  return . LitE . StringL . T.unpack  =<< docsToJSFunction docs

docsToJSFunction :: [Doc] -> Q Text
docsToJSFunction docs = do jsBody <- docsToJSString docs
                           return $ "function(){return " <> jsBody <> "}"

docsToJSString :: [Doc] -> Q Text
docsToJSString = fmap T.concat . mapM docToJSExp

docToJSExp :: Doc -> Q Text
docToJSExp = \case
  DocForall _ _ _ -> error "$forall not supported"
  DocWith _ _ -> error "$with not supported"
  DocMaybe _ _ _ _ -> error "$maybe not supported"
  DocCond _ _ -> error "$if not supported"
  DocCase _ _ -> error "$case not supported"
  DocContent c -> contentToJSExp c

contentToJSExp :: Content -> Q Text
contentToJSExp = \case
  ContentRaw s -> return $ escapeForJavascript $ T.pack s
  ContentVar d -> return $ "+" <> derefToJSExp d <> "+"
  ContentUrl _ _ -> error "URL Rendering not yet supported"
  ContentEmbed d -> return $ derefToJSExp d
  ContentMsg _ -> error "Messages not supported in javascript yet"
  ContentAttrs _ -> error "ContentAttrs not supported in javascript yet"

derefToJSExp :: Deref -> Text
derefToJSExp = \case
  DerefBranch _ _ -> error "DerefBranch not suppored in javascript"
  DerefModulesIdent _ _ -> error "DerefModulesIdent not supported in javascript"
  DerefIdent (Ident s) -> T.pack s
  DerefIntegral i -> T.pack.show $ i
  DerefRational r -> T.pack.show $ (fromIntegral (numerator r) / fromIntegral (denominator r) :: Double)
  DerefString s -> "\"" <> T.pack s <> "\""
  DerefList ds -> "[" <> (T.intercalate "," $ map derefToJSExp ds) <> "]"
  DerefTuple _ -> error "DerefTuple  not suppored in javascript"


