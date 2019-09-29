{-# LANGUAGE TemplateHaskell #-} -- TODO

module MyTH where

import Language.Haskell.TH

mkConv :: String -> (Int, String) -> Q [Dec]
mkConv name qw = do
  tt <- [| qw |]
  return $ [FunD (mkName name) [Clause [] (NormalB tt) []]]

mkShow :: Name -> (Int, String) -> Q [Dec]
mkShow = undefined
-- mkShow name _ = do
--   tt <- [| qw |]
--   return $ [FunD (mkName "dif") [Clause [] (NormalB tt) []]]