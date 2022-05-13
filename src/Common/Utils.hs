{-# LANGUAGE FlexibleInstances #-}
module Common.Utils (showP, showI) where
import Generated.Syntax

showP :: BNFC'Position -> String
showP (Just (x, y)) = "line " ++ show x ++ ", column " ++ show y
showP _             = "unknown position"

showI :: Ident -> String
showI (Ident s) = s