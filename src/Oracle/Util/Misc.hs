{-
Copyright (c) 2020 Microsoft Corporation. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Authors: Daniel Selsam

Miscellaneous utilites not requiring qualified names.
-}

module Oracle.Util.Misc where

flipl f xs init g = f g init xs
