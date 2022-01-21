{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Covalent

import Data.Text (Text)
import Control.Lens
import Data.Text.Lens
import GHC.Generics
import GHC.Base (Applicative)

data Numeral = NumInt Int | NumDouble | NumPair !Double !Int deriving Generic

instance Covalent Show Numeral
instance Covalent Num Numeral

showNumeral :: Numeral -> String
showNumeral = viewsCommon @Show show

add1 :: Numeral -> Numeral
add1 = overCommon @Num (+1)

data SomeText = FromString String | FromText Text deriving Generic

instance Covalent IsText SomeText

myPacked :: Lens' SomeText String
myPacked f = traverseCommon @IsText $ unpacked f

main = pure ()