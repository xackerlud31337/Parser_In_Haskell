-- Add your names and student numbers to the following lines. Do not change anything else on these lines, since it they are parsed.
-- Student 1: Your Name (sxxxxxxx)
-- Student 2: Second student (syyyyyyy)
-- Student 3: Third student (szzzzzzz)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.All





-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
