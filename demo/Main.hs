module Main where

import Prelude
import qualified Optima
import qualified Attoparsec.Data as Attoparsec


main = parseOpts >>= print where
  parseOpts =
    Optima.params "Demo"
      (liftA3 (,,)
        (Optima.param (Just 'a') "arg-a"
          (Optima.value
            "Description of A"
            (Optima.showable True)
            Optima.unformatted
            (Optima.explicitParser Attoparsec.bool)))
        (Optima.param (Just 'b') "arg-b"
          (Optima.value
            "Description of B"
            Optima.defaultless
            Optima.unformatted
            (Optima.explicitParser Attoparsec.text)))
        (Optima.param Nothing "arg-c"
          (Optima.value
            ""
            Optima.defaultless
            Optima.unformatted
            (Optima.explicitParser Attoparsec.utf8Bytes))))
