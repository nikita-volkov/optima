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
            (Optima.explicitlyParsed Attoparsec.bool)))
        (Optima.group "group1"
          (liftA2 (,)
            (Optima.member "bb"
              (Optima.value
                "Description of B"
                Optima.defaultless
                Optima.unformatted
                (Optima.explicitlyParsed Attoparsec.text)))
            (Optima.member "d" textParam)))
        (Optima.group "group-of-alternatives"
          (asum [
            Optima.member "e" textParam,
            Optima.subgroup "subgroup" (asum [
                Optima.member "f" textParam,
                Optima.member "g" (flag $> "slkdfjsdkj")
              ])
            ])))
    where
      textParam = Optima.value "Text param" Optima.defaultless Optima.unformatted (Optima.explicitlyParsed Attoparsec.text)
      flag = Optima.flag "Flag type 1"
      bytesParam = Optima.value "" Optima.defaultless Optima.unformatted (Optima.explicitlyParsed Attoparsec.utf8Bytes)
