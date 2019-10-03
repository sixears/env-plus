{-# LANGUAGE UnicodeSyntax #-}

-- base --------------------------------

import System.IO  ( IO )

-- tasty -------------------------------

import Test.Tasty           ( TestTree, defaultIngredients, testGroup )
import Test.Tasty.Runners   ( defaultMainWithIngredients )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified Env.Reader
import qualified Env.Types 

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "env-plus" [ Env.Reader.tests, Env.Types.tests ]

main ∷ IO ()
main = defaultMainWithIngredients defaultIngredients tests

-- that's all, folks! ----------------------------------------------------------
