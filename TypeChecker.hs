{-# LANGUAGE Haskell2010 #-}

-- | Wrapper that provides the tc-tdiag executable.
--   It parses the input and applies the type checker attribute grammar that might either succeed 
--   and pass on the input ATerm, or fail with an error.
module TypeChecker where

import TypeChecker.AG (typeCheckComponent)

import Control.Arrow

import CCO.Component
import CCO.Feedback
import CCO.Tree
import CCO.Printing

import CCO.Diag

-- | Parses an ATerm produced by the T-Diagram parser and runs it through the type checker.
--   On success, it prints its input to stdout.
mainComponent :: Component String String
mainComponent = arr (\x -> (x,x)) 
                >>> first CCO.Tree.parser
                >>> first (component toTree)
                >>> first typeCheckComponent
                >>> arr snd

-- |
main :: IO ()
main = ioWrap mainComponent


