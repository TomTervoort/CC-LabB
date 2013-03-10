{-# LANGUAGE Haskell2010 #-}

module Tdiag2Picture (main) where

import Control.Arrow
import CCO.Feedback
import CCO.Component hiding (parser)
import CCO.Printing (text)
import CCO.Diag hiding (parser)
import CCO.Picture
import CCO.Tree       (parser, Tree (fromTree, toTree))



-- | Parses an ATerm produced by the T-Diagram parser and runs it through the type checker.
tdiag2picture :: Diag -> Feedback Picture
tdiag2picture (Diag sp diag_) = return (tdiag2picture_ diag_)

tdiag2picture_ :: Diag_ -> Picture
tdiag2picture_ (Program p l) = (Picture (65, 30) [
	(Put (7.5, 0) (Line (1, 0) 50)),
	(Put (7.5, 0) (Line (0, 1) 15)),
	(Put (7.5, 15) (Line (-1, 2) 7.5)),
	(Put (57.5, 15) (Line (1, 2) 7.5)),
	(Put (57.5, 0) (Line (0, 1) 15)),
	(Put (0, 30) (Line (1, 0) 65)),
	(Put (7.5, 15) (Makebox (50, 15) p)),
	(Put (7.5, 0) (Makebox (50, 15) l))
	])

tdiag2picture_ (Platform m) = (Picture (50, 30) [
	(Put (0, 15) (Line(5,-3) 25)),
	(Put (25,0) (Line (5,3) 25)),
	(Put (0,15) (Line (0,1) 15)),
	(Put (0,30) (Line (1,0) 50)),
	(Put (50,30) (Line(0,-1) 15)),
	(Put (0,15) (Makebox (50,15) m))
	])

tdiag2picture_ (Interpreter i l m) = (Picture (50,30) [
	(Put (0,0) (Framebox (50,30) "")),
	(Put (0,20) (Makebox (50,10) l)),
	(Put (0,10) (Makebox (50,10) i)),
	(Put (0,0) (Makebox (50,10) m))
	])

tdiag2picture_ (Compiler c l1 l2 m) = (Picture (150,30) [
	(Put (50,0) (Line (0,1) 20)),
	(Put (50,20) (Line (-1,0) 50)),
	(Put (0,20) (Line (0,1) 10)),
	(Put (0,30) (Line (1,0) 150)),
	(Put (150,30) (Line (0,-1) 10)),
	(Put (150,20) (Line (-1,0) 50)),
	(Put (100,20) (Line (0,-1) 20)),
	(Put (100,0) (Line (-1,0) 50)),
	(Put (0,20) (Makebox (50,10) l1)),
	(Put (50,20) (Makebox (50,10) "$\\longrightarrow$")),
	(Put (100,20) (Makebox (50,10) l2)),
	(Put (50,10) (Makebox (50,10) c)),
	(Put (50,0) (Makebox(50,10) m))
	])


--tdiag2picture_ (Execute d1 d2) 
--tdiag2picture_ (Compile d1 d2) 



main = ioWrap $ CCO.Tree.parser >>> component toTree >>> component tdiag2picture >>> arr fromTree >>> printer