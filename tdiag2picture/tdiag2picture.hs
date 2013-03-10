{-# LANGUAGE Haskell2010 #-}

module Tdiag2Picture (main) where

import Control.Arrow
import CCO.Feedback
import CCO.Component hiding (parser)
import CCO.Printing (text)
import CCO.Diag hiding (parser)
import CCO.Picture
import CCO.Tree       (parser, Tree (fromTree, toTree))

type Pos = (Int, Int)
data PosDiag =
    PosProgram      Pos Ident Ident
  | PosPlatform     Pos Ident
  | PosInterpreter  Pos Ident Ident Ident
  | PosCompiler     Pos Ident Ident Ident Ident
  | PosExecute      Pos PosDiag PosDiag
  | PosCompile      Pos PosDiag PosDiag

-- | Parses an ATerm produced by the T-Diagram parser and runs it through the type checker.
tdiag2picture :: Diag -> Feedback Picture
tdiag2picture (Diag sp diag_) = return (tdiag2picture_ (diag2posdiag diag_))

diag2posdiag :: Diag_ -> PosDiag
diag2posdiag (Program p l) = PosProgram p l
diag2posdiag (Platform m) = PosPlatform  m
diag2posdiag (Interpreter i l m) = PosInterpreter (0, 0) i l m
diag2posdiag (Compiler c l1 l2 m) = PosCompiler (0, 0) c l1 l2 m
diag2posdiag (Execute d1 d2) = PosExecute (0, 0) d1 d2
diag2posdiag (Compile d1 d2) PosCompile (0, 0) d1 d2

offset :: (Int, Int) -> PosDiag -> PosDiag
offset (x, y) (PosProgram (x2, y2) p l) = PosProgram (x + x2, y + y2) p l
offset (x, y) (PosPlatform (x2, y2) m) = PosPlatform (x + x2, y + y2) m
offset (x, y) (PosInterpreter (x2, y2) i l m) = PosInterpreter (x + x2, y + y2) i l m
offset (x, y) (PosCompiler (x2, y2) c l1 l2 m) = PosCompiler (x + x2, y + y2) c l1 l2 m
offset (x, y) (PosExecute (x2, y2) d1 d2) = PosExecute (x + x2, y + y2) d1 d2
offset (x, y) (PosCompile (x2, y2) d1 d2) PosCompile (x + x2, y + y2) d1 d2

calculate_pos (x1, y1) (PosProgram (x2, y2) p l) = PosProgram (x1 + x2, y1 + y2) p l
calculate_pos (x1, y1) (PosPlatform (x2, y2) m) = PosPlatform (x1 + x2, y1 + y2) m
calculate_pos (x1, y1) (PosInterpreter (x2, y2) i l m) = PosInterpreter (x1 + x2, y1 + y2) i l m
calculate_pos (x1, y1) (PosCompiler (x2, y2) c l1 l2 m) = PosCompiler (x1 + x2, y1 + y2) c l1 l2 m
calculate_pos (x1, y1) (PosExecute (x2, y2) d1 d2) = h (calculate_pos d1) (calculate_pos d2)
													 where h d1a d2a = PosExecute d1a (offset ((getx d1a) + (getanchorx d1a), (gety d1a) + (getanchory d1a)) d2a)
calculate_pos (x1, y1) (PosCompile (x2, y2) d1 d2) = h (calculate_pos d1) (calculate_pos d2)
													 where h d1a d2a = PosCompile d1a (offset ((getx d1a) + (getanchorx d1a), (gety d1a) + (getanchory d1a)) d2a)


calculate_pos (x, y) (Execute d1 d2) = (x, y, (PosExecute (calculate_pos (x, y) d1)  ))
calculate_pos (x, y) p = (x, y, p)

tdiag2picture_ :: Diag_ -> Picture

tdiag2picture_ (Program p l) = [
	(Put (7.5, 0) (Line (1, 0) 50)),
	(Put (7.5, 0) (Line (0, 1) 15)),
	(Put (7.5, 15) (Line (-1, 2) 7.5)),
	(Put (57.5, 15) (Line (1, 2) 7.5)),
	(Put (57.5, 0) (Line (0, 1) 15)),
	(Put (0, 30) (Line (1, 0) 65)),
	(Put (7.5, 15) (Makebox (50, 15) p)),
	(Put (7.5, 0) (Makebox (50, 15) l))
	]

tdiag2picture_ (Platform m) = [
	(Put (0, 15) (Line(5,-3) 25)),
	(Put (25,0) (Line (5,3) 25)),
	(Put (0,15) (Line (0,1) 15)),
	(Put (0,30) (Line (1,0) 50)),
	(Put (50,30) (Line(0,-1) 15)),
	(Put (0,15) (Makebox (50,15) m))
	]

tdiag2picture_ (Interpreter i l m) = [
	(Put (0,0) (Framebox (50,30) "")),
	(Put (0,20) (Makebox (50,10) l)),
	(Put (0,10) (Makebox (50,10) i)),
	(Put (0,0) (Makebox (50,10) m))
	]

tdiag2picture_ (Compiler c l1 l2 m) = [
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
	]

tdiag2picture_ (Execute d1 d2) = d1 ++ d2
--tdiag2picture_ (Compile d1 d2) 



main = ioWrap $ CCO.Tree.parser >>> component toTree >>> component tdiag2picture >>> arr fromTree >>> printer