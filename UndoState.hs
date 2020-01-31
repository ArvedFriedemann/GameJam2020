module UndoState where

import Control.Monad.State.Lazy

type UndoState s = State ([s],[s],s)

getU::UndoState s s
getU = do {
  (_,_,s) <- get;
  return s
}

putU:: s -> UndoState s ()
putU newS = do {
  (preStack, postStack, s) <- get;
  put (s:preStack, postStack, newS)
}

--returns undone state (or current one if cannot be undone)
undo:: UndoState s s
undo = do {
  (preStack, postStack, s) <- get;
  case preStack of
    (x:xs) -> put (xs, s:postStack, x) >> return s
    [] -> return s
}

--returns redone state (or current one if cannot be redone)
redo:: UndoState s s
redo = do {
  (preStack, postStack, s) <- get;
  case postStack of
    (x:xs) -> put (s:preStack, xs, x) >> return s
    [] -> return s
}
