{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FileIO where

import           Course.Applicative
import           Course.Core
import           Course.Functor
import           Course.List
import           Course.Monad

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

To test this module, load ghci in the root of the project directory, and do
    >> :main "share/files.txt"

Example output:

$ ghci
GHCi, version ...
Loading package...
Loading ...
[ 1 of 28] Compiling (etc...
...
Ok, modules loaded: Course, etc...
>> :main "share/files.txt"
============ share/a.txt
the contents of a

============ share/b.txt
the contents of b

============ share/c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main :: IO ()
main = (run . headOr Nil) =<< getArgs

type FilePath = Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run path = printFiles =<< getFiles . lines . snd =<< getFile path

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles paths = sequence $ map getFile paths

-- Read a file from the given path and return a pair with the path and file contents
getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile path = (\c -> return (path, c)) =<< readFile path

-- Call printFile for each path content pair in the list
printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles li = void $ sequence $ map (uncurry printFile) li

-- Print given path and contents
printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile path content = do
  putStrLn $ "========= " ++ path
  putStrLn content
