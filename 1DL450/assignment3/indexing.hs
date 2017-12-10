import Control.Monad.Par.IO (runParIO)
import Control.Parallel.Strategies (rpar, rseq, parList, using)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Par.Combinator (parMapM)

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy as LL
import qualified Data.ByteString.Char8 as B

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.IntSet as Set
import Data.IntSet (IntSet)

import System.Environment
import System.Exit
import System.IO
import Data.Array
import Data.Char
import Control.Monad

-- A document index and search program.  Use it like this:
--
-- $ ./index docs/*
-- search: <enter search term here>
-- docs/file1
-- docs/file2
-- docs/file3
--
--

-- Documents are numbered by the order they appear on the command line
type DocSet = IntSet

-- A DocIndex maps a word to the set of documents that contain the word
type DocIndex = Map B.ByteString DocSet

joinIndices :: [DocIndex] -> DocIndex
joinIndices = foldr (Map.unionWith Set.union) Map.empty

mkIndex :: Int -> L.ByteString -> DocIndex
mkIndex i s
  = Map.fromListWith Set.union [ (B.concat (L.toChunks w), Set.singleton i)
                               | w <- ws ] --`using` rpar
  where ws = L.splitWith (not . isAlphaNum) s

search :: DocIndex -> [B.ByteString] -> DocSet
search index words = foldr1 Set.intersection (map lookup words)
  where lookup w = Map.findWithDefault Set.empty w index --`using` rpar

-- -----------------------------------------------------------------------------

buildIndex :: (Int, FilePath) -> IO DocIndex
buildIndex (n, fs) = do
  s <- L.readFile fs
  let index = mkIndex n s
  return index

buildIndices :: [FilePath] -> IO [DocIndex]
buildIndices fs = do
  indices <- runParIO $ parMapM (\x -> liftIO $ buildIndex x) (zip [0..] fs)
  return indices

searchForWord :: B.ByteString -> [DocIndex] -> Array Int String -> [String]
searchForWord s indices arr =
  let results = map ((flip search) (B.words s)) indices `using` parList rseq
      results' = map Set.toList results `using` parList rseq
      results'' = concat results' -- `using` parList rseq
  in
    map (arr !) results''

main = do
  hSetBuffering stdout NoBuffering
  fs <- getArgs

  indices <- buildIndices fs
  -- array mapping doc number back to filename
  let arr :: Array Int String
      arr = listArray (0,length fs - 1) fs

  forever $ do
    putStr "search (^D to end): "
    eof <- isEOF
    when eof $ exitWith ExitSuccess
    s <- B.getLine
    putStr "wait... "

    let files = searchForWord s indices arr

    putStrLn ("\n" ++ unlines files)
