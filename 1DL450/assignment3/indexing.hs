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

-- We do not join the indices since we want to be able to parallelize the search
-- joinIndices :: [DocIndex] -> DocIndex
-- joinIndices = foldr (Map.unionWith Set.union) Map.empty

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
--  indices <- sequence (map buildIndex (zip [0..] fs))
  indices <- runParIO $ parMapM (\x -> liftIO $ buildIndex x) (zip [0..] fs)
  return indices

searchForWords :: B.ByteString -> [DocIndex] -> [Int]
searchForWords s indices =
  let result = map ((flip search) (B.words s)) indices --`using` parList rseq
      result' = map Set.toList result --`using` parList rseq
--  let result' = map Set.toList (map ((flip search) (B.words s)) indices) `using` parList rseq
  in
    concat result'
    
main = do
  hSetBuffering stdout NoBuffering
  fs <- getArgs

  -- indices is a separate index for each (numbered) document
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

    let result :: [Int]  -- indices of docs containing the words in the term
        result = searchForWords s indices

        -- map the result back to filenames
        files = map (arr !) result

    putStrLn ("\n" ++ unlines files)
