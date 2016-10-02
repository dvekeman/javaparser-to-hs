{-# LANGUAGE StandaloneDeriving #-}

import Control.Monad ( liftM )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import Data.List ( find )

import Language.Java.Syntax
import Language.Java.Pretty
import Language.Java.Lexer

import System.Environment
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import System.Directory.Tree
import System.FilePath.Posix

import Data.Typeable

--------------------------------------------------------------------------------

parseJava :: FilePath -> FilePath -> IO ()
parseJava newpath oldpath
  | ".java" == takeExtension oldpath = do
    res <- parse oldpath
    case res of
         Left _     -> do
           putStrLn $ "Unable to parse " ++ oldpath
           contents <- B.readFile oldpath
           B.writeFile newpath contents
           putStrLn $ "Wrote unparsed file to " ++ newpath
         Right tree -> do 
                         putStrLn $ "Tree: " ++ show (typeOf tree)
                         let reread = map unL $ lexer $ show $ pretty tree 
                         let pTree = pretty tree
                             contents = show $ pTree
                         putStrLn $ contents
                         writeFile newpath $ contents
                         putStrLn $ "Wrote parsed file to " ++ newpath
  | otherwise = do
    contents <- B.readFile oldpath
    B.writeFile newpath contents

parse :: FilePath -> IO (Either String CompilationUnit)
parse path = withSystemTempFile "parse" $ \tmp h -> do
                         hClose h
                         exitCode <- system $ "java -jar lib/javaparser-to-hs.jar " ++ (show path) ++ " " ++ (show tmp)
                         case exitCode of
                           ExitFailure _ -> return $ Left "parse failed"
                           ExitSuccess   -> do
                                              print tmp
                                              tmpcontents <- readFile tmp
                                              print tmpcontents
                                              liftM (Right . read) $ readFile tmp

{-
Parsing can rearrange modifiers, pretty-printing can insert parens, annotations are dropped. What do we do? Remove them!
-}
lexicalDifference :: [Token] -> [Token] -> Maybe (Token, Token)
lexicalDifference xs ys = let xs' = [x | x <- removeAnno xs, not (x `elem` unstableTokens)]
                              ys' = [y | y <- removeAnno ys, not (y `elem` unstableTokens)] in
                          find substantiveDiff (zip xs' ys')
  where
    substantiveDiff (IntTok x, LongTok y) = x /= y
    substantiveDiff (LongTok x, IntTok y) = x /= y
    substantiveDiff (FloatTok x, DoubleTok y) = x /= y
    substantiveDiff (DoubleTok x, FloatTok y) = x /= y
    substantiveDiff (x, y)                = x /= y

    removeAnno ts = rAnn False 0 ts

    rAnn True 0 ts                = rAnn False 0 ts
    rAnn True n (CloseParen : ts) = rAnn True (n-1) ts
    rAnn True n (OpenParen : ts)  = rAnn True (n+1) ts
    rAnn True n (t:ts)            = rAnn True n ts

    rAnn False n (Op_AtSign : t : OpenParen : ts) = rAnn True (n+1) ts
    rAnn False n (Op_AtSign : t : ts)         = rAnn False n ts
    rAnn False n (t : ts)                     = t : rAnn False n ts
    rAnn False 0 []                           = []

    unstableTokens = [ OpenParen
                      , CloseParen
                      , KW_Public
                      , KW_Private
                      , KW_Protected
                      , KW_Abstract
                      , KW_Final
                      , KW_Static
                      , KW_Strictfp
                      , KW_Transient
                      , KW_Volatile
                      , KW_Native
                      , KW_Synchronized
                      , Comma
                      , SemiColon
                      ]

unL :: L a -> a
unL (L _ x) = x

dumpParsedFile :: String -> FilePath -> IO ()
dumpParsedFile content fp = writeFile fp content

copyDirectory path newpath = do
  orig <- readDirectoryWith return path
  -- writeDirectory (newpath :/ dirTree orig)
  writeDirectoryWith (\newpath oldpath -> parseJava newpath oldpath) $ (newpath :/ dirTree orig)

main :: IO ()
main = do (inputDir:outputDir:_) <- getArgs
          copyDirectory inputDir outputDir
          putStrLn "Done"

-- main :: IO ()
-- main = do fil <- liftM head $ getArgs
--           origStream <- liftM (map unL.lexer) $ readFile fil
--           res <- parse fil
--           case res of
--                Left _     -> exitFailure
--                Right tree -> let reread = map unL $ lexer $ show $ pretty tree in
--                          do  let contents = show $ pretty tree
--                              -- putStrLn contents
--                              dumpParsedFile contents (fil ++ ".out")
--                              case lexicalDifference reread origStream of
--                                Nothing -> do putStrLn (fil ++ ": No differences") >> return ()
--                                Just x  -> do putStrLn $ "Different: " ++ show x
--                                              putStrLn $ show $ pretty tree
--                                              exitFailure
