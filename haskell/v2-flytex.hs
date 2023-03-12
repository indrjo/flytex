#!/usr/bin/env runghc

module Main where

-- As you can see from the list of imports below, there are few modules the
-- program requires to work properly.
--
-- !!! Write an appropriate .cabal file.
-- !!! In *.cabal specify:
-- !!!  base, process, options, regex-pcre
-- !!!
-- !!! No particular version?

import Control.Monad (filterM)
import Control.Conditional (when, unless)
import Data.List (isSuffixOf, intercalate, (\\))
import Data.Either
import qualified Text.Regex.PCRE.Light.Char8 as PCRE
import System.Process (readCreateProcessWithExitCode, shell)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stdout, stderr, Handle)
import Options

-- ------------------------------------------------------------------------
-- 0. THE MAIN
-- ------------------------------------------------------------------------

-- Starting in medias res, the main amounts at two big pieces: one is the 
-- function flytex and the other is makeTeXCommand. Roughly speaking, the 
-- latter is the command to be issued to the host system, whereas the first
-- is the core of the program: while texing, needed but abset packages are
-- installed "on the fly".
main :: IO ()
main = makeTeXCommand >>= either flytexSaysError flytex


-- ------------------------------------------------------------------------
-- # MAKE THE UNDERLYING SYSTEM DO THINGS
-- ------------------------------------------------------------------------

-- There ought be no surprise if we say this program intimately relies on
-- the OS that hosts this program. Our ideal user, as we have already said,
-- is a GNU/Linux user, or even a *nix one. Thus we need a function that
-- sends commands to the system and makes them run. The arguments are:
--  * a string that announces the execution of a shell command
--  * the actual shell command as a string
--  * a string to be sent via the standard input, just in case the user is
--    expected to take action.
-- The output is a triple: the exit code and two strings containing the
-- standard output and error. See remarks below.
exec :: String -> String -> String -> IO (ExitCode, String, String)
exec intro comm instr = do
  -- !!! Sometimes it may be of comfort for the user to have some witness
  -- !!! of life from the program. Sometimes if is not. How to decide?
  unless (null intro) (flytexSays intro)
  -- The interface of Haskell for interacting with the underlying operating
  -- system is summoned: just two functions! This is a delicate place and
  -- strongly depends on the platform where the program runs. Extreme care.
  readCreateProcessWithExitCode (shell comm) instr


-- ------------------------------------------------------------------------
-- # INVOKING TLMGR
-- ------------------------------------------------------------------------

-- A minimal TeX Live has *tlmgr* to handle packages: not only you install
-- packages with it, but you can search packages containing a given file!
-- They are both interesting for our purpose.

-- Installing packages is the simpler part here, you just have to type
--  $ tlmgr install PACKAGE
-- and wait tlmgr to end. Take as input the name of a package you want to 
-- install, attempt to do it and eventually return a boolean value, with
-- True indicating that the installation has ended successfully and False
-- if otherwise.
tlmgrInstall :: String -> IO Bool
tlmgrInstall pkg =
  exec ("installing " ++ pkg ++ "...")
       ("tlmgr install " ++ pkg) "" >>=
    \(exit_code, _, _) -> return $ exit_code == ExitSuccess
    -- !!! This function ignores completely any output of `tlmgr install`.

-- It is best we provide a function to perform multiple installations. For
-- a list of packages, try to install all them. The output is a list of all
-- the packages for which the installation has failed for some reason.
tlmgrMultipleInstalls :: [String] -> IO [String]
tlmgrMultipleInstalls = filterM (fmap not . tlmgrInstall)

-- Let us turn our focus on searching packages now. To do so, let us start
-- from a descriptive example.
--
-- | $ tlmgr search --global --file caption.sty
-- | tlmgr: package repository [...]
-- | caption:
-- | 	 texmf-dist/tex/latex/caption/bicaption.sty
-- | 	 texmf-dist/tex/latex/caption/caption.sty
-- | 	 texmf-dist/tex/latex/caption/ltcaption.sty
-- | 	 texmf-dist/tex/latex/caption/subcaption.sty
-- | ccaption:
-- | 	 texmf-dist/tex/latex/ccaption/ccaption.sty
-- | lwarp:
-- | 	 texmf-dist/tex/latex/lwarp/lwarp-caption.sty
-- | 	 texmf-dist/tex/latex/lwarp/lwarp-ltcaption.sty
-- | 	 texmf-dist/tex/latex/lwarp/lwarp-mcaption.sty
-- | 	 texmf-dist/tex/latex/lwarp/lwarp-subcaption.sty
-- | mcaption:
-- | 	 texmf-dist/tex/latex/mcaption/mcaption.sty
--
-- The first line just tells the repository interrogated, we do not care of
-- of that here. The other lines are the ones very interesting: there is a
-- sequence of
-- 
--  package:
--    path1
--    path2
--    ...
--    pathN
--
-- In our example, the paths end with `caption.sty`. But we want only the
-- file `caption.sty` and not, say, `ccaption.sty`. This problem can be 
-- easily solved putting a "/", as follows:
--
-- | $ tlmgr search --global --file /caption.sty
-- | tlmgr: package repository [...]
-- | caption:
-- | 	 texmf-dist/tex/latex/caption/caption.sty
--
-- Thus part of the work is to extract from such lines only the names of 
-- the packages containing the given file: concretely, this means to filter
-- the lines ending with ":".
findPackages :: [String] -> [String]
findPackages = map init . filter (isSuffixOf ":")

-- Make tlmgr look for packages containing the given file. Take a string,
-- a filename, and search for packages containing it. If there are packages
-- for that file return Right (that list), else return Left (the filename).
-- The reason behind such output lies on the next function.
tlmgrSearch :: String -> IO (Either String [String])
tlmgrSearch fname =
    exec "" ("tlmgr search --global --file /" ++ fname) "" >>=
      \(exit_code, out_str, _) ->
        return $ case exit_code of
          ExitSuccess ->
          -- In case of exit code equal to 0, get rid of the first line of
          -- the standard output (see the example above) and scrape the
          -- remaining lines if there are some.
            case lines out_str of
              _:out_lns' ->
                let pkgsFound = findPackages out_lns' in
                  if null pkgsFound then Left fname else Right pkgsFound  
              _ -> Left fname
          -- Otherwise, just collect all the error message, to be presented
          -- to the user in future.
          ExitFailure _ -> Left fname

-- Given a list of filenames, look for packages containing them. It is 
-- returned a pair of lists:
--  * the first component is the list of the filenames for which there is 
--    no package, the "orphans"
--  * the second component is the list of all the packages found.
tlmgrMultipleSearches :: [String] -> IO ([String], [String])
tlmgrMultipleSearches fnames = -- do
  {- flytexSays $ "searching packages for: "
               ++ intercalate ", " fnames -}
  mapM tlmgrSearch fnames >>=
    \eithers -> return (lefts eithers, concat (rights eithers))


-- ------------------------------------------------------------------------
-- # FIND THE MISSING PACKAGES
-- ------------------------------------------------------------------------

-- Assume now you run something like
--
--    $ lualatex main.tex
--
-- There will be a detailed output. In particular, if a required package is
-- missing, the TeX engine will complain:
--
--    ! LaTeX Error: File `FILENAME' not found.
--
-- Such lines are for flytex! We will use Perl-style regular expressions to
-- isolate such lines from the output.

-- The function that isolates all such names. The unique argument is the 
-- output in one string, whereas the output is the list of all filenames 
-- found throughout the output.
filterMissings :: String -> [String]
filterMissings = matchAll "! (?:La)*TeX Error: File `([^\']+)\' not found."

-- The functions doing all the hard work behind.
match :: String -> String -> [String]
match pat str = maybe [] id $
  PCRE.match (PCRE.compile pat [PCRE.multiline]) str []

matchAll :: String -> String -> [String]
matchAll pat str =
  case match pat str of
    [pre, captured] ->
      captured : matchAll pat (str \\ pre)
    _ -> []


-- ------------------------------------------------------------------------
-- 3. PREPARE THE COMMAND TO BE RUN
-- ------------------------------------------------------------------------

-- For future readability and changes, let us take advantage of unnecessary
-- type synonyms.
type TeXProgram = String   -- the path of the TeX binary to be invoked
type TeXOptions = String   -- the options passed to the program above
type FileToTeX  = String   -- the *.tex all the preceding stuff applies  

-- Here the command to issue to the system with its Show instance.
data TeXCommand = TeXCommand TeXProgram TeXOptions FileToTeX

instance Show TeXCommand where
  show (TeXCommand prog opts fp) =
    unwords $ if null opts then [prog, fp] else [prog, opts, fp]

-- This is the point where commandline arguments enters the scene.
-- This program supports only three options:
--
--   # the TeX program to be used     [mandatory, no default!]
--   # the options to be passed to it [default: ""]
--   # the file to be TeX-ed.         [mandatory, of course]
--
-- For the future: maybe insert more useful defaults here.

-- Options for flytex...
data MainOptions = MainOptions
  {
      optTeXProgram :: String
    , optTeXOptions :: String
    , optTeXFile    :: Maybe String
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "c" "pdflatex"
          "indicate the TeX engine you intend to use"
    <*> simpleOption "o" ""
          "option to pass to the TeX engine [default: \"\"]"
    <*> simpleOption "i" Nothing
          "the file you want to be TeX-ed"

getTeXProgram :: IO String
getTeXProgram = option optTeXProgram

getTeXOptions :: IO String
getTeXOptions = option optTeXOptions

getTeXFile :: IO (Maybe String)
getTeXFile = option optTeXFile

option :: Options o => (o -> a) -> IO a
option f = runCommand $ \opts _ -> return (f opts)

-- Read the command line options passed to the program and either create a
-- TeXCommand to be issued to the system or present a complaint.
makeTeXCommand :: IO (Either String TeXCommand)
makeTeXCommand =
  getTeXProgram >>=
    \program ->
      getTeXOptions >>=
        \options ->
          getTeXFile >>=
            maybe (return $ Left "No file to TeX provided!")
              (\path ->
                  return $ Right $ TeXCommand program options path)


-- ------------------------------------------------------------------------
-- # TEX LIVE ON THE FLY
-- ------------------------------------------------------------------------

-- This is the main function. The type signature is not definitive.
flytex :: TeXCommand -> IO ()
flytex texCmd@(TeXCommand compiler options fname) =
  -- Compile the given TeX file. Observe the trick of passing a infinite
  -- string made of '\n' as input to the command executed: every time it is
  -- encountered an error as this
  -- 
  --  | ! LaTeX Error: File `libertine.sty' not found.
  --  |
  --  | Type X to quit or <RETURN> to proceed,
  --  | or enter new name. (Default extension: sty)
  --  |
  --  | Enter file name:
  --
  -- with a request of manual action from the user, it written '\n' which
  -- tells the TeX engine employed to go on although the error. The trick
  -- works fine for errors in general, since it is always given the chance
  -- to manually correct errors during the compilation. For example:
  --
  --  | ! <text of the error>
  --  | ...
  --  | ?
  --
  -- allows the user to type something after '?'.
  exec ("compiling " ++ fname ++ "..." )
       (unwords [compiler, options, fname]) (cycle "\n") >>=
    \(_, out_str, _) ->
      let missings = filterMissings out_str in
        case missings of
          [] -> flytexSays "end!"
          _ -> do
            flytexSays $ "searching packages for: "
                         ++ intercalate ", " missings
            tlmgrMultipleSearches missings >>=
              \(orphans, pkgsFound) -> do
                unless (null orphans) $
                  flytexSaysError $ "there are orphans: " ++
                                    intercalate ", " orphans
                case pkgsFound of
                  [] ->
                    flytexSaysError "I cannot retrieve packages!"
                  _ ->
                    tlmgrMultipleInstalls pkgsFound >>=
                      \fails -> case fails of
                        [] -> do
                          when (null orphans) (flytex texCmd)
                        _ ->
                          flytexSaysError $ "failed to install: "
                                            ++ intercalate ", " fails


-- ------------------------------------------------------------------------
-- # HOW THE PROGRAM COMMUNICATES
-- ------------------------------------------------------------------------

-- !!! No fatal massages here, that is no message will abort the execution
-- !!! of flytex. This feature may change or not, but for now that's it.

-- The general way, to say things.
say :: Handle -> String -> String -> IO ()
say hdl who txt = hPutStrLn hdl $ "[" ++ who ++ "] " ++ txt

flytexSays, flytexSaysError :: String -> IO ()
flytexSays      = say stdout "flytex"
flytexSaysError = say stderr "flytex-error"

tlmgrSays, tlmgrSaysError :: String -> IO ()
tlmgrSays      = say stdout "tlmgr"
tlmgrSaysError = say stderr "tlmgr-error"

