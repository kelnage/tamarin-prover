-- |
-- Copyright   : (c) 2010, 2011 Benedikt Schmidt & Simon Meier
-- License     : GPL v3 (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Portability : GHC only
--
-- Command line arguments
module Main.Flags (

  -- * Argument parsing
    Arguments
  , ArgKey
  , ArgVal

  -- ** Setting arguments
  , updateArg
  , addEmptyArg

  -- ** Retrieving arguments
  , getArg
  , findArg
  , argExists

  -- * The flags
  , helpFlag
  , toolFlags
  , theoryLoadFlags
  , interactiveModeFlags
  , batchModeFlags

  ) where

import           Data.Maybe

import           Control.Monad

import           System.Console.CmdArgs.Explicit

------------------------------------------------------------------------------
-- A simple generic representation of arguments
------------------------------------------------------------------------------

-- | A name of an argument.
type ArgKey = String

-- | A value of an argument.
type ArgVal = String

-- | It is most convenient to view arguments just as 'String' based key-value
-- pairs. If there are multiple values for the same key, then the left-most
-- one is preferred.
type Arguments = [(ArgKey,ArgVal)]

-- | Does an argument exist.
argExists :: String -> Arguments -> Bool
argExists a = isJust . findArg a

-- | Find the value(s) corresponding to the given key.
findArg :: MonadPlus m => ArgKey -> Arguments -> m ArgVal
findArg a' as = msum [ return v | (a,v) <- as, a == a' ]

-- | Find the value corresponding to the given key. Throw an error if no value
-- exists.
getArg :: ArgKey -> Arguments -> ArgVal
getArg a =
  fromMaybe (error $ "getArg: argument '" ++ a ++ "' not found") . findArg a

-- | Add an argument to the from of the list of arguments.
addArg :: ArgKey -> ArgVal -> Arguments -> Arguments
addArg a v = ((a,v):)

-- | Add an argument with the empty string as the value.
addEmptyArg :: String -> Arguments -> Arguments
addEmptyArg a = addArg a ""

-- | Update an argument.
updateArg :: ArgKey -> ArgVal -> Arguments -> Either a Arguments
updateArg a v = Right . addArg a v


-- | Add the help flag.
helpFlag :: Flag Arguments
helpFlag = flagNone ["help","h"] (addEmptyArg "help") "Display help message"

-- | Flags for handing over the path to the maude and graph rendering tool (dot or json).
toolFlags :: [Flag Arguments]
toolFlags =
  [ flagOpt "dot" ["with-dot"] (updateArg "withDot") "FILE" "Path to GraphViz 'dot' tool"
  , flagOpt "json" ["with-json"] (updateArg "withJson") "FILE" "Path to JSON rendering tool (not working with --diff)"
  , flagOpt "maude" ["with-maude"] (updateArg "withMaude") "FILE" "Path to 'maude' rewriting tool"
  ]

theoryLoadFlags :: [Flag Arguments]
theoryLoadFlags =
  [ flagOpt "" ["prove"] (updateArg "prove") "PREFIX"
      "Attempt to prove all the lemmas, or if specified, the lemmas with names starting with PREFIX"

  , flagOpt "dfs" ["stop-on-trace"] (updateArg "stopOnTrace") "DFS|BFS|NONE"
      "How to search for traces (default DFS)"

  , flagOpt "5" ["bound", "b"] (updateArg "bound") "INT"
      "Bound the depth of the proofs"

  , flagOpt "s" ["heuristic"] (updateArg "heuristic") "(s|S|o|O|p|P|l|c|C|i|I)+"
      "Sequence of goal rankings to use (default 's')"

  , flagOpt "summary" ["partial-evaluation"] (updateArg "partialEvaluation")
      "SUMMARY|VERBOSE"
      "Partially evaluate multiset rewriting system"

  , flagOpt "" ["defines","D"] (updateArg "defines") "STRING"
      "Define flags for pseudo-preprocessor."

  , flagNone ["diff"] (addEmptyArg "diff")
      "Turn on observational equivalence mode using diff terms."

  , flagNone ["quit-on-warning"] (addEmptyArg "quit-on-warning")
      "Strict mode that quits on any warning that is emitted."

--  , flagOpt "" ["diff"] (updateArg "diff") "OFF|ON"
--      "Turn on observational equivalence (default OFF)."
  ]


interactiveModeFlags :: [Flag Arguments]
interactiveModeFlags =
  [ flagOpt "" ["port","p"] (updateArg "port") "PORT" "Port to listen on"
  , flagOpt "" ["interface","i"] (updateArg "interface") "INTERFACE"
            "Interface to listen on (use '*4' for all IPv4 interfaces)"
  , flagOpt "" ["image-format"] (updateArg "image-format") "PNG|SVG" "image format used for graphs (default PNG)"
  , flagNone ["debug"] (addEmptyArg "debug") "Show server debugging output"
  -- , flagNone ["autosave"] (addEmptyArg "autosave") "Automatically save proof state"
  -- , flagNone ["loadstate"] (addEmptyArg "loadstate") "Load proof state if present"
  ]

batchModeFlags :: [Flag Arguments]
batchModeFlags =
  -- [ flagNone ["html"] (addEmptyArg "html")
  --     "generate HTML visualization of proofs"
  [ flagNone ["no-compress"] (addEmptyArg "noCompress")
      "Do not use compressed sequent visualization"
  , flagNone ["parse-only"] (addEmptyArg "parseOnly")
      "Just parse the input file and pretty print it as-is"
  , flagOpt "" ["output","o"] (updateArg "outFile") "FILE" "Output file"
  , flagOpt "" ["Output","O"] (updateArg "outDir") "DIR"  "Output directory"
  ]
