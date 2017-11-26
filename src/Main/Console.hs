-- |
-- Copyright   : (c) 2010, 2011 Benedikt Schmidt & Simon Meier
-- License     : GPL v3 (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Portability : GHC only
--
-- Support for interaction with the console: argument parsing.

{-# LANGUAGE TemplateHaskell #-}

module Main.Console (

    defaultMain

  -- * Static information about the Tamarin prover
  , programName

  -- * Constructing interaction modes for Tamarin prover
  , TamarinMode
  , tamarinMode

  , helpAndExit
  , unifiedHelpAndExit

  -- * Pretty printing and console output
  , lineWidth
  , shortLineWidth

  , renderDoc
  ) where

import           Data.Maybe
import           Data.Version                    (showVersion)
import           Data.Time
import           Safe

import           Control.Monad

import           System.Console.CmdArgs.Explicit
import           System.Console.CmdArgs.Text
import           System.Exit

import qualified Text.PrettyPrint.Class          as PP

import           Paths_tamarin_prover (version)

import           Language.Haskell.TH
import           Development.GitRev

import           Main.Flags

------------------------------------------------------------------------------
-- Static constants for the tamarin-prover
------------------------------------------------------------------------------

-- | Program name
programName :: String
programName = "tamarin-prover"

-- | Version string
versionStr :: String
versionStr = unlines
  [ concat
    [ programName
    , " "
    , showVersion version
    , ", (C) David Basin, Cas Cremers, Jannik Dreier, Simon Meier, Ralf Sasse, Benedikt Schmidt, ETH Zurich 2010-2017"
    ]
  , concat
    [ "Git revision: "
    , $(gitHash)
    , case $(gitDirty) of
          True  -> " (with uncommited changes)"
          False -> ""
    , ", branch: "
    , $(gitBranch)
    ]
  , concat
    [ "Compiled at: "
    , $(stringE =<< runIO (show `fmap` Data.Time.getCurrentTime))
    ]
  , ""
  , "This program comes with ABSOLUTELY NO WARRANTY. It is free software, and you"
  , "are welcome to redistribute it according to its LICENSE, see"
  , "'https://github.com/tamarin-prover/tamarin-prover/blob/master/LICENSE'."
  ]

-- | Line width to use.
lineWidth :: Int
lineWidth = 110

shortLineWidth :: Int
shortLineWidth = 78


------------------------------------------------------------------------------
-- Modes for using the Tamarin prover
------------------------------------------------------------------------------

-- | A representation of an interaction mode with the Tamarin prover.
data TamarinMode = TamarinMode
       { tmName        :: String
       , tmCmdArgsMode :: Mode Arguments
         -- ^ Run is given a reference to the mode. This enables changing the
         -- static information of a mode and keeping the same 'run' function.
         -- We use this for implementing the 'main' mode.
       , tmRun         :: TamarinMode -> Arguments -> IO ()
       , tmIsMainMode  :: Bool
       }

defaultMode = Mode
  { modeGroupModes = toGroup []
  , modeNames      = [programName]
  , modeValue      = []
  , modeCheck      = updateArg "mode" programName
  , modeExpandAt   = False
  , modeReform     = const Nothing-- no reform possibility
  , modeHelp       = "Automated verification of security protocols."
  , modeHelpSuffix = []
  , modeArgs       = ([], Nothing)   -- no positional arguments
  , modeGroupFlags = toGroup [] -- no flags
  }

-- | Smart constructor for a 'TamarinMode'.
tamarinMode :: String -> Help
            -> (Mode Arguments -> Mode Arguments) -- ^ Changes to default mode.
            -> (TamarinMode -> Arguments -> IO ())
            -> TamarinMode
tamarinMode name help adaptMode run0 = TamarinMode
  { tmName = name
  , tmCmdArgsMode = adaptMode $ defaultMode
  , tmRun        = run
  , tmIsMainMode = False
  }
  where
    run thisMode as
      | argExists "help"    as = unifiedHelpAndExit Nothing
      | argExists "version" as = putStrLn versionStr
      | otherwise              = run0 thisMode as

tamarinFlags = theoryLoadFlags ++
               toolFlags ++ 
               interactiveModeFlags ++ 
               batchModeFlags

unifiedHelpAndExit :: Maybe String -> IO ()
unifiedHelpAndExit mayMsg = do
    putStrLn $ showText (Wrap lineWidth)
             $ helpText header HelpFormatOne $ defaultMode 
                { modeArgs       = ([], Just $ flagArg (updateArg "workDir") "WORKDIR")
                , modeCheck      = updateArg "mode" "unified"
                , modeGroupFlags = Group tamarinFlags [] [("About", [helpFlag])]
                }
  where
    separator = replicate shortLineWidth '-'
    (header, end) = case mayMsg of
        Nothing  -> ([], return ())
        Just msg -> (["error: " ++ msg], exitFailure)

-- | Disply help message of a tamarin mode and exit.
helpAndExit :: TamarinMode -> Maybe String -> IO ()
helpAndExit tmode mayMsg = do
    putStrLn $ showText (Wrap lineWidth)
             $ helpText header HelpFormatOne (tmCmdArgsMode tmode)
    -- output example info
    when (tmIsMainMode tmode) $ do
      putStrLn $ unlines
        [ separator
        , "See 'https://github.com/tamarin-prover/tamarin-prover/blob/master/README.md'"
        , "for usage instructions and pointers to examples."
        , separator
        ]
    end
  where
    separator = replicate shortLineWidth '-'
    (header, end) = case mayMsg of
        Nothing  -> ([], return ())
        Just msg -> (["error: " ++ msg], exitFailure)

-- | Main function.
defaultMain :: TamarinMode -> [TamarinMode] -> IO ()
defaultMain firstMode otherModes = do
    as <- processArgs $ tmCmdArgsMode mainMode
    case findArg "mode" as of
      Nothing   -> error $ "defaultMain: impossible - mode not set"
      Just name -> headNote "defaultMain: impossible - no mode found" $ do
          tmode <- (mainMode : otherModes)
          guard (tmName tmode == name)
          return $ tmRun tmode tmode as
  where
    mainMode = firstMode
      { tmName        = programName
      , tmCmdArgsMode = (tmCmdArgsMode firstMode)
          { modeNames = [programName]
          , modeCheck      = updateArg "mode" programName
          , modeGroupModes = toGroup (map tmCmdArgsMode $ otherModes)
          , modeGroupFlags = (modeGroupFlags $ tmCmdArgsMode firstMode)
              { groupNamed =
                  [ ("About"
                    , [ helpFlag
                      , flagVersion (addEmptyArg "version")
                      ] )
                  ]
              }
          }
      , tmIsMainMode = True
      }


------------------------------------------------------------------------------
-- Pretty printing
------------------------------------------------------------------------------

-- | Render a pretty-printing document.
renderDoc :: PP.Doc -> String
renderDoc = PP.renderStyle (PP.defaultStyle { PP.lineLength = lineWidth })
