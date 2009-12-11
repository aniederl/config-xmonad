-- {-# OPTIONS_GHC -cpp #-}
-- {-
-- #include <X11/XF86keysym.h>
-- -}

import XMonad

import qualified Data.Map as M
import qualified XMonad.StackSet as W


import XMonad.Actions.Promote
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.SpawnOn

-- mouse
import XMonad.Actions.MouseResize
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp

import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.SimpleFloat
import XMonad.Layout.WindowArranger
import XMonad.Layout.Decoration
import XMonad.Layout.Maximize
import XMonad.Layout.ResizableTile
import XMonad.Layout.Named
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Workspace

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Scratchpad

import Data.Ratio
import Data.List
import Data.Maybe
import Data.Monoid ( mappend )

import Text.Regex.Posix

import System.Exit

import XMonad.StackSet (view, greedyView, tag, hidden, stack)


myFont    :: String
myBgColor :: String
myFgColor :: String

myFont = "xft:DejaVu Sans Mono:size="

myBgColor = "black"
myFgColor = "blue"

titleFontSize = "8"
promptFontSize = "14"

myTerminal = "urxvt"

myHome = "/home/andi"

myTheme = defaultTheme
    { fontName = myFont ++ titleFontSize
    }

myXPConfig = defaultXPConfig
    { font              = myFont ++ promptFontSize,
      bgColor           = myBgColor,
      fgColor           = myFgColor,
      bgHLight          = myFgColor,
      fgHLight          = myBgColor,
      borderColor       = myBgColor,
      promptBorderWidth = 0,
      position          = Top,
      defaultText       = ""
    }

myShellXPConfig = myXPConfig
    { -- autocomplete after 1 sec
      autoComplete      = Just 100000
    }

myNoteXPConfig = myXPConfig
    { position = Bottom }


floatSimple :: (Show a, Eq a) => ModifiedLayout (Decoration DefaultDecoration DefaultShrinker)
                      (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
floatSimple = decoration shrinkText myTheme DefaultDecoration (mouseResize $ windowArrangeAll $ SF 20)


myWorkspaces  = [ "term", "mail", "web", "web 2" ] ++ map show [5 .. 9 :: Int]

myLayout =
         layoutHints
         $ avoidStruts
         $ smartBorders
         $ mkToggle1 NBFULL
         $ maximize
         $ onWorkspace "term" (tiled ||| Mirror tiled ||| Full)
         $ Full
                     ||| tiled
                     ||| Mirror tiled
    where
      tiled   = named "Tall" $ (ResizableTall nmaster delta ratio [])
      nmaster = 1
      ratio   = 1/2
      delta   = 3/100


scratchpadWorkspaceTag = "NSP"

delKeys = []
insKeys =
    [ ("M-<Return>",        promote)
    , ("M-v",               sendMessage ToggleStruts)
    , ("M-g",               sendMessage $ ToggleStrut L)
    , ("M-h",               moveTo Prev (WSIs $ do ne <- return (isJust . stack)
                                                   ns <- return ((scratchpadWorkspaceTag /=) . tag)
                                                   return (\w -> ne w && ns w)))
    , ("M-l",               moveTo Next (WSIs $ do ne <- return (isJust . stack)
                                                   ns <- return ((scratchpadWorkspaceTag /=) . tag)
                                                   return (\w -> ne w && ns w)))
    , ("M-S-h",             shiftToPrev)
    , ("M-S-l",             shiftToNext)
    , ("M-C-S-h",           shiftToPrev >> prevWS)
    , ("M-C-S-l",           shiftToNext >> nextWS)

    -- cycle throug recent workspaces with alt-tab
    , ("M-<Tab>",           cycleRecentWS [xK_Alt_L] xK_Tab xK_grave)

    -- resize master pane
    , ("M-z",               sendMessage Shrink)
    , ("M-x",               sendMessage Expand)

    , ("M-C-<Space>",       sendMessage $ Toggle NBFULL)

    -- banish the mouse pointer into bottom right corner
    , ("M-b",               banish LowerRight)

    -- toggle to last workspace (like C-a C-a in screen)
    , ("M-a",               (windows $ view =<< tag . head . (filter (\(W.Workspace tag _ _) -> tag /= scratchpadWorkspaceTag)) . hidden))
    , ("M-;",               (windows $ view =<< tag . head . (filter (\(W.Workspace tag _ _) -> tag /= scratchpadWorkspaceTag)) . hidden))

    -- close only focused copy (kill if last)
    , ("M-S-c",             kill1)

    , ("C-M-l",             spawn "xscreensaver-command -lock")
    , ("M-c",               withFocused (sendMessage . maximizeRestore))
    --, ("M-S-p",             prompt (myTerminal ++ " -e") myShellXPConfig) -- no completion
    , ("M-S-p",             spawn ("exe=`dmenu_path | dmenu -nb '#000000' -nf '#CCCCCC'` && eval \"exec " ++ myTerminal ++ " -e $exe\""))

    -- note taking
    , ("M-n",               appendFilePrompt myNoteXPConfig (myHome ++ "/.notes"))

    -- workspace prompt
    , ("M-o",               workspacePrompt myShellXPConfig (windows . W.view))
    , ("M-S-o",             workspacePrompt myShellXPConfig (windows . W.shift))

    -- scratchpad terminal with a screen session
    -- need to add '-name' as first argument or else urxvt won't use it
    , ("M-s",               scratchpadSpawnActionTerminal ((terminal myConfig) ++ " -name scratchpad -e $SHELL -c 'screen -c ~/.xmonad/screenrc-scratchpad -D -R scratchpad'"))

    , ("M-e",               spawn myTerminal)
    --, ("M-n",               refresh)
    , ("M-C-S-q",           io (exitWith ExitSuccess))
    , ("M-C-<Home>",        spawn "mpc toggle")
    , ("M-C-<End>",         spawn "mpc stop")
    , ("M-C-<Page_Up>",     spawn "mpc prev")
    , ("M-C-<Page_Down>",   spawn "mpc next")
    ]
    ++
    -- This enables view switching, window shifting, and window copying
             [("M" ++ m ++ ('-':k:[]) , windows $ f i)
                  | (i, k) <- zip myWorkspaces ['1'..'9']
                  , (f, m) <- [(W.view, ""), (W.shift, "-S"), (copy, "-C-S")]]


multimediaKeys =
        [ ("XF86AudioLowerVolume", unsafeSpawn "amixer -q set Master 5%-")
        , ("XF86AudioMute",        unsafeSpawn "amixer -q set Master toggle")
        , ("XF86AudioRaiseVolume", unsafeSpawn "amixer -q set Master 5%+")
        , ("XF86AudioPlay",        spawn "mpc toggle")
        , ("XF86AudioStop",        spawn "mpc stop")
        , ("XF86AudioPrev",        spawn "mpc prev")
        , ("XF86AudioNext",        spawn "mpc next")
        ]

delButtons = []
insButtons = []

myGestures = M.fromList
        [ ( [],                 focus)
        , ([R, D, L, U],        \w -> spawn "xscreensaver-command -lock")
        ]

myManageHook = composeAll $
                -- auto float
               [ className =? c --> doCenterFloat  | c <- myClassFloats ]
               ++
               [ title     =? t --> doCenterFloat  | t <- myTitleFloats ]
               ++
                -- ignore
               [ resource  =? r --> doIgnore | r <- myIgnores ]
               ++
                -- shifts
               [ className =? c --> doF (W.shift ws) | (c, ws) <- myShifts ]
               ++
               [ title     =? p --> doF (W.shift ws) | (p, ws) <- myTerminalShifts ]
               ++
                -- other hooks
               [ manageDocks
               , scratchpadManageHookDefault
               ]
    where moveToC c w = className =? c --> doF (W.shift w)
          moveToT t w = title     =? t --> doF (W.shift t)
          floatC  c   = className =? c --> doFloat
          myClassFloats = [ "Gimp", "gimp", "Xmessage", "feh", "Display", "MPlayer", "Kdiff3", "Audacious" ] --"MPlayer",
          myTitleFloats = [ "Downloads", "Firefox Preferences", "Thunderbird Preferences", "Save As...",
                            "Preferences...", "Confirm...", "Connect via URL", "Enter Password", "Password Required", "Transfer Files", "Rename",
                            "Make Directory", "Delete Files/Directories", "Getting directory listings", "Options", "Chmod", "Add Bookmark",
                            "Edit Bookmarks", "Delete Bookmarks", "Save Playlist", "GQview Preferences", "Inkscape Preferences (Shift+Ctrl+P)",
                            "Select file to open", "Select file to save to", "Warning", "Closing Project - K3b", "Open Files - K3b", "Options - K3b",
                            "Add Config setting", "Edit Config setting", "Close Nicotine-Plus?", "Nicotine Settings", "OpenOffice.org 2.0", "Open",
                            "Options - OpenOffice.org - User Data", "File Properties", "Preference", "Session Manager - Mozilla Firefox",
                            "Session Manager Options", "Firefox Add-on Updates", "Message Filters", "Event Tester", "Pinentry" ]
          myIgnores = []
          myShifts =  zip [ "Opera", "Firefox" ] (repeat "web")
                   ++ zip [ "Thunderbird-bin" ] (repeat "mail")
                   ++ zip ["Amarokapp", "amarokapp", "Ario"] (repeat "music")
          myTerminalShifts = zip ["newsbeuter", "slrn", "mutt", "centerim"] (repeat "mail")

myBitmapsDir = "/home/andi/.dzen/bitmaps/dzen"
myPP h = defaultPP
    { ppCurrent = wrap ("^fg(#FFFFFF)^bg(#647A90)^p(2)^i(" ++ myBitmapsDir ++ "/has_win.xbm)") "^p(2)^fg(grey55)^bg()"
    , ppVisible = wrap ("^bg(grey30)^fg(grey75)^p(2)") "^p(2)^fg(grey55)^bg()"
    , ppSep     = " ^fg(grey60)^r(3x3)^fg() "
    , ppLayout  = dzenColor "#647A90" "" .
        (\x -> case x of
                    "Tall" -> "tall ^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                    "Hinted Tall" -> "tall ^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                    "Hinted Maximize Tall" -> "tall ^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                    "Mirror Tall" -> "mirror ^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                    "Hinted Mirror Tall" -> "mirror ^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                    "Hinted Maximize Mirror Tall" -> "mirror ^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                    "Hinted Wide" -> "mirror ^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                    "Full" -> "full ^i(" ++ myBitmapsDir ++ "/full.xbm)"
                    "Hinted Full" -> "full ^i(" ++ myBitmapsDir ++ "/full.xbm)"
                    "Hinted Maximize Full" -> "full ^i(" ++ myBitmapsDir ++ "/full.xbm)"
                    "Hinted Circle" -> "circle"
                    "Grid" -> "grid"
                    "Hinted Magnifier Grid" -> "grid"
                    "Tabbed" -> "tabbed"
                    _        -> pad x
                    )
    , ppTitle   = dzenColor "white" "" . dzenEscape . wrap "< " " >" -- . shorten 50
    , ppOutput  = hPutStrLn h
    , ppSort    = fmap (.scratchpadFilterOutWorkspace) $ ppSort defaultPP
    --, ppExtras = [ pprWindowSet topicconfig myPP ]
    }

statusBarCmd = "dzen2 -bg '#000000' -fg '#FFFFFF' -h 16 -fn '-xos4-terminus-*-*-*-*-14-*-*-*-*-*-*-*' -sa c -e '' -ta l" -- -w 800"
--statusBarCmd = "dzen2 -bg '#000000' -fg '#FFFFFF' -h 16 -fn 'xft:DejaVu Sans:size=14' -sa c -e '' -ta l" -- -w 800"

logBarCmd = "inotail -f -n 30 /var/log/messages | dzen2 -e 'entertitle=uncollapse;leavetitle=collapse' -bg '#000000' -fg '#FFFFFF' -h 16 -fn '-xos4-terminus-*-*-*-*-14-*-*-*-*-*-*-*' -sa c -e '' -ta l -x 800 -w 480"

myConfig = withUrgencyHook dzenUrgencyHook {args = ["-bg", "yellow", "-fg", "black"]}
         $ defaultConfig
         { borderWidth        = 2
         , terminal           = myTerminal
         , startupHook        = ewmhDesktopsStartup +++ setWMName "LG3D"
         , handleEventHook    = ewmhDesktopsEventHook
         , normalBorderColor  = "#333333"
         , focusedBorderColor = "#0000ff"
         , workspaces         = myWorkspaces
         , layoutHook         = myLayout
         , manageHook         = myManageHook
         }
         `removeKeysP` delKeys
         `additionalKeysP` insKeys
         `additionalKeysP` multimediaKeys
         `removeMouseBindings` delButtons
         `additionalMouseBindings` insButtons
         where x +++ y = mappend x y

main = do din <- spawnPipe statusBarCmd
          sp <- mkSpawner
          xmonad $ myConfig
                 { logHook            = ewmhDesktopsLogHook >> (dynamicLogWithPP $ myPP din) >> updatePointer (Relative 1.0 1.0)
                 , manageHook         = manageSpawn sp <+> myManageHook
                 }
                `additionalKeysP` [ ("M-p", shellPromptHere sp myShellXPConfig) ]
