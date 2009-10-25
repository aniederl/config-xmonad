-- {-# OPTIONS_GHC -cpp #-}
-- {-
-- #include <X11/XF86keysym.h>
-- -}

import XMonad hiding ( (|||) )

import qualified Data.Map as M
import qualified XMonad.StackSet as W


import XMonad.Actions.Promote
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.WindowGo

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

import XMonad.Layout.Circle
import XMonad.Layout.Square
import XMonad.Layout.Simplest
import XMonad.Layout.TwoPane
import XMonad.Layout.Magnifier
import XMonad.Layout.Grid
import XMonad.Layout.Combo
import XMonad.Layout.LayoutCombinators

import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.DragPane (dragPane, DragType(..))
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.SimpleFloat
import XMonad.Layout.WindowArranger
import XMonad.Layout.Decoration
import XMonad.Layout.ShowWName
import XMonad.Layout.Maximize

import XMonad.Prompt
import XMonad.Prompt.XMonad
import XMonad.Prompt.DirExec

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Themes
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

myTheme = defaultTheme
    { fontName = myFont ++ titleFontSize
    }

myXPConfig = defaultXPConfig
    { font = myFont ++ promptFontSize,
--      font = "xft:DejaVu Sans:size=18",
      bgColor = myBgColor,
      fgColor = myFgColor,
      bgHLight = myFgColor,
      fgHLight = myBgColor,
      borderColor = myBgColor,
      promptBorderWidth = 0,
      position = Bottom,
--      height = 16,
      defaultText = ""
    }

myShowWNameConfig = defaultSWNConfig
    { swn_font    = "xft:DejaVu Sans Mono:size=18" -- "-misc-fixed-*-*-*-*-20-*-*-*-*-*-*-*"
    , swn_bgcolor = "black"
    , swn_color   = "blue"
    , swn_fade    = 0.3
    }

floatSimple :: (Show a, Eq a) => ModifiedLayout (Decoration DefaultDecoration DefaultShrinker)
                      (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
floatSimple = decoration shrinkText myTheme DefaultDecoration (mouseResize $ windowArrangeAll $ SF 20)


myWorkspaces  = [ "term", "mail", "web", "web 2" ] ++ map show [5 .. 9 :: Int]

myLayout =
--           showWName' myShowWNameConfig
         layoutHints
         $ avoidStruts
         $ smartBorders
         $ onWorkspace "term" (tiled ||| Mirror tiled ||| Circle ||| magnify Grid ||| Full)
         $ Full
                     ||| tiled
                     ||| Mirror tiled
                     ||| Circle
                     ||| magnify Grid
    where
      tiled   = Tall nmaster delta ratio
      nmaster = 1
      ratio   = 1/2
      delta   = 3/100
      magnify = magnifiercz (12%10)


maybeTerm :: String -> (String, X ())
maybeTerm cmd = (cmd, raiseMaybe (runInTerm "" cmd) (title =? cmd))

terminalCommands :: [(String, X ())]
terminalCommands = [ maybeTerm c | c <- programs ]
    where
        programs = [ "newsbeuter", "slrn", "mutt", "centerim"]


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
    , ("M-z",               sendMessage Shrink)
    , ("M-x",               sendMessage Expand)

    -- banish the mouse pointer into bottom right corner
    , ("M-b",               banish LowerRight)

    -- toggle to last workspace (like C-a C-a in screen)
    , ("M-a",               (windows $ view =<< tag . head . (filter (\(W.Workspace tag _ _) -> tag /= scratchpadWorkspaceTag)) . hidden))
    , ("M-;",               (windows $ view =<< tag . head . (filter (\(W.Workspace tag _ _) -> tag /= scratchpadWorkspaceTag)) . hidden))

    , ("C-M-l",             spawn "xscreensaver-command -lock")
    , ("M-c",               withFocused (sendMessage . maximizeRestore))
    , ("M-p",               spawn "exe=`dmenu_path | dmenu -nb '#000000' -nf '#CCCCCC'` && eval \"exec $exe\"")
    , ("M-S-p",             spawn ("exe=`dmenu_path | dmenu -nb '#000000' -nf '#CCCCCC'` && eval \"exec " ++ myTerminal ++ " -e $exe\""))

    -- scratchpad terminal with a screen session
    -- need to add '-name' as first argument or else urxvt won't use it
    , ("M-s",               scratchpadSpawnActionTerminal ((terminal myConfig) ++ " -name scratchpad -e $SHELL -c 'screen -c ~/.xmonad/screenrc-scratchpad -D -R scratchpad'"))

    , ("M-e",               spawn myTerminal)
    , ("M-n",               refresh)
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
        [ ((0, 0x1008ff11), unsafeSpawn "amixer -q set Master 5%-")
        , ((0, 0x1008ff12), unsafeSpawn "amixer -q set Master toggle")
        , ((0, 0x1008ff13), unsafeSpawn "amixer -q set Master 5%+")
--        , ((0, 0x1008ff14), spawn "dcop amarok player playPause")
        , ((0, 0x1008ff14), spawn "mpc toggle")
        , ((0, 0x1008ff15), spawn "mpc stop")
        , ((0, 0x1008ff16), spawn "mpc prev")
        , ((0, 0x1008ff17), spawn "mpc next")
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
                    "Mirror Tall" -> "mirror ^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                    "Hinted Mirror Tall" -> "mirror ^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                    "Hinted Wide" -> "mirror ^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                    "Full" -> "full ^i(" ++ myBitmapsDir ++ "/full.xbm)"
                    "Hinted Full" -> "full ^i(" ++ myBitmapsDir ++ "/full.xbm)"
                    "Hinted Circle" -> "circle"
                    "Grid" -> "grid"
                    "Hinted Magnifier Grid" -> "grid"
                    "Tabbed" -> "tabbed"
                    _        -> pad x
                    )
    , ppTitle   = dzenColor "white" "" . dzenEscape . wrap "< " " >" -- . shorten 50
    , ppOutput  = hPutStrLn h
    , ppSort    = fmap (.scratchpadFilterOutWorkspace) $ ppSort defaultPP
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
         `additionalKeys` multimediaKeys
         `removeMouseBindings` delButtons
         `additionalMouseBindings` insButtons
         where x +++ y = mappend x y

main = do din <- spawnPipe statusBarCmd
          xmonad $ myConfig
                 { logHook            = ewmhDesktopsLogHook >> (dynamicLogWithPP $ myPP din) >> updatePointer (Relative 0.5 0.5) }
