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
import XMonad.Actions.TopicSpace as TS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect

-- mouse
import XMonad.Actions.MouseResize
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Warp

import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog as DL
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
import XMonad.Layout.Reflect
import XMonad.Layout.ComboP
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed
import XMonad.Layout.IM
import XMonad.Layout.Grid

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

myFont = "xft:DejaVu Sans Mono"

--myDzen2Font = "-xos4-terminus-*-*-*-*-14-*-*-*-*-*-*-*"
myDzen2Font = myFont ++ ":size=12"

myBgColor = "black"
myFgColor = "blue"

titleFontSize = ":size=8"
promptFontSize = ":size=14"

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

myGSConfig = defaultGSConfig
    { gs_font = myFont }

floatSimple :: (Show a, Eq a) => ModifiedLayout (Decoration DefaultDecoration DefaultShrinker)
                      (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
floatSimple = decoration shrinkText myTheme DefaultDecoration (mouseResize $ windowArrangeAll $ SF 20)


gvimSession tg session = spawnT tg ("gvim -c ':SessionOpen " ++ session ++ "' -c 'let v:this_session = \"" ++ session ++ "\"'")

codeTopicAction tg = spawnShell tg >> spawnT tg "gvim"
codeTopicAction' tg topic = spawnScreenSession tg topic >> gvimSession tg topic

codeTopicSession :: TopicConfig -> String -> (String, X () )
codeTopicSession tg topic = (topic, (spawnScreenSession tg topic >> gvimSession tg topic))


-- external topic file
-- format: multiple lines with name and dir each
--   topic1 topicdir1
--   topic2 topicdir2
--   ...
myTopicFile = myHome ++ "/.xmonad/topics"

zipTopics :: String -> [(String, String)]
zipTopics s = (map myZip) ( (map words) (lines s) )
    where
        myZip (x:y:xs) = (x, y)

mapFirst :: (a -> c) -> [(a, b)] -> [c]
mapFirst _ [] = []
mapFirst f ((x,y):xs) = f x : mapFirst f xs

unzipFirst :: [(a, b)] -> [a]
unzipFirst [] = []
unzipFirst ((x,y):xs) = x : unzipFirst xs


-- topic helper functions from TopicSpace doc
spawnShell :: TopicConfig -> X ()
spawnShell tg = currentTopicDir tg >>= spawnShellIn

spawnT :: TopicConfig -> String -> X ()
spawnT tg program = currentTopicDir tg >>= spawnIn program

spawnScreenSession :: TopicConfig -> String -> X ()
spawnScreenSession tg session = currentTopicDir tg >>= spawnScreenSessionIn session

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawnIn myTerminal dir

screenSession session = myTerminal ++  " -e screen -D -R " ++ session
spawnScreenSessionIn :: String -> Dir -> X ()
spawnScreenSessionIn session dir = spawnIn (screenSession session) dir

spawnIn :: String -> Dir -> X ()
spawnIn program dir = spawn $ "cd ''" ++ dir ++ "'' && " ++ program ++ " &"
{-spawnIn program dir = spawn $ myTerminal ++ "'(cd " ++ dir ++ " && zsh )'"-}

dmenuArgs :: XPConfig -> String
dmenuArgs c = ""
          ++ " -fn \"" ++ font c ++ "\""
          ++ " -nb \"" ++ bgColor c ++ "\""
          ++ " -nf \"" ++ fgColor c ++ "\""
          ++ " -sb \"" ++ bgHLight c ++ "\""
          ++ " -sf \"" ++ fgHLight c ++ "\""
          ++ text
          ++ bottom
    where
        text = case defaultText c of
                    "" -> ""
                    _  -> " -p " ++ defaultText c
        bottom = case position c of
                      Bottom -> " -b"
                      _      -> ""

dmenuPromptCmd :: XPConfig -> String
dmenuPromptCmd conf = "exe=`dmenu_path | yeganesh -- " ++ dmenuArgs conf ++ "` && eval \"exec $exe\""

addTopic :: TopicConfig -> String -> X ()
addTopic tc newtag = addHiddenTopic tc newtag >> switchTopic tc newtag

addHiddenTopic :: TopicConfig -> String -> X ()
addHiddenTopic tc newtag = addHiddenWorkspace newtag

-- copy to nth last focused topic
-- adapted from shiftNthLastFocused
copyNthLastFocused :: Int -> X ()
copyNthLastFocused n = do
    ws <- fmap (listToMaybe . drop n) getLastFocusedTopics
    whenJust ws $ windows . copy


gridselectTopic :: TopicConfig -> GSConfig WorkspaceId -> X ()
gridselectTopic tg conf = withWindowSet $ \ws -> do
    let wss = map W.tag $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws)
    gridselect conf (zip wss wss) >>= flip whenJust (switchTopic tg)


tiledModifiers a = layoutHints
                 $ mkToggle1 NBFULL
                 $ maximize
                 $ a

tiled ratio = named "tall" $ tiledModifiers $ ResizableTall nmaster delta ratio []
    where
        nmaster = 1
        delta   = 3/100

halfTiled   = tiled $ 1/2
tiledMirror = named "mirror" $ Mirror $ tiled $ 1/2
codeMirror  = named "code"   $ Mirror $ tiled $ 4/5

layoutTerm = halfTiled  ||| tiledMirror ||| Full
layoutCode = codeMirror ||| halfTiled   ||| Full

defaultLayouts = Full   ||| halfTiled   ||| tiledMirror

layoutGimp = named "gimp"
           $ combineTwoP (TwoPane 0.85 0.15) Full
             (combineTwoP (reflectHoriz $ TwoPane 0.25 0.25)
              simpleTabbed
              (simpleTabbed ||| Full ||| halfTiled)
              (Role "gimp-dock")
             )
             (Role "gimp-toolbox")

layoutPidgin = named "IM"
             $ reflectHoriz
             $ withIM size roster
--             $ reflectHoriz
             $ layout
    where
        layout = Grid
        size   = 1%5
        roster = Title "Buddy List"

scratchpadWorkspaceTag = "NSP"

delKeys = []
insKeys =
    [ ("M-<Return>",        promote)
    , ("M-v",               sendMessage ToggleStruts)
    , ("M-r",               sendMessage $ ToggleStrut L)
    , ("M-w",               nextScreen)
    , ("M-e",               swapNextScreen)
    , ("M-d",               shiftNextScreen)
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

    -- focus most recently urgent window
    , ("M-u",               focusUrgent)

    -- close only focused copy (kill if last)
    , ("M-S-c",             kill1)

    , ("C-M-l",             spawn "xscreensaver-command -lock")
    , ("M-c",               withFocused (sendMessage . maximizeRestore))
    --, ("M-S-p",             prompt (myTerminal ++ " -e") myShellXPConfig) -- no completion
    , ("M-S-p",             spawn ("exe=`dmenu_path | dmenu -nb '#000000' -nf '#CCCCCC'` && eval \"exec " ++ myTerminal ++ " -e $exe\""))

    -- note taking
    , ("M-n",               appendFilePrompt myNoteXPConfig (myHome ++ "/.notes"))

    -- workspace/topic prompt
    , ("M-S-g",             workspacePrompt myShellXPConfig (windows . W.shift))

    , ("M-S-f",             gridselectWorkspace myGSConfig W.shift)

    , ("M-C-o",             renameWorkspace myXPConfig)

    , ("M-S-<Backspace>",   removeWorkspace)

    -- scratchpad terminal with a screen session
    -- need to add '-name' as first argument or else urxvt won't use it
    , ("M-s",               scratchpadSpawnActionTerminal ((terminal myConfig) ++ " -name scratchpad -e $SHELL -c 'screen -c ~/.xmonad/screenrc-scratchpad -D -R scratchpad'"))

    --, ("M-n",               refresh)
    , ("M-C-S-q",           io (exitWith ExitSuccess))
    , ("M-C-<Home>",        spawn "mpc toggle")
    , ("M-C-<End>",         spawn "mpc stop")
    , ("M-C-<Page_Up>",     spawn "mpc prev")
    , ("M-C-<Page_Down>",   spawn "mpc next")
    ]


multimediaKeys =
        [ ("<XF86AudioLowerVolume>", unsafeSpawn "notify-vol down")
        , ("<XF86AudioMute>",        unsafeSpawn "notify-vol mute")
        , ("<XF86AudioRaiseVolume>", unsafeSpawn "notify-vol up")
        , ("<XF86AudioPlay>",        spawn "mpc toggle")
        , ("<XF86AudioStop>",        spawn "mpc stop")
        , ("<XF86AudioPrev>",        spawn "mpc prev")
        , ("<XF86AudioNext>",        spawn "mpc next")
        ]

delButtons = []
insButtons = []

myGestures = M.fromList
        [ ( [],                 focus)
        , ([R, D, L, U],        \w -> spawn "xscreensaver-command -lock")
        ]

myManageHook = composeAll $
               [ composeOne [ isFullscreen -?> doFullFloat   ] ]
               ++
               [ composeOne [ isDialog     -?> doCenterFloat ] ]
               ++
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
               [ role      =? r --> unfloat | r <- mySinkRoles ]
               ++
                -- other hooks
               [ manageDocks
               , scratchpadManageHookDefault
               ]
    where moveToC c w = className =? c --> doF (W.shift w)
          moveToT t w = title     =? t --> doF (W.shift t)
          floatC  c   = className =? c --> doFloat
          unfloat     = ask >>= doF . W.sink
          myClassFloats = [ "Xmessage", "feh", "Display", "MPlayer", "Kdiff3", "Audacious" ] --"MPlayer",
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
                   ++ zip [ "Thunderbird-bin" ] (repeat "com")
                   ++ zip ["Amarokapp", "amarokapp", "Ario" ] (repeat "music")
                   ++ [ ("Gitk", "gitk") ]
                   ++ zip [ "Pidgin" ] (repeat "im")
          myTerminalShifts = zip ["newsbeuter", "slrn", "mutt", "centerim"] (repeat "com")
          mySinkRoles = [ "gimp-toolbox", "gimp-image-window" ]
          role = stringProperty "WM_WINDOW_ROLE"


myBitmapsDir = "/home/andi/.dzen/bitmaps/dzen"
myPP :: PP
myPP = defaultPP
    { ppCurrent = wrap ("^fg(#FFFFFF)^bg(#647A90)^p(2)^i(" ++ myBitmapsDir ++ "/has_win.xbm)") "^p(2)^fg(grey55)^bg()"
    , ppVisible = wrap ("^bg(grey30)^fg(grey75)^p(2)") "^p(2)^fg(grey55)^bg()"
    , ppSep     = " ^fg(grey60)^r(3x3)^fg() "
    , ppWsSep   = " | "
    , ppUrgent  = wrap (dzenColor "#FF0000" "" "{") (dzenColor "#FF0000" "" "}") . pad
    , ppLayout  = dzenColor "#647A90" "" .
        (\x -> case x of
                    "tall"   ->   "tall ^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                    "mirror" -> "mirror ^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                    "code"   ->   "code ^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                    "full"   ->   "full ^i(" ++ myBitmapsDir ++ "/full.xbm)"
                    "Grid"   -> "grid"
                    _        -> pad x
                    )
    , ppTitle   = dzenColor "white" "" . dzenEscape . wrap "< " " >" -- . shorten 50
    , ppSort    = fmap (.scratchpadFilterOutWorkspace) $ ppSort defaultPP
    }


mergePPOutputs :: [PP -> X String] -> PP -> X String
mergePPOutputs x pp = fmap (intercalate (ppSep pp)) . sequence . sequence x $ pp

onlyTitle :: PP -> PP
onlyTitle pp = defaultPP { ppCurrent = const ""
                         , ppHidden  = const ""
                         , ppVisible = const ""
                         , ppLayout  = ppLayout pp
                         , ppTitle   = ppTitle pp }

myDynamicLogString :: TopicConfig -> PP -> X String
myDynamicLogString tg pp = mergePPOutputs [TS.pprWindowSet tg, dynamicLogString . onlyTitle] pp

myDynamicLogWithPP :: TopicConfig -> PP -> X ()
myDynamicLogWithPP tg pp = myDynamicLogString tg pp >>= io . ppOutput pp


statusBarCmd = "dzen2 -bg '#000000' -fg '#FFFFFF' -h 16 -fn \"" ++ myDzen2Font ++ "\" -sa c -e '' -ta l" -- -w 800"

logBarCmd = "inotail -f -n 30 /var/log/messages | dzen2 -e 'entertitle=uncollapse;leavetitle=collapse' -bg '#000000' -fg '#FFFFFF' -h 16 -fn \"" ++ myDzen2Font ++ "\" -sa c -e '' -ta l -x 800 -w 480"

myConfig = withUrgencyHookC NoUrgencyHook urgencyConfig { suppressWhen = Focused }
         $ defaultConfig
         { borderWidth        = 2
         , terminal           = myTerminal
         , startupHook        = ewmhDesktopsStartup +++ setWMName "LG3D"
         , handleEventHook    = ewmhDesktopsEventHook
         , normalBorderColor  = "#333333"
         , focusedBorderColor = "#0000ff"
         , workspaces         = myTopics
         , manageHook         = myManageHook
         }
         `removeKeysP` delKeys
         `additionalKeysP` insKeys
         `additionalKeysP` multimediaKeys
         `removeMouseBindings` delButtons
         `additionalMouseBindings` insButtons
         where x +++ y = mappend x y

myTopics :: [Topic]
myTopics = [ "admin", "com", "web", "web2", "web3", "music",
             "xmonad", "documents", "sweb", "bs", "sup", "conf", "slrnrc",
             "gimp", "gitk", "cal", "im"
           ]
myTopicDirs = [ ("xmonad", ".xmonad")
              , ("sweb",   "bs/sweb")
              , ("bs",     "bs")
              , ("sup",    "src/sup")
              , ("conf",   "etc")
              , ("slrnrc", "etc/slrn")
              ]

main = do
    tf <- readFile myTopicFile
    let ts = zipTopics tf
    let tc = TopicConfig {
      topicDirs    = M.fromList $ myTopicDirs ++ ts
    , defaultTopicAction = const $ return()
    , defaultTopic = "admin"
    , maxTopicHistory = 10
    , topicActions = M.fromList $
        [ ("admin",     spawnScreenSession tc "main" >>  spawnT tc (myTerminal ++ " -e su -l -c 'screen -D -R main'"))
        , codeTopicSession tc "xmonad"
        , codeTopicSession tc "slrnrc"
        , codeTopicSession tc "sup"
        , codeTopicSession tc "sweb"
        , codeTopicSession tc "bs"
        , ("conf",      codeTopicAction tc)
        , ("music",     spawn "ario")
        , ("gimp",      spawn "gimp")
        , ("cal",       spawnT tc (myTerminal ++ " -e wyrd"))
        , ("im",        spawn "pidgin")
        ]
        ++
        (mapFirst (codeTopicSession tc) ts)
    }
    let ws = (workspaces myConfig) ++ (unzipFirst ts)
    checkTopicConfig ws tc
    din  <- spawnPipe (statusBarCmd ++ " -xs 1")
    din2 <- spawnPipe (statusBarCmd ++ " -xs 2")
    --din <- spawnPipe statusBarCmd
    xmonad $ myConfig
        { logHook            =   ewmhDesktopsLogHook
                             >> (myDynamicLogWithPP tc $ myPP { ppOutput = hPutStrLn din })
                             >> (myDynamicLogWithPP tc $ myPP { ppOutput = hPutStrLn din2 })
                             >>  updatePointer (Relative 1.0 1.0)
        , manageHook         = manageSpawn <+> myManageHook
        , workspaces         = ws
        , layoutHook         = avoidStruts
                             $ smartBorders
                             $ onWorkspace "admin"          layoutTerm
                             $ onWorkspace "gimp"           layoutGimp
                             $ onWorkspace "im"             layoutPidgin
                             $ onWorkspaces [ "conf"
                                            , "slrnrc"
                                            , "xmonad"
                                            , "sweb"
                                            , "bs"
                                            ]               layoutCode
                             $ onWorkspaces (unzipFirst ts) layoutCode
                             $ defaultLayouts
        }
        `additionalKeysP` ( [
          ("M-p",   spawnHere (dmenuPromptCmd myShellXPConfig))
        , ("M-i",   spawnShell tc)
        , ("M-g",   workspacePrompt myShellXPConfig (switchTopic tc))
        , ("M-o",   workspacePrompt myXPConfig (addTopic tc))
        , ("M-S-o", workspacePrompt myXPConfig (addHiddenTopic tc))
        , ("M-f",   gridselectTopic tc myGSConfig)
        ]
        ++
        -- switch or shift to Nth last focused workspace (history)
        [("M" ++ m ++ ('-':k:[]) , f i)
            | (i, k) <- zip [1..] ['1'..'9']
            , (f, m) <- [(switchNthLastFocused tc, ""), (shiftNthLastFocused, "-S"), (copyNthLastFocused, "-C-S")]]
        )
