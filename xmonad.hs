{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-deprecations #-}
-- {-# OPTIONS_GHC -cpp #-}
-- {-
-- #include <X11/XF86keysym.h>
-- -}

import XMonad

import qualified Data.Map as M
import qualified XMonad.StackSet as W

import qualified XMonad.Actions.FlexibleManipulate as Flex

import XMonad.Actions.Promote
import XMonad.Actions.CycleWS hiding (Not)
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
--import XMonad.Hooks.DynamicLog as DL
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP as SBPP
import XMonad.Hooks.WorkspaceHistory

import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders (smartBorders, noBorders)
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
import XMonad.Layout.IM hiding (Not)
import XMonad.Layout.Grid
import XMonad.Layout.TrackFloating
import XMonad.Layout.MagicFocus
import XMonad.Layout.Circle

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Workspace

import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare
import qualified XMonad.Util.Hacks as Hacks

import Data.Ratio
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Ord

import Control.Monad

import Text.Regex.Posix

import System.Exit
import System.Directory
import System.Environment

import XMonad.StackSet (view, greedyView, tag, hidden, stack)

import XMonad.Hooks.TaffybarPagerHints (pagerHintsEventHook, pagerHintsLogHook)


-- default configuration -------------------------------------------------------

myFont = "xft:DejaVu Sans Mono"

--myDzen2Font = "-xos4-terminus-*-*-*-*-14-*-*-*-*-*-*-*"
myDzen2Font = myFont ++ ":size=12"

titleFontSize  = ":size=8"
promptFontSize = ":size=14"

myTerminal = "urxvt"

myBrowser = "google-chrome-beta"


myBgColor = "black"
myFgColor = "blue"

myTheme = XMonad.Layout.Tabbed.def
    { fontName = myFont ++ titleFontSize
    }

myXPConfig = XMonad.Prompt.def
    { font              = myFont ++ promptFontSize
    , bgColor           = myBgColor
    , fgColor           = myFgColor
    , bgHLight          = myFgColor
    , fgHLight          = myBgColor
    , borderColor       = myBgColor
    , promptBorderWidth = 0
    , position          = Top
    , defaultText       = ""
    }

myShellXPConfig = myXPConfig
    { -- autocomplete after 1 sec
      autoComplete      = Just 100000
    }

myNoteXPConfig = myXPConfig
    { position = Bottom }

myGSConfig = XMonad.Actions.GridSelect.def
    { gs_font = myFont }

myFilter = filterOutWs [scratchpadWorkspaceTag]
myConfig = withUrgencyHookC NoUrgencyHook urgencyConfig { suppressWhen = Focused }
         $ docks $ addEwmhWorkspaceSort (pure myFilter) $ ewmhFullscreen $ ewmh $ Hacks.javaHack XMonad.def
         { borderWidth        = 2
         , terminal           = myTerminal
         , normalBorderColor  = "#333333"
         , focusedBorderColor = "#0000ff"
         , startupHook        = setWMName "LG3D" <+> addEWMHFullscreen
         , handleEventHook    = handleEventHook def <> pagerHintsEventHook <+> Hacks.windowedFullscreenFixEventHook
         , logHook            = updatePointer (1.0, 1.0) (1, 1) -- (Relative 1.0 1.0)
         , manageHook         = manageSpawn <+> myManageHook
         }
         `removeKeysP` delKeys
         `additionalKeysP` multimediaKeys
         `removeMouseBindings` delButtons
         `additionalMouseBindings` insButtons

updateMyConfig conf home tc ts = conf
    { workspaces = map topicName ts
    , layoutHook = myLayoutHook  ts
    }
    `additionalKeysP` (insKeys home tc)


-- Fullscreen support ----------------------------------------------------------

-- advertise fullscreen capability (fixes Firefox fullscreen issue)
-- see https://github.com/xmonad/xmonad-contrib/issues/183
addNETSupported :: Atom -> X()
addNETSupported x = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
        sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
        when (fromIntegral x `notElem` sup) $
            changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]


-- Topics ----------------------------------------------------------------------

data TopicType = Code
               | Other
    deriving Eq
data MyTopicItem = MyTopicItem { topicName   :: Topic
                               , topicDir    :: Dir
                               , topicAction :: X ()
                               , topicType   :: TopicType
                               }


myDefaultTopicAction = return ()

myDefaultTopicConfig = TopicConfig
    { topicDirs          = M.empty
    , topicActions       = M.empty
    , defaultTopicAction = const $ myDefaultTopicAction
    , defaultTopic       = "admin"
    , maxTopicHistory    = 10
    }


-- actions
myActionTopics' :: [(Topic, X ())]
myActionTopics' = [ ("admin", spawnTmuxSession "main" >>  spawnT (myTerminal ++ " -e sudo -i tmuxinator admin"))
                  , ("music", spawn "strawberry")
                  , ("gimp",  spawn "gimp")
                  , ("cal",   spawnT (myTerminal ++ " -e wyrd"))
                  , ("im",    spawn "skype")
                  , ("skype", spawn "skype")
                  , ("teams", spawn myBrowser)
                  , ("web",   spawn myBrowser)
                  , ("com",   spawn "thunderbird")
                  , ("vbox",  spawn "VirtualBox")
                  ]

myActionTopics :: [(Topic, Dir, X ())]
myActionTopics = map (\(n, a) -> (n, "", a)) myActionTopics'
               ++ [ ("conf", "etc", codeTopicAction)
                 ]

myCodeTopics = [ ("xmonad", ".xmonad")
               , ("slrnrc", "etc/slrn")
               ]

myOtherTopics = [ "documents"
                , "gitk"
                ]


myTopics :: [MyTopicItem]
myTopics = map topicItem''    myActionTopics
           ++
           map codeTopicItem  myCodeTopics
           ++
           map otherTopicItem myOtherTopics
    where
        topicItem'' (name, dir, action) = topicItem' name dir action
        codeTopicItem  (name, dir)      = topicItem name dir (codeTopicSession' myTopicConfig name) Code
        otherTopicItem name             = topicItem' name ""  myDefaultTopicAction

myTopicNames :: [Topic]
myTopicNames = map topicName myTopics

topicDirMap :: [MyTopicItem] -> Map Topic Dir
topicDirMap ts = M.fromList $ map (\(MyTopicItem n d _ _) -> (n, d)) ts

topicActionMap :: [MyTopicItem] -> Map Topic (X ())
topicActionMap ts = M.fromList $ map (\(MyTopicItem n _ a _) -> (n, a)) ts


updateTopicConfig :: TopicConfig -> [MyTopicItem] -> TopicConfig
updateTopicConfig tc ts = tc
    { topicDirs    = topicDirs    tc <+> (topicDirMap    ts)
    , topicActions = topicActions tc <+> (topicActionMap ts)
    }

myTopicConfig = updateTopicConfig myDefaultTopicConfig myTopics


topicItem :: Topic -> Dir -> X () -> TopicType -> MyTopicItem
topicItem name dir action ttype = MyTopicItem name topicdir action ttype
    where topicdir =
              case dir of
                   "" -> "${HOME}/"
                   _  -> dir

topicItem' :: Topic -> Dir -> X () -> MyTopicItem
topicItem' name dir action = topicItem name dir action Other

typedTopicItem :: TopicConfig -> Topic -> Dir -> String -> MyTopicItem
typedTopicItem tc name dir ttype | ttype == "code" = topicItem  name dir (codeTopicSession' tc name) Code
                                 | otherwise      = topicItem' name dir myDefaultTopicAction


-- external topic file
-- format: multiple lines with name, dir and type each
--   topic1 topicdir1 code
--   topic2 topicdir2
--   topic3 topicdir3 code
--   ...
myTopicFile     = ".xmonad/topics"
myCodeTopicFile = ".xmonad/code-topics"

zipTopics' :: TopicConfig -> String -> String -> [MyTopicItem]
zipTopics' tc dc s = (map myZip) ( (map words) (lines s) )
    where
        myZip (x:y:z:xs) = typedTopicItem tc x y z
        myZip (x:y:xs)   = typedTopicItem tc x y dc
zipTopics tc s = zipTopics' tc "other" s

readTopicsFile :: String -> IO String
readTopicsFile f = do
    e <- doesFileExist f
    if e then readTopicsFile' f
         else return ""

readTopicsFile' :: String -> IO String
readTopicsFile' f = do
    l <- readFile f
    return $ l


-- spawn functions
spawnShell :: TopicConfig -> X ()
spawnShell tc = currentTopicDir tc >>= spawnShellIn

spawnT' :: TopicConfig -> String -> X ()
spawnT' tc program = currentTopicDir tc >>= spawnIn program
spawnT program = spawnT' myTopicConfig program

spawnIn :: String -> Dir -> X ()
spawnIn program dir = spawn $ "cd ''" ++ dir ++ "'' && " ++ program ++ " &"
{-spawnIn program dir = spawn $ myTerminal ++ "'(cd " ++ dir ++ " && zsh )'"-}

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawnIn myTerminal dir


-- session functions
spawnScreenSession' :: TopicConfig -> String -> X ()
spawnScreenSession' tc session = currentTopicDir tc >>= spawnScreenSessionIn session

spawnScreenSession :: String -> X ()
spawnScreenSession session = spawnScreenSession' myTopicConfig session

screenSession session = myTerminal ++ " -e screen -D -R " ++ session
spawnScreenSessionIn :: String -> Dir -> X ()
spawnScreenSessionIn session dir = spawnIn (screenSession session) dir

--gvimSession tc session = spawnT' tc ("gvim -c ':SessionOpen " ++ session ++ "' -c 'let v:this_session = \"" ++ session ++ "\"'")
gvimSession tc session = spawnT' tc ("gvim'")


spawnTmuxSession' :: TopicConfig -> String -> X ()
spawnTmuxSession' tc session = currentTopicDir tc >>= spawnTmuxSessionIn session

spawnTmuxSession :: String -> X ()
spawnTmuxSession session = spawnTmuxSession' myTopicConfig session

spawnTmuxSessionIn :: String -> Dir -> X ()
spawnTmuxSessionIn session dir = spawnIn (tmuxSession session) dir
    where
        tmuxSession session = myTerminal ++ " -e tmux new -A -s " ++ session

spawnTmuxinatorSessionIn :: String -> Dir -> X ()
spawnTmuxinatorSessionIn session dir = spawnIn (tmuxSession session) dir
    where
        tmuxSession session = myTerminal ++ " -e tmuxinator start -A -s " ++ session


-- code topics
codeTopicAction' tc = spawnShell tc >> spawnT' tc "gvim"
codeTopicAction = codeTopicAction' myTopicConfig

codeTopicSession :: TopicConfig -> String -> (String, X () )
codeTopicSession tc topic = (topic, codeTopicSession' tc topic)

codeTopicSessionIn :: String -> Dir -> X ()
codeTopicSessionIn session dir = spawnIn (myTerminal ++ " -e tmuxinator start code -n \"" ++ session ++ "\" workspace=" ++ dir) dir

codeTopicSession' :: TopicConfig -> String -> X ()
codeTopicSession' tc topic = currentTopicDir tc >>= codeTopicSessionIn topic
--codeTopicSession' tc topic = spawnTmuxSession' tc topic >> gvimSession tc topic


-- Layouts ---------------------------------------------------------------------
floatSimple :: (Show a, Eq a) => ModifiedLayout (Decoration DefaultDecoration DefaultShrinker)
                      (ModifiedLayout MouseResize (ModifiedLayout WindowArranger SimpleFloat)) a
floatSimple = decoration shrinkText myTheme DefaultDecoration (mouseResize $ windowArrangeAll $ SF 20)

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

layoutTerm = halfTiled  ||| tiledMirror ||| Full ||| magicFocus(noBorders Circle)
layoutCode = Full ||| codeMirror ||| halfTiled   ||| magicFocus(noBorders Circle)

defaultLayouts = Full   ||| halfTiled   ||| tiledMirror ||| Circle

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
             $ withIM ratio roster
--             $ reflectHoriz
             $ layout
    where
        layout = Grid
        ratio  = 1%5
        roster = (ClassName "Pidgin") `And` (Role "buddy_list")

layoutSkype = named "skype"
             $ reflectHoriz
             $ withIM ratio roster
--             $ reflectHoriz
             $ layout
    where
        layout = Grid
        ratio  = 1%5
        roster  = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))

-- needs list of code topics as argument
myLayoutHook ts = avoidStruts
                $ trackFloating
                $ smartBorders
                $ onWorkspace "admin" layoutTerm
                $ onWorkspace "gimp"  layoutGimp
                $ onWorkspace "im"    layoutPidgin
                $ onWorkspace "skype" layoutSkype
                $ onWorkspace "conf"  layoutCode
                $ onWorkspaces codeWS layoutCode
                $ defaultLayouts
    where
        codeWS = [ topicName t | t <- ts, topicType t == Code ]


-- Scratchpads ----------------------------------------------------------------

scratchpads = [ NS "term" ((terminal myConfig) ++ " -name scratchpad -e $SHELL -c 'tmux'") (appName =? "scratchpad") (customFloating $ W.RationalRect 0.25 0.375 0.5 0.3)
              , NS "htop" ((terminal myConfig) ++ " -name htop -e htop") (appName =? "htop") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
              ]

-- Key Bindings ----------------------------------------------------------------

delKeys = ["M-" ++ [n] | n <- ['1'..'9']]
insKeys home tc =
    [ ("M-<Return>",        promote)
    , ("M-v",               sendMessage ToggleStruts)
    , ("M-w",               nextScreen)
    , ("M-e",               swapNextScreen)
    , ("M-d",               shiftNextScreen)

    -- cycle through recent workspaces
    --, ("M-h",               myCycleRecentWS [xK_Alt_L, xK_Alt_R] xK_l xK_h)
    --, ("M-l",               myCycleRecentWS [xK_Alt_L, xK_Alt_R] xK_l xK_h)
    --, ("M-S-h",             myShiftRecentWS [xK_Alt_L, xK_Alt_R] xK_l xK_h)
    --, ("M-S-l",             myShiftRecentWS [xK_Alt_L, xK_Alt_R] xK_l xK_h)
    --, ("M-<Tab>",           myCycleRecentWS [xK_Alt_L, xK_Alt_R] xK_l xK_h)
    --, ("M-`",               myCycleRecentWS [xK_Alt_L, xK_Alt_R] xK_l xK_h)

    -- resize master pane
    , ("M-z",               sendMessage Shrink)
    , ("M-x",               sendMessage Expand)

    , ("M-C-<Space>",       sendMessage $ Toggle NBFULL)

    -- banish the mouse pointer into bottom right corner
    , ("M-b",               banish LowerRight)

    -- toggle to last workspace (like C-a C-a in screen)
    --, ("M-a",               myToggleWS)
    --, ("M-;",               myToggleWS)
    , ("M-a",               myToggleTopic tc)
    , ("M-;",               myToggleTopic tc)

    -- focus most recently urgent window
    , ("M-u",               focusUrgent)

    -- close only focused copy (kill if last)
    , ("M-S-c",             kill1)

    --, ("C-M-l",             spawn "xscreensaver-command -lock")
    , ("C-M-l",             spawn "slock")
    , ("M-c",               withFocused (sendMessage . maximizeRestore))

    -- spawn a terminal
    , ("M-i",               spawnShell tc)

    -- run prompt
    , ("M-p",               spawnT' tc (dmenuPromptCmd myShellXPConfig))

    -- note taking
    , ("M-n",               appendFilePrompt myNoteXPConfig (home ++ "/.notes"))

    -- workspace/topic prompt
    , ("M-S-o",             workspacePrompt myXPConfig (addHiddenTopic))
    , ("M-S-g",             workspacePrompt myShellXPConfig (windows . W.shift))

    , ("M-S-f",             gridselectWorkspace myGSConfig W.shift)

    , ("M-o",               workspacePrompt myXPConfig (addTopic tc))
    , ("M-g",               workspacePrompt myShellXPConfig (switchTopic tc))
    , ("M-f",               gridselectTopic tc myGSConfig)

    , ("M-r",               renameWorkspace myXPConfig)
    , ("M-<F-1>",           renameWorkspace myXPConfig)

    , ("M-S-<Backspace>",   removeWorkspace)

    -- scratchpad terminal with a screen session
    -- need to add '-name' as first argument or else urxvt won't use it
    --, ("M-s",               scratchpadSpawnActionTerminal ((terminal myConfig) ++ " -name scratchpad -e $SHELL -c 'tmux'"))
    , ("M-s",               namedScratchpadAction scratchpads "term")
    , ("M-S-t",             namedScratchpadAction scratchpads "htop")

    --, ("M-n",               refresh)
    , ("M-C-S-q",           io (exitWith ExitSuccess))
    , ("M-C-<Home>",        spawn "xmpc toggle")
    , ("M-C-<End>",         spawn "xmpc stop")
    , ("M-C-<Page_Up>",     spawn "xmpc prev")
    , ("M-C-<Page_Down>",   spawn "xmpc next")

    -- screenshot
    , ("<Print>",           unsafeSpawn "scrot '%Y-%m-%d-%H%M_$wx$h.png' -e 'mv $f ~/shots/'")

    -- audacious
    --, ("<F-12>",            spawn "audtool playback-seek-relative -1 playback-pause")
    --, ("<F-11>",            spawn "audtool playback-seek-relative +3")
    --, ("<F-10>",            spawn "audtool playback-seek-relative -3")

    --, ("<M-`",              unsafeSpawn "notify-time")

    -- cmus
    , ("<F-12>",            spawn "cmus-remote -C \'seek -1\' \'player-pause\'")
    , ("<F-11>",            spawn "cmus-remote -C \'seek +3\'")
    , ("<F-10>",            spawn "cmus-remote -C \'seek -3\'")
    ]
    ++
    -- switch or shift to Nth last focused workspace (history)
    -- [("M" ++ m ++ ('-':k:[]) , f i)
    [(mod ++ m ++ (k:[]), f i)
        | (i, k) <- zip [1..] ['1'..'9']
        , (f, m) <- [(switchNthLastFocused tc, "M-"), (shiftNthLastFocused, "M-S-")]
        -- , (f, m) <- [(switchNthLastFocused tc, ""), (shiftNthLastFocused, "S-"), (copyNthLastFocused, "C-S-")]
    ]
    where
        --mod  = "M-"
        --mod  = "C-M-"
        mod  = ""


multimediaKeys =
        [ ("<XF86AudioLowerVolume>",  unsafeSpawn "notify-vol down")
        , ("<XF86AudioRaiseVolume>",  unsafeSpawn "notify-vol up")
        , ("<XF86AudioMute>",         unsafeSpawn "notify-vol mute")
        , ("<XF86AudioMicMute>",      unsafeSpawn "notify-mic-toggle")
        , ("<XF86Favorites>",         unsafeSpawn "notify-mic-toggle")
        , ("<XF86TouchpadToggle>",    unsafeSpawn "notify-touchpad-toggle")
        , ("<XF86MonBrightnessDown>", unsafeSpawn "notify-bright down")
        , ("<XF86MonBrightnessUp>",   unsafeSpawn "notify-bright up")
        --, ("<XF86AudioPlay>",        spawn "xmpc toggle")
        --, ("<XF86AudioStop>",        spawn "xmpc stop")
        --, ("<XF86AudioPrev>",        spawn "xmpc prev")
        --, ("<XF86AudioNext>",        spawn "xmpc next")
        , ("<XF86Display>",          spawn "qdbus org.mpris.MediaPlayer2.strawberry /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
        , ("<XF86WLAN>",             spawn "qdbus org.mpris.MediaPlayer2.strawberry /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
        , ("<XF86AudioPlay>",        spawn "qdbus org.mpris.MediaPlayer2.strawberry /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
        , ("<XF86AudioStop>",        spawn "qdbus org.mpris.MediaPlayer2.strawberry /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Stop")
        , ("<XF86AudioPrev>",        spawn "qdbus org.mpris.MediaPlayer2.strawberry /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
        , ("<XF86AudioNext>",        spawn "qdbus org.mpris.MediaPlayer2.strawberry /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
        ]

delButtons =
        [ (mod1Mask, button1)
        , (mod1Mask, button2)
        , (mod1Mask, button3)
        ]
insButtons =
        [ ((mod1Mask .|. controlMask, button1), (\w -> focus w >> Flex.mouseWindow Flex.linear w))
        , ((mod1Mask .|. controlMask, button2), (\w -> focus w >> windows W.shiftMaster))
        , ((mod1Mask .|. controlMask, button3), (\w -> focus w >> Flex.mouseWindow Flex.resize w))
        ]

myGestures = M.fromList
        [ ( [],                 focus)
        , ([R, D, L, U],        \w -> spawn "xscreensaver-command -lock")
        ]

myToggleWS :: X ()
myToggleWS = toggleWS' [scratchpadWorkspaceTag]
myToggleTopic :: TopicConfig -> X ()
myToggleTopic tc = switchNthLastFocusedByScreen tc 1
--myToggleTopic tc = switchNthLastFocused tc 1

--myCycleRecentWS = myRecentWS W.view
--myShiftRecentWS = myRecentWS shiftView'
--    where shiftView' id ws = W.view id $ W.shift id ws
--
--myRecentWS f = cycleWindowSets options
--    where
--        options w = map (f `flip` w) (recentTags w)
--        recentTags w = map tag $ tail (myWS w) ++ [head (myWS w)]
--        myWS w = scratchpadFilterOutWorkspace $ W.workspaces w


--cycleRecentWS' = cycleWindowSets options
--  where options w = map (W.view `flip` w) (recentTags w)
--        recentTags w = map W.tag $ W.hidden w ++ [W.workspace (W.current w)]


addTopic :: TopicConfig -> String -> X ()
addTopic tc newtag = addHiddenTopic newtag >> switchTopic tc newtag

addHiddenTopic :: String -> X ()
addHiddenTopic newtag = addHiddenWorkspace newtag

---- copy to nth last focused topic
---- adapted from shiftNthLastFocused
--copyNthLastFocused :: Int -> X ()
--copyNthLastFocused n = do
--    ws <- fmap (listToMaybe . drop n) getLastFocusedTopics
--    whenJust ws $ windows . copy


gridselectTopic :: TopicConfig -> GSConfig WorkspaceId -> X ()
gridselectTopic tc conf = withWindowSet $ \ws -> do
    let wss = map W.tag $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws)
    gridselect conf (zip wss wss) >>= flip whenJust (switchTopic tc)


-- dmenu prompt
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
dmenuPromptCmd conf = "exe=`dmenu_path | yeganesh -- "
                    ++ dmenuArgs conf
                    ++ "` && eval \"exec $exe\""

-- Dock support ----------------------------------------------------------------

-- fix window order for newly started docks
-- see https://github.com/xmonad/xmonad-contrib/issues/211

-- | Restack dock under lowest managed window.
lowerDock :: ManageHook
lowerDock = checkDock --> do
    w <- ask
    mlw <- liftX $ findLowest
    case mlw of
      Just lw   -> liftX $ do
        d <- asks display
        liftIO $ restackWindows d [lw, w]
        return idHook
      Nothing   -> return idHook

-- | Find lowest managed window.
findLowest :: X (Maybe Window)
findLowest  = withWindowSet $ \ws -> do
    d <- asks display
    r <- asks theRoot
    (_, _, ts) <- liftIO $ queryTree d r
    return (find (`W.member` ws) ts)


-- Manage Hook -----------------------------------------------------------------

myManageHook = composeAll $
             [ composeOne [ isFullscreen -?> doFullFloat   ] ]
             ++
             [ composeOne [ isDialog     -?> doCenterFloat ] ]
             ++
              -- auto float
             [ className =? c --> doCenterFloat  | c <- myClassCenterFloats ]
             ++
             [ title     =? t --> doCenterFloat  | t <- myTitleCenterFloats ]
             ++
             [ className =? c --> doFloat  | c <- myClassFloats ]
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
             [ lowerDock
             , manageDocks
             , namedScratchpadManageHook scratchpads
             ]
    where moveToC c w = className =? c --> doF (W.shift w)
          moveToT t w = title     =? t --> doF (W.shift t)
          floatC  c   = className =? c --> doFloat
          unfloat     = ask >>= doF . W.sink
          myClassCenterFloats = [ "Xmessage", "feh", "Display", "MPlayer", "mpv", "Kdiff3", "Audacious" ] --"MPlayer",
          myClassFloats = [ "Plasma-desktop", "plasmashell", "Klipper" ] --"MPlayer",
          myTitleCenterFloats = [ "Downloads", "Firefox Preferences", "Thunderbird Preferences", "Save As...",
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
                   ++ zip [ "Skype" ] (repeat "skype")
          myTerminalShifts = zip ["newsbeuter", "slrn", "mutt", "centerim"] (repeat "com")
          mySinkRoles = [ "gimp-toolbox", "gimp-image-window" ]
          role = stringProperty "WM_WINDOW_ROLE"


-- Dynamic Log -----------------------------------------------------------------

myBitmapsDir = ".xmonad/.dzen/bitmaps/dzen"

myDefaultPP = SBPP.def

myPP :: String -> PP
myPP home = myDefaultPP
    { ppCurrent = wrap ("^fg(#FFFFFF)^bg(#647A90)^p(2)^i(" ++ myBitmapsDir ++ "/has_win.xbm)") "^p(2)^fg(grey55)^bg()"
    , ppVisible = wrap ("^bg(grey30)^fg(grey75)^p(2)") "^p(2)^fg(grey55)^bg()"
    , ppSep     = " ^fg(grey60)^r(3x3)^fg() "
    , ppWsSep   = " | "
    , ppUrgent  = wrap (dzenColor "#FF0000" "" "{") (dzenColor "#FF0000" "" "}") . pad
    , ppLayout  = dzenColor "#647A90" "" .
        (\x -> case x of
                    "tall"   ->   "Tall ^i(" ++ bitmapsDir ++ "/tall.xbm)"
                    "mirror" -> "Mirror ^i(" ++ bitmapsDir ++ "/mtall.xbm)"
                    "code"   ->   "Code ^i(" ++ bitmapsDir ++ "/mtall.xbm)"
                    "Full"   ->   "Full ^i(" ++ bitmapsDir ++ "/full.xbm)"
                    "Grid"   ->   "Grid ^i(" ++ bitmapsDir ++ "/grid.xbm)"
                    "IM"     ->     "IM ^i(" ++ bitmapsDir ++ "/im.xbm)"
                    "gimp"   ->   "Gimp ^i(" ++ bitmapsDir ++ "/gimp.xbm)"
                    _        -> pad x
        )
    , ppTitle   = dzenColor "white" "" . dzenEscape . wrap "< " " >" -- . shorten 50
    --, ppSort    = fmap (.scratchpadFilterOutWorkspace) $ ppSort myDefaultPP
    }
    where
        bitmapsDir = home ++ "/" ++ myBitmapsDir


mergePPOutputs :: [PP -> X String] -> PP -> X String
mergePPOutputs x pp = fmap (intercalate (ppSep pp)) . sequence . sequence x $ pp

onlyTitle :: PP -> PP
onlyTitle pp = myDefaultPP { ppCurrent = const ""
                           , ppHidden  = const ""
                           , ppVisible = const ""
                           , ppLayout  = ppLayout pp
                           , ppTitle   = ppTitle pp }

---- taken from XMonad.Actions.TopicSpace and modified for respecting ppSort
---- function
mypprWindowSet :: TopicConfig -> PP -> X String
mypprWindowSet tg pp = do
    winset <- gets windowset
    urgents <- readUrgents
    sort' <- ppSort pp
    let empty_workspaces = map W.tag $ filter (isNothing . W.stack) $ W.workspaces winset
        maxDepth = maxTopicHistory tg
    --setLastFocusedTopic tg
    --                    (W.tag . W.workspace . W.current $ winset)
    --                    (`notElem` empty_workspaces)
    lastWs <- workspaceHistory
    let depth topic = fromJust $ elemIndex topic (lastWs ++ [topic])
        add_depth proj topic = proj pp . (((topic++":")++) . show) . depth $ topic
        pp' = pp { ppHidden = add_depth ppHidden, ppVisible = add_depth ppVisible }
        sortWindows = take maxDepth . sortBy (comparing $ depth . W.tag) . sort'
    return $ SBPP.pprWindowSet sortWindows urgents pp' winset


myDynamicLogString :: TopicConfig -> PP -> X String
myDynamicLogString tc pp = mergePPOutputs [mypprWindowSet tc, dynamicLogString . onlyTitle] pp

myDynamicLogWithPP :: TopicConfig -> PP -> X ()
myDynamicLogWithPP tc pp = myDynamicLogString tc pp >>= io . ppOutput pp


-- main ------------------------------------------------------------------------

statusBarCmd = "dzen2"
             ++ " -dock"
             ++ " -bg '#000000'"
             ++ " -fg '#FFFFFF'"
             ++ " -h 16 -fn \"" ++ myDzen2Font ++ "\""
             ++ " -sa c -e '' -ta l"
--             ++ " -w 1920"
--             ++ " -w 800"
--             ++ " -expand right"

logBarCmd = "inotail -f -n 30"
          ++ "/var/log/messages | dzen2"
          ++ " -e 'entertitle=uncollapse;leavetitle=collapse'"
          ++ " -bg '#000000'"
          ++ " -fg '#FFFFFF'"
          ++ " -h 16 -fn \"" ++ myDzen2Font ++ "\""
          ++ " -sa c -e '' -ta l"
          ++ " -x 800 -w 480"


fileTopicList tc tf1 tf2 = zipTopics tc tf1 ++ zipTopics' tc "code" tf2

main = do
    home <- getEnv "HOME"
    tf   <- readTopicsFile $ home ++ "/" ++ myTopicFile
    ctf  <- readTopicsFile $ home ++ "/" ++ myCodeTopicFile
    --din  <- spawnPipe $ "taffybar"
    din  <- spawnPipe $ statusBarCmd ++ " -w 2560" ++ " -xs 1"
    din2 <- spawnPipe $ statusBarCmd ++ " -w 2560" ++ " -xs 2"
    din3 <- spawnPipe $ statusBarCmd ++ " -w 1920" ++ " -xs 3"
    --din4 <- spawnPipe $ "cat >> .status_bar.log"
    let tc   = updateTopicConfig myTopicConfig $ fileTopicList tc tf ctf
    let conf = updateMyConfig myConfig home tc $ myTopics ++ fileTopicList tc tf ctf
    xmonad $ conf
        { logHook            = logHook conf >> workspaceHistoryHookExclude [scratchpadWorkspaceTag] <> pagerHintsLogHook
                             >> (myDynamicLogWithPP tc $ (myPP home) { ppOutput = hPutStrLn din })
                             >> (myDynamicLogWithPP tc $ (myPP home) { ppOutput = hPutStrLn din })
                             >> (myDynamicLogWithPP tc $ (myPP home) { ppOutput = hPutStrLn din2 })
                             >> (myDynamicLogWithPP tc $ (myPP home) { ppOutput = hPutStrLn din3 })
--                             >> (myDynamicLogWithPP tc $ (myPP home) { ppOutput = hPutStrLn din4 })
        }

