{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}
------------------------------------------------------------------------}}}
-- Modules     0.17                                                     {{{
---------------------------------------------------------------------------
-- XMONAD
import XMonad

import Data.Char (isSpace, toUpper)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Data.Monoid

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- Actions
import XMonad.Actions.UpdatePointer
import XMonad.Actions.SpawnOn
import XMonad.Actions.WithAll
import XMonad.Actions.Submap as SM
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.Navigation2D
import XMonad.Actions.GridSelect
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesUp, rotSlavesDown, rotAllDown)

import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Actions.Search as S

-- Systems
import System.Exit
import System.IO (hClose, hPutStr, hPutStrLn)
import System.Posix.Unistd (nodeName, getSystemID)
import System.Process

-- Hooks
import XMonad.ManageHook
import XMonad.Hooks.DebugKeyEvents
import XMonad.Hooks.DynamicLog
import XMonad.Actions.DynamicProjects
import XMonad.Hooks.EwmhDesktops as E
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.InsertPosition (Focus(Newer), Position(Below), insertPosition)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, ToggleStruts(..), docksStartupHook)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ScreenCorners
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.WindowSwallowing

-- Layouts
import XMonad.Layout.Fullscreen -- hiding (fullscreenEventHook)
import XMonad.Layout.Gaps
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TrackFloating
import XMonad.Layout.MultiToggle
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import XMonad.Layout.MultiToggle.Instances

import qualified XMonad.Layout.GridVariants as G

-- Utilities, other
import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.Paste
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

-- Prompts
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt


------------------------------------------------------------------------}}}
-- Vars                                                                 {{{
---------------------------------------------------------------------------
myModMask               = mod4Mask  -- Sets modkey to super/windows key
myModAlt                = mod1Mask  -- Left Alt

myTerminal              = "alacritty"
myAltTerminal           = "urxvt"
myStTerminal            = "st"
myBrowser               = "brave"
myFileMgr               = "pcmanfm"

myLauncher              = "dmenu_run"
myStatusBar             = "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
windowCount             = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

base03                  = "#002b36"
base02                  = "#82AAFF"
base01                  = "#586e75"
base00                  = "#657b83"
base0                   = "#839496"
base1                   = "#93a1a1"
base2                   = "#eee8d5"
base3                   = "#fdf6e3"
yellow                  = "#b58900"
orange                  = "#cb4b16"
red                     = "#dc322f"
magenta                 = "#d33682"
violet                  = "#6c71c4"
blue                    = "#0092EB"
cyan                    = "#2aa198"
green                   = "#859900"
grey                    = "#949494"
window                  = "#90BBBD"

-- sizes
gap                     = 8
topbar                  = 10
mainborder              = 0
prompt                  = 32
status                  = 32

active                  = blue
activeWarn              = red
inactive                = base02
focusColor              = blue
unfocusColor            = base02

myNormalBorderColor     = "#000000"
myFocusedBorderColor    = active

myBorderWidth           = 0
myBorderSpace           = 4

myFocusFollowsMouse     = True
myClickJustFocuses      = False

myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

topBarTheme = def
    { fontName              = myFont
    , inactiveBorderColor   = base03
    , inactiveColor         = base03
    , inactiveTextColor     = base03
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = yellow
    , decoHeight            = topbar
    }

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = base02
    , activeBorderColor     = active
    , inactiveBorderColor   = base02
    , activeTextColor       = base03
    , inactiveTextColor     = base00
    }

myPromptTheme = def
    { font                  = myFont
    , bgColor               = base03
    , fgColor               = active
    , fgHLight              = base03
    , bgHLight              = active
    , borderColor           = base03
    , promptBorderWidth     = 0
    , height                = prompt
    , position              = Top
    }

warmPromptTheme = myPromptTheme
    { bgColor               = yellow
    , fgColor               = base03
    , position              = Top
    }

hotPromptTheme = myPromptTheme
    { bgColor               = red
    , fgColor               = base3
    , position              = Top
    }


------------------------------------------------------------------------}}}
-- Startup/Event Hooks                                                  {{{
---------------------------------------------------------------------------
myStartupHook = do
    spawn "~/.local/scripts/startup.sh"
    setDefaultCursor xC_left_ptr
    return ()


myEventHook = debugKeyEvents
    <+> fadeWindowsEventHook



------------------------------------------------------------------------}}}
-- Window rules/ManageHook                                              {{{
---------------------------------------------------------------------------
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
-- resource (also known as appName) is the first element in WM_CLASS(STRING)
-- className is the second element in WM_CLASS(STRING)
-- title is WM_NAME(STRING)
-- ALSO
-- xdotool selectwindow getwindowname

myManageHook ::  ManageHook
myDFloat = doRectFloat (W.RationalRect (1/6) (1/6) (2/3) (2/3))
myManageHook = composeAll . concat $
  [ [isDialog --> myDFloat]
  , [className =? c --> doCenterFloat | c <- myCFloats]
  , [title =? t --> doCenterFloat | t <- myTFloats]
  , [resource =? r --> doCenterFloat | r <- myRFloats]
  , [(className =? i <||> title =? i <||> resource =? i) --> doIgnore | i <- myIgnores]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 0) | x <- my1Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 1) | x <- my2Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 2) | x <- my3Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 3) | x <- my4Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 4) | x <- my5Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 5) | x <- my6Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 6) | x <- my7Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 7) | x <- my8Shifts]
  , [(className =? x <||> title =? x <||> resource =? x) --> doShift (myWorkspaces !! 8) | x <- my9Shifts]
  ]
  where
-- classes / titles / resources
  myCFloats = []
  myTFloats = []
  myRFloats = []
  myIgnores = []
  my1Shifts = []
  my2Shifts = ["Firefox","Brave-browser"]
  my3Shifts = ["mumble"]
  my4Shifts = ["Pcmanfm"]
  my5Shifts = ["remmina","xfreerdp","rdesktop","microsoft teams - preview"]
  my6Shifts = ["VirtualBox Manager","VirtualBox","virt-manager"]
  my7Shifts = []
  my8Shifts = []
  my9Shifts = []


------------------------------------------------------------------------}}}
-- LogHook & Bars                                                       {{{
---------------------------------------------------------------------------
myLogHook h = do

   fadeWindowsLogHook myFadeHook

   dynamicLogWithPP $ def
        { ppOutput = \x -> hPutStrLn h x
        , ppCurrent = xmobarColor active "" . wrap "[" "]"                  -- Current workspace in xmobar
        , ppVisible = xmobarColor base0 ""                                  -- Visible but not current workspace
        , ppHidden = xmobarColor base02 "" . wrap " " " "                   -- Hidden workspaces in xmobar
--        , ppHiddenNoWindows = xmobarColor "#F07178" "" . wrap " " " "       -- Hidden workspaces (no windows)
        , ppHiddenNoWindows = const ""                                      -- only show ws with content
        , ppTitle = xmobarColor window "" . shorten 60                      -- Title of active window in xmobar
        , ppSep =  "<fc=#949494> <fn=1>| </fn></fc>"                        -- Separators in xmobar
        , ppWsSep = " ~ "
        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"                -- Urgent workspace
        , ppExtras = [windowCount]                                          -- # of windows current workspace
        , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        -- , ppOrder = \(ws:l:t:_)   -> [ws]                                -- removes everything excepts wspaces
        , ppLayout = (\ x -> case x of
                      "||=" -> "<icon="++iconsdir++"layout-tall-right.xbm/>"
                      " + " -> "<icon="++iconsdir++"layout-grid.xbm/>"
                      "TTT" -> "<icon="++iconsdir++"layout-mirror-top.xbm/>"
                      "=[]" -> "<icon="++iconsdir++"layout-tall-right.xbm/>"
                      "_+_" -> "<icon="++iconsdir++"layout-mirror-bottom.xbm/>"
                      "[]=" -> "<icon="++iconsdir++"layout-tall-left.xbm/>"
                      "|||" -> "<icon="++iconsdir++"layout-threecol.xbm/>"
                      "[@]" -> "<icon="++iconsdir++"layout-bsp.xbm/>"
                      "[_]" -> "<icon="++iconsdir++"layout-full.xbm/>"
                     )
        , ppSort = fmap (.filterOutWs [scratchpadWorkspaceTag]) getSortByIndex
        }
        where
          iconsdir = ("$HOME/.config/xmonad" ++ "/icons/")


myFadeHook = composeAll
    [ opaque -- default to opaque
    --transparency 0.06
    --, isUnfocused --> opacity 0.9
    ,(className =? "mpv") --> opacity 1.0
    ,(className =? "mpv") <&&> (isUnfocused) --> opacity 1.0
    ,(className =? "Brave-browser") --> opacity 1.0
    ,(className =? "Brave-browser") <&&> (isUnfocused) --> opacity 1.0
    ,(className =? "Alacritty") --> opacity 0.85
    ,(className =? "Alacritty") <&&> (isUnfocused) --> opacity 0.85
    ,(className =? "Signal") --> opacity 0.95
    ,(className =? "Signal") <&&> (isUnfocused) --> opacity 0.95
    ,(className =? "st-256color") --> opacity 0.95
    ,(className =? "st-256color") <&&> (isUnfocused) --> opacity 0.95
    ,(className =? "Leafpad") --> opacity 0.96
    ,(className =? "Leafpad") <&&> (isUnfocused) --> opacity 0.96
    ,(className =? "Geany") <&&> (isUnfocused) --> opacity 0.9
    ,(className =? "Pcmanfm") --> opacity 0.9
    ,(className =? "Pcmanfm") <&&> (isUnfocused) --> opacity 0.9
    , isDialog --> opaque
    ]


-- WINDOW SWALLOWING
myHandleEventHook = swallowEventHook (className =? "Alacritty" <||> className =? "Termite") (return True)


------------------------------------------------------------------------}}}
-- Workspaces                                                           {{{
---------------------------------------------------------------------------

xmobarEscape = concatMap doubleLts
  where
    doubleLts '<' = "<<"
    doubleLts x   = [x]

wsGEN   = "GEN"
wsWEB   = "WEB"
wsSYS   = "SYS"
wsMGR   = "MGR"
wsDEV   = "DEV"
wsVIRT  = "VIRT"
wsCOM   = "COM"
wsMON   = "MON"
wsFLOAT = "FLT"

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape)
               $ [wsGEN, wsWEB, wsDEV, wsMGR, wsSYS, wsVIRT, wsCOM, wsMON, wsFLOAT]
  where
    clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                  (i,ws) <- zip [1..9] l,
                  let n = i ]


projects :: [Project]
projects =

    [ Project   { projectName       = "<action=xdotool key super+2>WEB</action>"
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawn myBrowser
                }

    , Project   { projectName       = "<action=xdotool key super+8>MON</action>"
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsMON (myTerminal ++ " -e htop")
                                                spawnOn wsMON (myTerminal ++ " -e gotop")
                                                spawnOn wsMON (myTerminal ++ " -e tty-clock")
                }

    , Project   { projectName       = "<action=xdotool key super+4>MGR</action>"
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsMGR myFileMgr
                                                --spawnOn wsMGR myAltFileMgr
                                                --spawnOn wsMGR (myTerminal ++ " -e ranger")
                }
    ]



------------------------------------------------------------------------}}}
-- ScratchPads                                                          {{{
---------------------------------------------------------------------------


-- RationalRect guides
  -- top half, perfect fit
  -- (0/1) (0/1) (1/1) (1/2)
  -- bottom half of screen space around edge
  -- (1/100) (49/100) (98/100) (1/2)
  -- centered square
  -- (1/4 (1/4) (1/2) (1/2)

scratchpads = [
    NS "term" "st -n scratchpad" (resource =? "scratchpad")
        (customFloating $ W.RationalRect (1/100) (1/10) (98/100) (8/10)),

    NS "music" "st -n ncmpcpp -e ncmpcpp" (resource =? "ncmpcpp")
        (customFloating $ W.RationalRect (1/100) (1/10) (98/100) (8/10)),

    NS "ncpamixer" "st -n ncpamixer -e ncpamixer" (resource =? "ncpamixer")
        (customFloating $ W.RationalRect (1/4) (2/3) (2/4) (1/4)),

    NS "calculator" "qalculate-gtk" (resource =? "Qalculate-gtk")
        (customFloating $ W.RationalRect (1/4) (2/3) (2/4) (1/4))

 ] where role = stringProperty "WM_WINDOW_ROLE"


------------------------------------------------------------------------}}}
-- Main                                                                 {{{
---------------------------------------------------------------------------
main = do
     xmproc <- spawnPipe myStatusBar

     xmonad
       $ dynamicProjects projects
       $ docks
       $ ewmhFullscreen
       $ ewmh
       $ withUrgencyHook LibNotifyUrgencyHook
       $ addDescrKeys ((mod4Mask, xK_F1), showKeybindings) myKeys
       $ myConfig xmproc

myConfig p = def
        { modMask            = myModMask
        , terminal           = myTerminal
        , clickJustFocuses   = myClickJustFocuses
        , focusFollowsMouse  = myFocusFollowsMouse
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth        = myBorderWidth
        , workspaces         = myWorkspaces
        , manageHook         = myManageHook <+> namedScratchpadManageHook scratchpads
        , handleEventHook    = myHandleEventHook
        , layoutHook         = myLayoutHook
        , logHook            = myLogHook p
        , startupHook        = myStartupHook
        }



------------------------------------------------------------------------}}}
-- Layouts                                                              {{{
---------------------------------------------------------------------------
mySpacing bs = spacingRaw True (uniformBorder bs) True (uniformBorder bs) True
   where
       uniformBorder n = Border n n n n

gapper s = (gaps [(U, s), (R, s), (L, s), (D, s)]) . mySpacing (toInteger s)
grid orientation s = gapper s $ G.SplitGrid orientation 1 1 (1/2) (16/9) (5/100)

myLayoutHook = fullScreenToggle $ tall ||| avoidStruts (standardLayouts) ||| threeLayout ||| spirals

  where
    fullScreenToggle = mkToggle (single FULL)

    standardLayouts =
                    named " + " (noFrillsDeco shrinkText topBarTheme $ mySpacing (toInteger myBorderSpace) $ reflectVert $ G.Grid (16/10)) -- grid -- reflectvert spawns windows on the left
                    ||| named "TTT" (noFrillsDeco shrinkText topBarTheme $ grid G.B myBorderSpace) -- grid ; bottom ; spacing
                    ||| named "[]=" (noFrillsDeco shrinkText topBarTheme $ grid G.L myBorderSpace) -- grid ; left ; spacing
                    ||| named "_+_" (noFrillsDeco shrinkText topBarTheme $ grid G.T myBorderSpace) -- grid ; Top ; spacing
                    ||| named "=[]" (noFrillsDeco shrinkText topBarTheme $ grid G.R myBorderSpace) -- grid ; right ; spacing

    tall = named "||=" $ avoidStruts $ (noFrillsDeco shrinkText topBarTheme $ mySpacing (toInteger myBorderSpace) $ ResizableTall 1 (3/100) (1/2) [])
    threeLayout = named "|||" $ avoidStruts $ noFrillsDeco shrinkText topBarTheme $ mySpacing (toInteger myBorderSpace) $ (ThreeColMid 1 (3/100) (1/3))
    spirals  = named "[@]" $ avoidStruts $ noFrillsDeco shrinkText topBarTheme $ mySpacing (toInteger myBorderSpace) $ spiral (6/7)




-- colors
type Hex = String
type ColorCode = (Hex,Hex)
type ColorMap = M.Map Colors ColorCode

data Colors = Black | Red | Green | Yellow | Blue | Orange | Magenta | Cyan | White | Purple | BG | Hue
    deriving (Ord,Show,Eq)

colorLook :: Colors -> Int -> Hex
colorLook color n =
    case M.lookup color colors of
        Nothing -> "#000000"
        Just (c1,c2) -> if n == 0 then c1 else c2

colors :: ColorMap
colors = M.fromList
    [ (Black , ("#000000", "#121212")) -- black and gray7
    , (Red , ("#ff0000", "#f21835")) --
    , (Green , ("#0dd40d", "#006400")) --
    , (Yellow , ("#FFFF00", "#C3C32D")) --
    , (Blue , ("#000040", "#80c0ff")) --
    , (Orange , ("#ff8C00", "#ce260b")) --
    , (Magenta , ("#6f4484", "#915eaa")) --
    , (Cyan , ("#2B7694", "#47959E")) --
    , (White , ("#D6D6D6", "#A3A3A3")) --
    , (Purple , ("#9B30FF", "#7D26CD")) --
    , (BG , ("#000000", "#444444")) --
    , (Hue , ("#0e2f44", "#262b56")) --
    ]



------------------------------------------------------------------------}}}
-- LibNotify                                                            {{{
---------------------------------------------------------------------------
-- to evoke urgent bell on terminal run:
-- sleep 2; echo -e '\a'
-- on a non visible workspace. make sure terminal has urgent on
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "" ++ idx]



------------------------------------------------------------------------}}}
-- Keys                                                                 {{{
---------------------------------------------------------------------------
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
  h <- spawnPipe $ "yad --text-info --fontname=\"SauceCodePro Nerd Font Mono 12\" --fore=#46d9ff back=#282c36 --center --geometry=1200x800 --title \"XMonad keybindings\""
  hPutStr h (unlines $ showKm x)
  hClose h
  return ()

subtitle' ::  String -> ((KeyMask, KeySym), NamedAction)
subtitle' x = ((0,0), NamedAction $ map toUpper
                      $ sep ++ "\n-- " ++ x ++ " --\n" ++ sep)
  where
    sep = replicate (6 + length x) '-'


myKeys :: XConfig l0 -> [((KeyMask, KeySym), NamedAction)]
myKeys c =
  --(subtitle "Custom Keys":) $ mkNamedKeymap c $
  let subKeys str ks = subtitle' str : mkNamedKeymap c ks in

  subKeys "Xmonad"
  [ ("M-q",               addName "Recompile/Restart XMonad"               $ confirmPrompt hotPromptTheme "restart xmonad?" $ spawn "xmonad --recompile && xmonad --restart")
  , ("M-S-q",             addName "Quit XMonad"                            $ confirmPrompt hotPromptTheme "kill xmonad?" $ io (exitWith ExitSuccess))
  , ("M-<Backspace>",     addName "Kill focused window"                    $ kill1)
  , ("M-S-<Backspace>",   addName "Kill all windows on WS"                 $ confirmPrompt hotPromptTheme "kill all?" $ killAll)
  ]

  ^++^ subKeys "Window navigation"
  [ ("M-j",               addName "Move focus to next window"              $ windows W.focusDown)
  , ("M-<Down>",          addName "Move focus to next window"              $ windows W.focusDown)
  , ("M-k",               addName "Move focus to prev window"              $ windows W.focusUp)
  , ("M-<Up>",            addName "Move focus to prev window"              $ windows W.focusUp)
  , ("M-m",               addName "Move focus to master window"            $ windows W.focusMaster)
  , ("M-S-j",             addName "Swap focused window with next window"   $ windows W.swapDown)
  , ("M-S-<Down>",        addName "Swap focused window with next window"   $ windows W.swapDown)
  , ("M-S-k",             addName "Swap focused window with prev window"   $ windows W.swapUp)
  , ("M-S-<Up>",          addName "Swap focused window with prev window"   $ windows W.swapUp)
  , ("M-S-m",             addName "Swap focused window with master window" $ windows W.swapMaster)
  , ("M-a",               addName "Move focused window to master"          $ promote)
  , ("M-S-,",             addName "Rotate all windows except master"       $ rotSlavesDown)
  , ("M-S-.",             addName "Rotate all windows current stack"       $ rotAllDown)
  , ("M-f",               addName "Fullscreen"                             $ sequence_ [ (withFocused $ windows . W.sink)
                                                                                       ,(sendMessage $ XMonad.Layout.MultiToggle.Toggle FULL) ])
  ]

  ^++^ subKeys "Move window to WS and go there"
  [ ("M-S-<Page_Up>",     addName "Move window to next WS"                 $ shiftTo Next nonNSP >> moveTo Next nonNSP)
  , ("M-S-<Page_Down>",   addName "Move window to prev WS"                 $ shiftTo Prev nonNSP >> moveTo Prev nonNSP)
  ]

  -- Floating windows
  ^++^ subKeys "Floating windows"
  [ ("M-S-f",             addName "Toggle float layout"                    $ sendMessage (T.Toggle "floats"))
  , ("M-t",               addName "Sink a floating window"                 $ withFocused $ windows . W.sink)
  , ("M-S-t",             addName "Sink all floated windows"               $ sinkAll)
  ]


  -- Scratchpads
  -- Toggle show/hide these programs. They run on a hidden workspace.
  -- When you toggle them to show, it brings them to current workspace.
  -- Toggle them to hide and it sends them back to hidden workspace (NSP).

  ^++^ subKeys "Scratchpads"
  [ ("M-M1-t",            addName "Toggle scratchpad terminal"             $ namedScratchpadAction scratchpads "term")
  , ("M-M1-r",            addName "Toggle scratchpad npcamixer"            $ namedScratchpadAction scratchpads "ncpamixer")
  , ("M-M1-m",            addName "Toggle scratchpad ncmpcpp"              $ namedScratchpadAction scratchpads "music")
  , ("M-M1-c",            addName "Toggle scratchpad calculator"           $ namedScratchpadAction scratchpads "calculator")
  ]

  -- Increase/decrease spacing (gaps)
  ^++^ subKeys "Window spacing (gaps)"
  [ ("C-M1-j",            addName "Decrease window spacing"                $ decWindowSpacing 4)
  , ("C-M1-k",            addName "Increase window spacing"                $ incWindowSpacing 4)
  , ("C-M1-h",            addName "Decrease screen spacing"                $ decScreenSpacing 4)
  , ("C-M1-l",            addName "Increase screen spacing"                $ incScreenSpacing 4)
  ]

  -- Increase/decrease windows in the master pane or the stack
  ^++^ subKeys "Increase/decrease windows in master pane or the stack"
  [ ("M-M1-<Up>",          addName "Increase clients in master"            $ sendMessage (IncMasterN 1))
  , ("M-M1-<Down>",        addName "Decrease clients in master"            $ sendMessage (IncMasterN (-1)))
  ]

  -- Window resizing
  ^++^ subKeys "Window resizing"
  [ ("M-[",               addName "Shrink window"                          $ sendMessage Shrink)
  , ("M-]",               addName "Expand window"                          $ sendMessage Expand)
  , ("M-S-[",             addName "Shrink window vertically"               $ sendMessage MirrorShrink)
  , ("M-S-]",             addName "Expand window vertically"               $ sendMessage MirrorExpand)
  ]

  -- Volume
  ^++^ subKeys "Volume"
  [ ("M-S--",             addName "Lower Volume"                           $ spawn "pactl set-sink-volume @DEFAULT_SINK@ -4%")
  , ("M-S-=",             addName "Raise Volume"                           $ spawn "pactl set-sink-volume @DEFAULT_SINK@ +4%")
  , ("M-S-0",             addName "Toggle Mute"                            $ spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  ]

  ^++^ subKeys "Apps"
  [ ("M-<Return>",        addName "Launch terminal"                        $ spawn (myTerminal))
  , ("M-C-\\",            addName "Launch web browser"                     $ spawn (myBrowser))
  , ("M-C-b",             addName "Launch web browser"                     $ spawn "blueman-manager")
  , ("M-C-c",             addName "gcolor2"                                $ spawn "gcolor2")
  , ("M-C-e",             addName "PcmanFM"                                $ spawn "pcmanfm")
  , ("M-C-f",             addName "Flameshot"                              $ spawn (myTerminal ++ " -e flameshot gui"))
  , ("M-C-g",             addName "Geany"                                  $ spawn "geany")
  , ("M-M1-h",            addName "Launch htop"                            $ spawn (myTerminal ++ " -e htop"))
  , ("M-C-l",             addName "leafpad"                                $ spawn "leafpad")
  , ("M-C-p",             addName "PcmanFM"                                $ spawn "pcmanfm")
  , ("M-C-q",             addName "QEMU Virt-Manager"                      $ spawn "virt-manager")
  , ("M-C-r",             addName "Remmina"                                $ spawn "remmina")
  , ("M-C-t",             addName "Todoist"                                $ spawn "todoist")
  , ("M-C-u",             addName "unimatrix"                              $ spawn (myTerminal ++ " -e unimatrix"))
  , ("M-C-v",             addName "VSCodium"                               $ spawn "vscodium")
  ]
    where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))


-- vim: ft=haskell:foldmethod=marker:expandtab:ts=4:shiftwidth=4
