  -- Base
import XMonad
import XMonad.Config.Desktop
import System.Directory
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace, toUpper, isDigit)
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarColor, xmobarPP, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, isDialog, doCenterFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.InsertPosition (Position(End), Focus(Newer), insertPosition)

    -- Layouts
import XMonad.Layout.Gaps
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Named
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))
import qualified XMonad.Layout.Dwindle as Dwindle

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

myFont :: String
myFont = "xft:UbuntuMono Nerd Font:regular:size=12:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask        -- modkey to super/windows key

altMask :: KeyMask
altMask = mod1Mask

myTerminal :: String
myTerminal = "alacritty"    -- default terminal

myTerm2 :: String
myTerm2 = "st"

myvlc :: String
myvlc = "vlc"

myTelegram :: String
myTelegram = "Telegram"

myobs :: String
myobs = "obs"

myBrowser :: String
myBrowser = "firefox"

myEditor :: String
myEditor = myTerminal ++ " -e nvim "  -- nvim as editor

myBorderWidth :: Dimension
myBorderWidth = 4           -- window border

myNormColor :: String
myNormColor   = "#282c34"   -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#4d78cc"   -- Border color of focused windows

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"

---------------------------------------------
----Scratchpads
---------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                 ,NS "ranger" spawnRngr findRngr manageRngr
                 ,NS "vlc" spawnVlc findVlc manageVlc
                 ,NS "Telegram" spawnTgrm findTgrm manageTgrm
                 ,NS "obs" spawnObs findObs manageObs
                ]
  where
    spawnTerm  = myTerm2 ++ " -n scratchpad"
    findTerm   = resource =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.5
                 t = 0.75 -h
                 l = 0.75 -w
    spawnRngr  = myTerm2 ++ " -n ranger 'ranger'"
    findRngr  = resource =? "ranger"
    manageRngr = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.5
                 t = 0.75 -h
                 l = 0.75 -w
    spawnVlc  = myvlc
    findVlc  = resource =? "vlc"
    manageVlc = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.5
                 t = 0.75 -h
                 l = 0.75 -w
    spawnTgrm  = myTelegram
    findTgrm  = resource =? "Telegram"
    manageTgrm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.5
                 t = 0.75 -h
                 l = 0.75 -w
    spawnObs  = myobs
    findObs  = resource =? "obs"
    manageObs = customFloating $ W.RationalRect l t w h
               where
                 h = 0.7
                 w = 0.7
                 t = 0.85 -h
                 l = 0.85 -w

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 9
           $ ResizableTall 1 (1/100) (1/2) []
dwindle  = renamed [Replace "dwindle"]
           $ mySpacing 9
           $ limitWindows 12
           $ Dwindle.Dwindle R Dwindle.CW (2/2) (11/10)
monocle  = renamed [Replace "monocle"]
           $ limitWindows 20 Full
grid     = renamed [Replace "grid"]
           $ limitWindows 12
           $ mySpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
threeCol = renamed [Replace "threeCol"]
           $ mySpacing' 9
           $ limitWindows 7
           $ ThreeCol 1 (1/100) (1/2)
threeColMid = renamed [Replace "|C|"]
           $ mySpacing' 9
           $ limitWindows 7
           $ ThreeColMid 1 (1/100) (1/2)
floats   = renamed [Replace "float"]
           $ limitWindows 20 simplestFloat

gap :: Int
gap = 18

fi = fromIntegral

mrt = mouseResizableTile { draggerType = FixedDragger (fi gap) (fi gap) }
applyGaps = gaps $ zip [U, D, R, L] $ repeat gap

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder myBorderWidth tall
                                 ||| dwindle
                                 ||| avoidStruts (applyGaps mrt)
                                 ||| grid
                                 ||| threeCol
                                 ||| threeColMid
                                 ||| noBorders monocle
                                 ||| floats

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = (isDialog --> doF W.swapUp)
               <+> composeAll
               [ manageDocks
               , (className =? "firefox" <&&> title =? "Library") --> doCenterFloat
               , (className =? "gcolor2")        --> doCenterFloat
               , (className =? "Gcolor2")        --> doCenterFloat
               , (className =? "mpv")            --> doCenterFloat
               , (className =? "obs")            --> doCenterFloat
               , (className =? "Lxappearance")   --> doCenterFloat
               , (resource  =? "kdesktop")       --> doIgnore
               , (resource  =? "desktop_window") --> doIgnore
               , isDialog --> doCenterFloat
               , namedScratchpadManageHook myScratchPads
               , insertPosition End Newer
               ]

myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")  -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")    -- Restarts xmonad
        , ("M-S-q", io exitSuccess)              -- Quits xmonad

    --  Terminal, Browser, Htop
        , ("M-<Return>", spawn (myTerminal))
        , ("M-b", spawn (myBrowser))
        , ("M-M1-h", spawn (myTerminal ++ " -e htop"))

    -- Kill windows
        , ("M-q", kill1)     -- Kill the currently focused client
        , ("M-S-a", killAll)   -- Kill all windows on current workspace

    -- Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor
        , ("M-S-<KP_Add>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-S-<KP_Subtract>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws

    -- Floating windows
        , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
        , ("M-d", decWindowSpacing 4)           -- Decrease window spacing
        , ("M-i", incWindowSpacing 4)           -- Increase window spacing
        , ("M-S-d", decScreenSpacing 4)         -- Decrease screen spacing
        , ("M-S-i", incScreenSpacing 4)         -- Increase screen spacing

    -- Windows navigation
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-S-<Space>", sendMessage ToggleStruts)     -- Toggles struts
        , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)  -- Toggles noborder
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full

    -- Increase/decrease windows in the master pane or the stack
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase # of clients master pane
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease # of clients master pane
        , ("M-C-<Up>", increaseLimit)                   -- Increase # of windows
        , ("M-C-<Down>", decreaseLimit)                 -- Decrease # of windows

    -- Window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Exoand vert window width

    ---- Rofi and Dmenu Scripts
        , ("C-<Space>", spawn "rofi -show drun")
        , ("M1-<Space>", spawn "dmenu_run")

    -- Scratchpads
        , ("M1-t", namedScratchpadAction myScratchPads "terminal")
        , ("M1-r", namedScratchpadAction myScratchPads "ranger")
        , ("M1-S-v", namedScratchpadAction myScratchPads "vlc")
        , ("M1-S-t", namedScratchpadAction myScratchPads "Telegram")
        , ("M1-o", namedScratchpadAction myScratchPads "obs")

    --- My Applications
        , ("M1-d", spawn "gnome-disks")
        , ("M1-e", spawn "thunar")
        , ("M1-g", spawn "gthumb")
        , ("M1-m", spawn "gnome-system-monitor")
        , ("M1-v", spawn "code")

    -- Multimedia Keys
        , ("<XF86AudioPlay>", spawn "playerctl play-pause")
        , ("<XF86AudioPrev>", spawn "playerctl previous")
        , ("<XF86AudioNext>", spawn "playerctl next")
        , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
        , ("<XF86HomePage>", spawn "firefox")
        , ("<Print>", spawn "screenshot")
        ]
    -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))


main :: IO ()
main = do

    xmbar <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobar.hs"
    xmonad $ ewmh def
        { manageHook = myManageHook <+> manageDocks
        , handleEventHook    = docksEventHook <+> fullscreenEventHook
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = dynamicLogWithPP $ namedScratchpadFilterOutWorkspacePP $ xmobarPP
              { ppOutput = \x -> hPutStrLn xmbar x                              -- xmobar
              , ppCurrent = xmobarColor "#71abeb" "" . wrap "[" "]"             -- Current workspace
              , ppVisible = xmobarColor "#5AB1BB" "" . clickable                -- Visible but not current workspace
              , ppHidden = xmobarColor "#e5c07b" ""  . wrap "*" "" . clickable  -- Hidden workspaces
              , ppHiddenNoWindows = xmobarColor "#d6d5d5" ""  . clickable       -- Hidden workspaces (no windows)
              , ppWsSep   = "  "                                                -- Workspaces separator
              , ppTitle = xmobarColor "#9ec07c" "" . shorten 60                 -- Title of active window
              , ppSep =  "<fc=#4b5363> <fn=1>|</fn> </fc>"                      -- Separator character
              , ppUrgent = xmobarColor "#e06c75" "" . wrap "!" "!"              -- Urgent workspace
              , ppExtras  = [windowCount]                                       -- # of windows current workspace
              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]                      -- order of things in xmobar
              , ppLayout  = xmobarColor "#c678dd" "" .
                  ( \t -> case t of
                      "MouseResizableTile" -> "MRT"
                      _ -> t
                  )
              }
        } `additionalKeysP` myKeys
