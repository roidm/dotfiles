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
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, isDialog, doCenterFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.InsertPosition as H

    -- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.ResizableTile
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ShowWName
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

myFont :: String
myFont = "xft:UbuntuMono Nerd Font:regular:size=12:antialias=true:hinting=true"

myEmojiFont :: String
myEmojiFont = "xft:JoyPixels:regular:size=12:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask        -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"    -- Sets default terminal

myTerm2 :: String
myTerm2 = "st"

myvlc :: String
myvlc = "vlc"

mytrm :: String
mytrm = "trm"

myBrowser :: String
myBrowser = "firefox"

myEditor :: String
myEditor = myTerminal ++ " -e nvim "    -- Sets vim as editor

myBorderWidth :: Dimension
myBorderWidth = 4           -- Sets border width for windows

myNormColor :: String
myNormColor   = "#282c34"   -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#4d78cc"   -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask          -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myStartupHook :: X ()
myStartupHook = do
    spawn "$HOME/.xmonad/scripts/polybar.sh"
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"

---------------------------------------------
----Scratchpads
---------------------------------------------

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                 ,NS "ranger" spawnRngr findRngr manageRngr
                 ,NS "vlc" spawnVlc findVlc manageVlc
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
    spawnTrm  = mytrm
    findTrm  = resource =? "trm"
    manageTrm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.5
                 t = 0.75 -h
                 l = 0.75 -w             

--Makes setting the spacingRaw simpler to write. The spacingRaw module adds a configurable amount of space around windows.
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
-- limitWindows n sets maximum number of windows displayed for layout.
-- mySpacing n sets the gap size around the windows.
tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ mySpacing 6
           $ ResizableTall 1 (1/100) (1/2) []
bsp        = renamed [Replace "bsp"] 
           $ limitWindows 12
           $ mySpacing 6
           $ emptyBSP 
monocle  = renamed [Replace "[M]"]
           $ limitWindows 20 Full
grid     = renamed [Replace "HHH"]
           $ limitWindows 12
           $ mySpacing 6
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
threeCol = renamed [Replace "|||"]
           $ mySpacing 6
           $ limitWindows 7
           $ ThreeCol 1 (1/100) (1/2)
threeColMid = renamed [Replace "|C|"]
           $ mySpacing 6
           $ limitWindows 7
           $ ThreeColMid 1 (1/100) (1/2)
floats   = renamed [Replace "><>"]
           $ limitWindows 20 simplestFloat
                     

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               myDefaultLayout =     withBorder myBorderWidth tall
                                 ||| bsp
                                 ||| noBorders monocle
                                 ||| grid
                                 ||| threeCol
                                 ||| threeColMid
                                 ||| floats
                                 ||| mouseResizableTile { masterFrac = 0.5, fracIncrement = 0.05, draggerType = (FixedDragger 6 6)}
                                 --{ masterFrac = 0.5, fracIncrement = 0.05, draggerType = FixedDragger}



myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
--myWorkspaces = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]
myWorkspaceIndices = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------
myManageHook :: Query (Data.Monoid.Endo WindowSet)
myManageHook = (isDialog --> doF W.swapUp)                       -- Bring Dialog Window on Top of Parent Floating Window
               <+> insertPosition Below Newer                    -- Insert New Windows at the Bottom of Stack Area
               <+> namedScratchpadManageHook myScratchPads       -- Adding Rules for Named Scratchpads
               <+> composeAll
               [ (className =? "firefox" <&&> title =? "Library") --> doCenterFloat    -- Float Firefox Downloads Window to Centre
               , (className =? "gcolor3")        --> doCenterFloat
               , (className =? "Gcolor3")        --> doCenterFloat
               , (className =? "mpv")            --> doCenterFloat
               , (className =? "Lxappearance")   --> doCenterFloat                     -- Float Lxappearance to Centre
               , (resource  =? "kdesktop")       --> doIgnore
               , (resource  =? "desktop_window") --> doIgnore
               , isDialog --> doCenterFloat                                            -- Float Dialog Windows to Centre
               ]

myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")  -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")    -- Restarts xmonad
        , ("M-S-q", io exitSuccess)              -- Quits xmonad

    -- Useful programs to have a keybinding for launch
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
        , ("M1-u", namedScratchpadAction myScratchPads "ranger")
        , ("M1-o", namedScratchpadAction myScratchPads "vlc")
        , ("M1-y", namedScratchpadAction myScratchPads "trm")

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


--Process Colours
type Colour = String
polybarColour :: Char -> Colour -> String -> String
polybarColour area (_:color) text = "%{" ++ [area] ++ color ++ "}" ++ text ++ "%{" ++ area:"--}"

colourCurrent = "#71abeb"
colourVisible = "#5AB1BB"
colourHidden  = "#e5c07b"
colourTitle   = "#9ec07c"
colourLyt     = "#c678dd"
colourSep     = "#4b5363"
colourHiNoWin = "#d6d5d5"
colourBG      = "#282c34"

----------------------------------------------
--Polybar dbus myLogHook
----------------------------------------------
myLogHook dbus =
  let signal     = D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")
      output str = D.emit dbus $ signal { D.signalBody = [D.toVariant $ UTF8.decodeString str] }
  in dynamicLogWithPP $ def
    { ppOutput = output
    , ppCurrent = polybarColour 'F' colourCurrent 
    , ppVisible = polybarColour 'F' colourVisible
    , ppLayout  = polybarColour 'F' colourLyt . removeWord "Hinted" . removeWord "Spacing"
    , ppHidden  = polybarColour 'F' colourHidden . noScratchPad
    , ppHiddenNoWindows = polybarColour 'F' colourHiNoWin . noScratchPad
    , ppWsSep   = "  "
    , ppSep     = polybarColour 'F' colourSep  " | "
    , ppTitle   = polybarColour 'F' colourTitle . shorten 60
    , ppExtras  = [windowCount]
    , ppOrder   = \(ws:l:t:ex) -> [ws,l]++ex++[t]
    }
    where
    -- then define it down here: if the workspace is NSP then print
    -- nothing, else print it as-is
    removeWord substr = unwords . filter (/= substr) . words
    noScratchPad ws = if ws == "NSP" then "" else ws

main :: IO ()
main = do

    --Polybar
    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log") [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    -- the xmonad, ya know...what the WM is named after!
    xmonad $ ewmh desktopConfig
        { manageHook = myManageHook <+> manageDocks
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        , workspaces         = myWorkspaces
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormColor
        , focusedBorderColor = myFocusColor
        , logHook = myLogHook dbus
        } `additionalKeysP` myKeys
