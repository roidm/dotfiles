  -- Base
import           System.Directory
import           System.Exit                         (exitSuccess)
import           System.IO                           (hPutStrLn)
import           XMonad
import qualified XMonad.StackSet                     as W

    -- Actions
import           XMonad.Actions.CopyWindow           (copyToAll, kill1,
                                                      killAllOtherCopies)
import           XMonad.Actions.CycleWS              (Direction1D (..),
                                                      WSType (..), moveTo,
                                                      nextScreen, prevScreen,
                                                      shiftTo)
import           XMonad.Actions.MouseResize
import           XMonad.Actions.Promote
import           XMonad.Actions.RotSlaves            (rotAllDown, rotSlavesDown)
import           XMonad.Actions.WindowGo             (runOrRaise)
import           XMonad.Actions.WithAll              (killAll, sinkAll)

    -- Data
import qualified Data.Map                            as M
import           XMonad.Prelude                      (Endo, fromJust, isDigit,
                                                      isJust, isSpace, toUpper)
    -- Hooks
import           XMonad.Hooks.DynamicLog             (PP (..), dynamicLogWithPP,
                                                      shorten, wrap,
                                                      xmobarBorder, xmobarColor,
                                                      xmobarPP)
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.InsertPosition         (Focus (..), Position (..),
                                                      insertPosition)
import           XMonad.Hooks.ManageDocks            (ToggleStruts (..),
                                                      avoidStruts,
                                                      docksEventHook,
                                                      manageDocks)
import           XMonad.Hooks.ManageHelpers          (doCenterFloat,
                                                      doFullFloat, isDialog,
                                                      isFullscreen)
import           XMonad.Hooks.RefocusLast            (refocusLastLayoutHook,
                                                      refocusLastWhen,
                                                      refocusingIsActive)
import           XMonad.Hooks.ServerMode
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.WorkspaceHistory
    -- Layouts
import           XMonad.Layout.Gaps
import           XMonad.Layout.GridVariants          (Grid (Grid))
import           XMonad.Layout.MouseResizableTile
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.Spiral
import           XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import qualified XMonad.Layout.Dwindle               as Dwindle
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.LimitWindows          (decreaseLimit,
                                                      increaseLimit,
                                                      limitWindows)
import           XMonad.Layout.MultiToggle           (EOT (EOT),
                                                      Toggle (Toggle), mkToggle,
                                                      single, (??))
import qualified XMonad.Layout.MultiToggle           as MT (Toggle (..))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Renamed
import           XMonad.Layout.Spacing
import qualified XMonad.Layout.ToggleLayouts         as T (ToggleLayout (Toggle),
                                                           toggleLayouts)
import           XMonad.Layout.WindowArranger        (WindowArrangerMsg (..),
                                                      windowArrange)
import           XMonad.Layout.WindowNavigation


   -- Utilities
import           XMonad.Util.Dmenu
import           XMonad.Util.EZConfig                (additionalKeysP)
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                     (runProcessWithInput,
                                                      safeSpawn, spawnPipe)
import           XMonad.Util.SpawnOnce

($.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
($.) = (.) . (.)

if2 :: (a -> b -> Bool) -> (a -> b -> c) -> (a -> b -> c) -> a -> b -> c
if2 p f g x y = if p x y then f x y else g x y

myModMask :: KeyMask
myModMask = mod4Mask        -- modkey to super/windows key

altMask :: KeyMask
altMask = mod1Mask

myTerminal :: String
myTerminal = "st"    -- default terminal

myEmacs :: String
myEmacs = "emacsclient -c -a 'emacs' "  -- Makes emacs keybindings easier to type

myTerm :: String
myTerm = "st"

myTerm2 :: String
myTerm2 = "st"

myvlc :: String
myvlc = "vlc"

myTelegram :: String
myTelegram = "Telegram"

mySkype :: String
mySkype = "skypeforlinux"

myobs :: String
myobs = "obs"

myBrowser :: String
myBrowser = "firefox"

-- myEditor :: String
-- myEditor = myTerminal ++ " -e nvim "  -- nvim as editor
myEditor :: String
myEditor = "emacsclient -c -a 'emacs' "  -- Sets emacs as editor


myBorderWidth :: Dimension
myBorderWidth = 4           -- window border

myNormColor :: String
myNormColor   = "#282c34"   -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#4d78cc"   -- Border color of focused windows
-- myFocusColor  = "#98C379"

myBorderColor :: String
myBorderColor  = "#71abeb"

centreRect :: W.RationalRect
centreRect = W.RationalRect (1 / 3) (1 / 3) (1 / 3) (1 / 3)

videoRect :: W.RationalRect
videoRect = W.RationalRect offset offset size size
  where
    size = 1 / 4
    offset = 1 - size - (size / 8)

isFloating :: Window -> WindowSet -> Bool
isFloating w s = M.member w (W.floating s)

enableFloat :: W.RationalRect -> Window -> (WindowSet -> WindowSet)
enableFloat = flip W.float

enableFloat' :: W.RationalRect -> Window -> X ()
enableFloat' = windows $. enableFloat

disableFloat :: Window -> (WindowSet -> WindowSet)
disableFloat = W.sink

disableFloat' :: Window -> X ()
disableFloat' = windows . disableFloat

toggleFloat :: W.RationalRect -> Window -> X ()
toggleFloat r = windows . if2 isFloating disableFloat (enableFloat r)

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
                 ,NS "skypeforlinux" spawnSkp findSkp manageSkp
                 ,NS "terminal2" spawnTerm2 findTerm2 manageTerm2

                ]
  where
    spawnTerm  = myTerminal ++ " -n spad"
    findTerm   = resource =? "spad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 0.5
                 t = 0.75 -h
                 l = 0.75 -w
    spawnTerm2  = myTerm2 ++ " -n scratchpad"
    findTerm2   = resource =? "scratchpad"
    manageTerm2 = customFloating $ W.RationalRect l t w h
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
    spawnSkp  = mySkype
    findSkp  = resource =? "skypeforlinux"
    manageSkp = customFloating $ W.RationalRect l t w h
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
spirals  = renamed [Replace "spirals"]
           $ windowNavigation
           $ mySpacing' 9
           $ spiral (2/2)
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
                                 ||| spirals
                                 ||| avoidStruts (applyGaps mrt)
                                 ||| grid
                                 ||| threeCol
                                 ||| threeColMid
                                 ||| noBorders monocle
                                 ||| floats

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
myWorkspaceIndex = M.fromList $ zipWith (,) myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndex

------------------------------------------------------------------------
-- MANAGEHOOK
------------------------------------------------------------------------
myManageHook :: Query (Endo WindowSet)
myManageHook = (isDialog --> doF W.swapUp)
               <+> insertPosition Below Newer
               <+> namedScratchpadManageHook myScratchPads
               <+> composeAll
               [ (className =? "firefox" <&&> title =? "Library") --> doCenterFloat
               , (className =? "gcolor3")        --> doCenterFloat
               , (className =? "Gcolor3")        --> doCenterFloat
               , (className =? "Gimp.bin")        --> doCenterFloat
               , (className =? "gimp.bin")        --> doCenterFloat
               , (className =? "mpv")            --> doCenterFloat
               , (className =? "transmission-gtk") --> doCenterFloat
               , (className =? "transmission-gtk") --> doCenterFloat
               , (className =? "file-roller") --> doCenterFloat
               , (className =? "File-roller") --> doCenterFloat
               , (className =? "obs")            --> doCenterFloat
               , (className =? "Lxappearance")   --> doCenterFloat
               , (resource  =? "kdesktop")       --> doIgnore
               , (resource  =? "desktop_window") --> doIgnore
               , isDialog --> doCenterFloat
               ]

myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
        [ ("M-C-r", spawn "xmonad --recompile")  -- Recompiles xmonad
        , ("M-S-r", spawn "xmonad --restart")    -- Restarts xmonad
        , ("M-S-q", io exitSuccess)              -- Quits xmonad

    --  Terminal, Browser, Htop
        , ("M-<Return>", spawn (myTerminal ++ " -e zsh"))
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
     --   , ("M-f", sendMessage (T.Toggle "Full"))
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
        , ("M-a", withFocused $ toggleFloat centreRect)
        , ("M-c", windows copyToAll <> withFocused (enableFloat' videoRect))
        , ("M-S-a", killAllOtherCopies <> withFocused disableFloat')

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
        , ("M1-<Space>", spawn "dmenu_run -sb '#4d78cc' -p 'Run: '")

    -- Scratchpads
        , ("M1-t", namedScratchpadAction myScratchPads "terminal")
        , ("M1-S-t", namedScratchpadAction myScratchPads "terminal2")
        , ("M1-r", namedScratchpadAction myScratchPads "ranger")
        , ("M1-S-v", namedScratchpadAction myScratchPads "vlc")
        , ("M1-C-t", namedScratchpadAction myScratchPads "Telegram")
        , ("M1-o", namedScratchpadAction myScratchPads "obs")
        , ("M1-S-o", namedScratchpadAction myScratchPads "skypeforlinux")

    -- Emacs (CTRL-e followed by a key)
        , ("M1-e e", spawn myEmacs)                 -- start emacs
        , ("M1-e b", spawn (myEmacs ++ ("--eval '(ibuffer)'")))   -- list buffers
        , ("M1-e s", spawn (myEmacs ++ ("--eval '(eshell)'")))    -- eshell
        , ("M1-e d", spawn (myEmacs ++ ("--eval '(dired nil)'"))) -- dired
        , ("M1-e v", spawn (myEmacs ++ ("--eval '(+vterm/here nil)'"))) -- vterm if on Doom Emacs

    --- My Applications
        , ("M1-d", spawn "gnome-disks")
        , ("M1-f", spawn "thunar")
        , ("M1-g", spawn "gthumb")
        , ("M1-m", spawn "gnome-system-monitor")
    --    , ("M1-s", spawn "picon-trans -c -5")
    --    , ("M1-C-s", spawn "picon-trans -c +5")
        , ("M1-v", spawn (myTerm ++ (" -e nvim ")))

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

    xmbar <- spawnPipe "xmobar $HOME/.config/xmobar/xmobar.hs"
    xmonad $ ewmh def
        { manageHook = manageDocks <+> myManageHook
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
              , ppCurrent = xmobarColor "#71abeb" "" . xmobarBorder "Bottom"  myBorderColor 3 -- Current workspace
              , ppVisible = xmobarColor "#5AB1BB" "" . clickable                -- Visible but not current workspace
              , ppHidden = xmobarColor "#e5c07b" "" -- . wrap "-" "-" . clickable  -- Hidden workspaces
              , ppHiddenNoWindows = xmobarColor "#d6d5d5" ""  . clickable       -- Hidden workspaces (no windows)
              , ppWsSep   = "  "                                                -- Workspaces separator
              , ppTitle = xmobarColor "#9ec07c" "" . shorten 90                 -- Title of active window
              , ppSep =  "<fc=#4b5363> <fn=1>|</fn> </fc>"                      -- Separator character
              , ppUrgent = xmobarColor "#e06c75" "" . wrap "!" "!"              -- Urgent workspace
              , ppExtras  = [windowCount]                                       -- # of windows current workspace
              , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]                      -- order of things in xmobar
              , ppLayout  = xmobarColor "#c678dd" "" .
                  ( \t -> case t of
                      "MouseResizableTile" -> "MRT"
                      _                    -> t
                  )
              }
        } `additionalKeysP` myKeys
