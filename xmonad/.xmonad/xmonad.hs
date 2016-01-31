import Control.Monad (liftM2)

import Data.Char (isSpace)

import System.Cmd
import System.IO (Handle, hPutStrLn)
import System.Posix.Unistd

import XMonad

import XMonad.Actions.RotSlaves
import XMonad.Actions.CycleWS
import XMonad.Actions.NoBorders

-- import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.IndependentScreens
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders (noBorders)
-- import XMonad.Layout.OnHost
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger

import XMonad.ManageHook

import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare

import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- | Define terminal
myTerminal :: String
myTerminal = "urxvtc"

-- | Define focused window border color
myFocusedBorderColor :: String
myFocusedBorderColor = "#FF0000"
-- myFocusedBorderColor = "#00dddd"
myNormalBorderColor  :: String
myNormalBorderColor  = "#303030"

-- | Define border width
-- myBorderWidth Host :: Dimension
myBorderWidth Desktop = 4
myBorderWidth Netbook = 3

-- | Define workspaces
-- myNetbookWorkspaces Host :: [WorkspaceId]
-- myWorkspaces Desktop = ["1:main","2:web","3:chat","4:mail","5:text","6:audio","7:video","8:browse","9:work"]
-- myWorkspaces Netbook = ["sys","web","msn","sup","txt","aud","vid","mgr","wrk"]
myWorkspaces Desktop = ["1:main","2:web","3:chat","4:mail","5:text","6:audio","7:video","8:browse","9:work","0"]
myWorkspaces Netbook = ["sys","web","msn","sup","txt","aud","vid","mgr","wrk","0"]

-- | The layout that tiles all windows
tiled = spacing 22 $ Tall nmaster delta ratio
  where
    nmaster = 1       -- Default number of windows in the master pane
    ratio   = 0.525   -- Default proportion of screen occupied by master pane
    -- ratio   = toRational (2/(1+sqrt(5)::Double)) -- Golden ratio
    delta   = 3/100   -- Percentage of screen to increment when resizing panes

-- | Layout that maximizes all windows
fullscreen = noBorders $ Full

-- | Layout that floats all windows
floating :: Eq a => (ModifiedLayout WindowArranger SimplestFloat) a
floating = simplestFloat

-- | Defaults set of layouts to be used on workspaces
-- naked   = avoidStruts (fullscreen ||| tiled ||| Mirror tiled) ||| fullscreen
dynamic = avoidStrutsOn [R] $ tiled ||| Mirror tiled ||| noBorders (tabbed shrinkText myTabConfig) ||| fullscreen
mirror  = avoidStrutsOn [R] $ Mirror tiled ||| tiled ||| noBorders (tabbed shrinkText myTabConfig) ||| fullscreen
full    = avoidStrutsOn [R] $ noBorders (tabbed shrinkText myTabConfig) ||| fullscreen ||| tiled ||| Mirror tiled
naked   = fullscreen ||| avoidStrutsOn [R] (noBorders (tabbed shrinkText myTabConfig) ||| fullscreen ||| tiled ||| Mirror tiled)

myTabConfig = defaultTheme
    { inactiveColor       = "#a8907f"
    , inactiveBorderColor = "#b29a89"   -- "#000000"
    , inactiveTextColor   = "#fdf6e3"
    -- , activeColor         = "#f0c000"
    -- , activeColor         = "#4E9258"
    -- , activeBorderColor   = "#d15a5a"   -- "#000000"
    -- , activeTextColor     = "#202020"
    -- , activeTextColor     = "#fdf6e3"
    , activeColor         = "#c75050"
    , activeBorderColor   = "#c75050"   -- "#000000"
    , activeTextColor     = "white"
    , urgentColor         = "#c75050"
    , urgentBorderColor   = "#c75050"   -- "#000000"
    , urgentTextColor     = "#fdf6e3"
    -- , fontName            = "-*-helvetica-medium-r-normal-*-10-*-*-*-*-*-*-*"
    , fontName            = "xft:Arial-9"
    , decoHeight          = 22
    }

-- | Define layouts to be used on workspaces
myLayoutHook =
    -- desktop
    onWorkspace "1:main"   full    $
    onWorkspace "2:web"    dynamic $
    onWorkspace "3:chat"   dynamic $
    onWorkspace "4:mail"   dynamic $
    onWorkspace "5:text"   dynamic $
    onWorkspace "6:audio"  mirror  $
    onWorkspace "7:video"  naked   $
    onWorkspace "8:browse" dynamic $
    onWorkspace "9:work"   naked   $
    -- onWorkspace "0"        dynamic $
    -- netbook
    onWorkspace "sys"      full    $
    onWorkspace "web"      full    $
    onWorkspace "msn"      full    $
    onWorkspace "sup"      full    $
    onWorkspace "txt"      full    $
    onWorkspace "aud"      full    $
    onWorkspace "vid"      naked   $
    onWorkspace "mgr"      dynamic $
    onWorkspace "wrk"      naked   $
    -- onWorkspace "0"        dynamic $
    -- default
                           dynamic

-- | Define the workspace an application has to go to
--   roles and resources are dependent on myterminal and autostart terminals
myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [
    -- desktop
      [ className =? c --> viewShift "1:main"              | c <- myMainClassShifts ]
    , [ resource  =? s --> viewShift "1:main"              | s <- myMainResourceShifts]
    , [ role      =? r --> viewShift "1:main"              | r <- myMainRoleShifts]
    , [ className =? c --> viewShift "2:web"               | c <- myWebClassShifts ]
    , [ resource  =? s --> viewShift "2:web"               | s <- myWebResourceShifts]
    , [ role      =? r --> viewShift "2:web"               | r <- myWebRoleShifts]
    , [ className =? c --> viewShift "3:chat"              | c <- myChatClassShifts ]
    , [ resource  =? s --> viewShift "3:chat"              | s <- myChatResourceShifts]
    , [ role      =? r --> viewShift "3:chat"              | r <- myChatRoleShifts ]
    , [ className =? c --> viewShift "4:mail"              | c <- myMailClassShifts ]
    , [ resource  =? s --> viewShift "4:mail"              | s <- myMailResourceShifts]
    , [ role      =? r --> viewShift "4:mail"              | r <- myMailRoleShifts]
    , [ className =? c --> viewShift "5:text"              | c <- myTextClassShifts ]
    , [ resource  =? s --> viewShift "5:text"              | s <- myTextResourceShifts]
    , [ role      =? r --> viewShift "5:text"              | r <- myTextRoleShifts]
    , [ className =? c --> viewShift "6:audio"             | c <- myAudioClassShifts ]
    , [ resource  =? s --> viewShift "6:audio"             | s <- myAudioResourceShifts]
    , [ role      =? r --> viewShift "6:audio"             | r <- myAudioRoleShifts]
    , [ className =? c --> viewShift "7:video"             | c <- myVideoClassShifts ]
    , [ className =? c --> viewShift "8:browse"            | c <- myBrowseClassShifts ]
    , [ resource  =? s --> viewShift "8:browse"            | s <- myBrowseResourceShifts ]
    , [ role      =? r --> viewShift "8:browse"            | r <- myBrowseRoleShifts ]
    , [ className =? c --> viewShift "9:work"              | c <- myWorkClassShifts ]
    , [ resource  =? s --> viewShift "9:work"              | s <- myWorkResourceShifts ]
    , [ role      =? r --> viewShift "9:work"              | r <- myWorkRoleShifts]
    -- netbook
    , [ className =? c --> viewShift "sys"                 | c <- myMainClassShifts ]
    , [ resource  =? s --> viewShift "sys"                 | s <- myMainResourceShifts]
    , [ role      =? r --> viewShift "sys"                 | r <- myMainRoleShifts]
    , [ className =? c --> viewShift "web"                 | c <- myWebClassShifts ]
    , [ resource  =? s --> viewShift "web"                 | s <- myWebResourceShifts]
    , [ role      =? r --> viewShift "web"                 | r <- myWebRoleShifts]
    , [ className =? c --> viewShift "msn"                 | c <- myChatClassShifts ]
    , [ resource  =? s --> viewShift "msn"                 | s <- myChatResourceShifts]
    , [ role      =? r --> viewShift "msn"                 | r <- myChatRoleShifts ]
    , [ className =? c --> viewShift "sup"                 | c <- myMailClassShifts ]
    , [ resource  =? s --> viewShift "sup"                 | s <- myMailResourceShifts]
    , [ role      =? r --> viewShift "sup"                 | r <- myMailRoleShifts]
    , [ className =? c --> viewShift "txt"                 | c <- myTextClassShifts ]
    , [ resource  =? s --> viewShift "txt"                 | s <- myTextResourceShifts]
    , [ role      =? r --> viewShift "txt"                 | r <- myTextRoleShifts]
    , [ className =? c --> viewShift "aud"                 | c <- myAudioClassShifts ]
    , [ resource  =? s --> viewShift "aud"                 | s <- myAudioResourceShifts]
    , [ role      =? r --> viewShift "aud"                 | r <- myAudioRoleShifts]
    , [ className =? c --> viewShift "vid"                 | c <- myVideoClassShifts ]
    , [ className =? c --> viewShift "mgr"                 | c <- myBrowseClassShifts ]
    , [ resource  =? s --> viewShift "mgr"                 | s <- myBrowseResourceShifts ]
    , [ role      =? r --> viewShift "mgr"                 | r <- myBrowseRoleShifts ]
    , [ className =? c --> viewShift "wrk"                 | c <- myWorkClassShifts ]
    , [ resource  =? s --> viewShift "wrk"                 | s <- myWorkResourceShifts ]
    , [ role      =? r --> viewShift "wrk"                 | r <- myWorkRoleShifts]
    -- non-workspace rules
    , [ className =? c --> doCenterFloat                   | c <- myClassCenterFloat ]
    , [ resource  =? s --> doCenterFloat                   | s <- myResourceCenterFloat ]
    , [ role      =? r --> doCenterFloat                   | r <- myRoleCenterFloat ]
    , [ (className =? c <&&> role =? "") --> doCenterFloat | c <- myClassRoleCenterFloat ]
    , [ className =? c --> doFloat                         | c <- myClassFloat ]
    , [ className =? c --> unfloat                         | c <- myClassUnfloat ]
    , [ className =? c --> doIgnore                        | c <- myClassIgnore ]
    ]
  where
    -- xp (pointer) to determine window class and role names
    viewShift              = doF . liftM2 (.) W.greedyView W.shift
    unfloat                = ask >>= doF . W.sink
    role                   = stringProperty "WM_WINDOW_ROLE"
    -- myMainClassShifts      = ["Conky"]
    myMainClassShifts      = []
    myMainResourceShifts   = []
    myMainRoleShifts       = myMainResourceShifts
    myWebClassShifts       = ["dwb","Google-chrome","Iceweasel","luakit","Minitube"]
    myWebResourceShifts    = ["RSS","Minitube"]
    myWebRoleShifts        = myWebResourceShifts
    myChatClassShifts      = ["Pidgin"]
    myChatResourceShifts   = ["Twitter"]
    myChatRoleShifts       = myChatResourceShifts
    myMailClassShifts      = ["Geary","Thunderbird"]
    myMailResourceShifts   = ["Mail","TODO","Usenet"]
    myMailRoleShifts       = myMailResourceShifts
    myTextClassShifts      = ["Hbro-linux-x86_64","Surf"]
    myTextResourceShifts   = ["Text","Vimwiki"]
    myTextRoleShifts       = myTextResourceShifts
    myAudioClassShifts     = ["Last.fm","mplayer2","Pavucontrol","Rrip_gui"]
    myAudioResourceShifts  = ["Mixer","Music","Radio"]
    myAudioRoleShifts      = myAudioResourceShifts
    -- myVideoClassShifts     = ["Smplayer","Umplayer","VirtualBox","xbmc.bin"]
    -- for now, until a conflict is found, aur xbmc-xvba has no class name!
    myVideoClassShifts     = ["Smplayer","Umplayer","Vlc","","xbmc.bin"]
    -- myBrowseClassShifts    = ["Nautilus","Rox","Thunar","Tixati","Transmission-gtk"]
    myBrowseClassShifts    = ["Nautilus","Thunar","Tixati","Transmission-gtk"]
    myBrowseResourceShifts = ["NZB"]
    myBrowseRoleShifts     = myBrowseResourceShifts
    myWorkClassShifts      = ["Digikam","Gimp","VirtualBox"]
    myWorkResourceShifts   = ["IMAP","syslog"]
    myWorkRoleShifts       = myWorkResourceShifts
    myClassCenterFloat     = ["Launchy","File-roller","Savebox"]
    -- myResourceCenterFloat  = ["Dialog"]
    myResourceCenterFloat  = []
    myRoleCenterFloat      = ["preferences"]
    myClassRoleCenterFloat = ["ROX-Filer"]
    myClassFloat           = ["Keepassx","Wicd-client.py","xpad"]
    -- myClassUnfloat         = ["Keepassx"]
    myClassUnfloat         = []
    myClassIgnore          = ["Xfce4-notifyd"]

-- | Define scratchpads
-- myScratchPadsDesktop :: NamedScratchpads
myScratchPads host =
    [
      NS "terminal" spawnTerm findTerm (manageTerm host)
    ]
  where
    -- spawnTerm  = "xterm -bg '#111111' -name scratchpad"
    -- spawnTerm  = "terminator --classname=scratchpad --profile=scratchpad"
    -- findTerm   = resource  =? "scratchpad"
    -- spawnTerm  = "xfce4-terminal --role=scratchpad"
    spawnTerm  = "urxvtc -title 'scratchpad' -name 'scratchpad' -e byobu"
    findTerm   = name =? "scratchpad"
      where
        name   = stringProperty "WM_CLASSNAME"
    manageTerm Desktop = customFloating $ W.RationalRect l t w h
      where
        h = 0.50          -- height
        w = 0.50          -- width
        t = (1 - h)/2     -- distance from top edge, * 1.0 would be flush with bottom
        l = (1 - w)/2     -- distance from left edge, centered left/right
    manageTerm Netbook = customFloating $ W.RationalRect l t w h
      where
        h = 0.85          -- height
        w = 0.90          -- width
        t = (1 - h)/2     -- distance from top edge, * 1.0 would be flush with bottom
        l = (1 - w)/2     -- distance from left edge, centered left/right

-- | Define new key combinations to be added
backtick :: KeySym
backtick  = 0x00000060

keysToAdd host x =
    [
    -- menu actions
      ((mod4Mask,                                xK_space),      spawn "dmenu_run")           -- launch xft dmenu
    -- , ((mod4Mask .|. shiftMask,                  xK_space),      spawn "dinit.d")              -- launch dmenu restart init.d process
    , ((mod4Mask .|. shiftMask,                  xK_space),      spawn "dsystemd")            -- launch dmenu restart systemd process
    , ((mod4Mask,                                xK_equal),      spawn "dcalculator")         -- launch dmenu calculator
    , ((mod4Mask .|. shiftMask,                  xK_equal),      spawn "dvolume +")           -- increase volume
    , ((mod4Mask,                                xK_minus),      spawn "dvolume -")           -- decrease volume
    , ((mod4Mask,                                xK_0),          spawn "dvolume 0")           -- toggle volume on/off
    , ((mod4Mask,                                xK_Return),     spawn "dhelp")               -- launch dmenu man pages
    , ((mod4Mask,                                xK_slash),      spawn "dfiles")              -- launch dmenu open file
    , ((mod4Mask .|. shiftMask,                  xK_slash),      spawn "dfolders")            -- launch dmenu open directory folder
    , ((mod4Mask,                                xK_F1),         spawn "dtest")               -- launch dmenu test menu!
    , ((mod4Mask,                                xK_a),          spawn "daddresses")          -- launch dmenu luakit bookmarks
    , ((mod4Mask,                                xK_b),          spawn "dbookmarks")          -- launch dmenu luakit bookmarks
    , ((mod4Mask .|. shiftMask,                  xK_b),          spawn "gbookmarks")          -- launch dmenu google-chrome bookmarks
    , ((mod4Mask,                                xK_c),          spawn "dconfigs")            -- launch dmenu edit config files
    , ((mod4Mask,                                xK_d),          spawn "ddictionary")         -- launch dmenu dictionary
    , ((mod4Mask,                                xK_e),          spawn "dedit")               -- launch dmenu edit!
    , ((mod4Mask,                                xK_f),          spawn "dfunctions")          -- launch dmenu edit fish functions
    , ((mod4Mask,                                xK_h),          spawn "dhistory")            -- launch dmenu luakit history
    , ((mod4Mask .|. shiftMask,                  xK_h),          spawn "dchrome")             -- launch dmenu google-chrome history
    , ((mod4Mask,                                xK_m),          spawn "dmusic")              -- launch dmenu music
    , ((mod4Mask,                                xK_p),          spawn "[ \"$(pidof xpad)\" = \"\" ] && xpad || xpad --quit") -- toggle post-it notes
    , ((mod4Mask,                                xK_s),          spawn "dscripts")            -- launch dmenu edit shell scripts
    , ((mod4Mask,                                xK_x),          spawn "dhalt")               -- launch dmenu quit session
    , ((shiftMask,                               xK_F1),         spawn "fish -c keymap")      -- toggle keyboard layout colemak/qwerty
    -- applications
    -- , ((modMask x,                               xK_o),          namedScratchpadAction myScratchPads "terminal") -- scratchpad
    , ((modMask x,                               xK_o),          namedScratchpadAction (myScratchPads host) "terminal") -- scratchpad
    , ((modMask x,                               xK_backslash),  spawn "dconky")              -- toggle conky panel on/off
    , ((modMask x .|. shiftMask,                 xK_backslash),  spawn "raise Conky")         -- raise conky
    , ((modMask x,                               xK_Return),     spawn myTerminal)            -- open terminal
    , ((modMask x,                               backtick),      spawn "rox")                 -- launch rox
    , ((modMask x .|. shiftMask,                 backtick),      spawn "fish -c roxy")        -- launch rox windows
    -- tile/window actions
    , ((modMask x,                               xK_c),          kill)                        -- close window
    , ((modMask x .|. shiftmask,                 xK_Return),     windows W.swapMaster)        -- swap focused and master windows
    , ((modMask x,                               xK_Down),       rotSlavesDown)               -- rotate non-master list down
    , ((modMask x,                               xK_Up),         rotSlavesUp)                 -- rotate non-master list up
    , ((modMask x .|. shiftMask,                 xK_Down),       rotAllDown)                  -- rotate all list down
    , ((modMask x .|. shiftMask,                 xK_Up),         rotAllUp)                    -- rotate all list up
    , ((modMask x .|. controlMask,               xK_Down),       nextWS)                      -- to next workspace
    , ((modMask x .|. controlMask,               xK_Up),         prevWS)                      -- to previous workspace
    , ((modMask x .|. controlMask .|. shiftMask, xK_Down),       shiftToNext)                 -- shift window to next workspace
    , ((modMask x .|. controlMask .|. shiftMask, xK_Up),         shiftToPrev)                 -- shift window to previous workspace
    -- screen actions
    , ((modMask x,                               xK_Right),      nextScreen)                  -- shift to next screen
    , ((modMask x,                               xK_Left),       prevScreen)                  -- shift to previous screen
    , ((modMask x .|. shiftMask,                 xK_Right),      swapNextScreen)              -- swap screen with next screen
    , ((modMask x .|. shiftMask,                 xK_Left),       swapPrevScreen)              -- swap screen with previous screen
    , ((modMask x .|. controlMask,               xK_Right),      shiftNextScreen)             -- shift window to next screen
    , ((modMask x .|. controlMask,               xK_Left),       shiftPrevScreen)             -- shift window to previous screen
    -- workspace actions
    , ((modMask x,                               xK_z),          toggleWS)                    -- shift to last viewed workspace
    , ((modMask x,                               xK_u),          focusUrgent)                 -- focus most recent urgent window
    -- decorations
    , ((modMask x,                               xK_d),          withFocused toggleBorder)    -- remove borders
    , ((modMask x,                               xK_b),          sendMessage ToggleStruts)    -- toggle status bar visibility
    -- assign workspace "0" to number key 0
    , ((modMask x,                               xK_0),          windows $ W.greedyView "0")
    , ((modMask x .|. shiftMask,                 xK_0),          withFocused (\w -> do { windows $ W.shift "0" }))
    ]

-- | Define existing key combinations to be removed
keysToRemove x =
    [
      (modMask x .|. shiftMask, xK_c)       -- was close window binding
    , (modMask x,               xK_p)       -- was launch dmenu binding
    , (modMask x,               xK_Return)  -- was swap focused and master windows
    , (modMask x .|. shiftMask, xK_Return)  -- was open terminal
    ]

-- | Delete the key combinations to be removed from the original keys
newKeys x = foldr M.delete (keys defaultConfig x) (keysToRemove x)

-- | Merge new key combinations with existing keys
myKeys host x = M.union (newKeys x) (M.fromList (keysToAdd host x))

-- | Forward the window information to the left dzen bar and format it
-- myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }
-- myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

-- | Bar style 24px high and colors
-- myXmonadBar host = "dzen2 -p -xs 1 -ta 'l'" ++ xmonadBarWidth host ++ myDzenStyle
--
-- xmonadBarWidth Desktop = " -w '900'"
-- xmonadBarWidth Netbook = " -w '585'"

-- myConkyBar host  = "conky -c /home/shum/.xmonad/conkyrc.dzen | dzen2 -p -xs 1 -ta 'r'" ++ conkyBarWidth host ++ myDzenStyle
--
-- conkyBarWidth Desktop = " -x '900' -w '1575'"
-- conkyBarWidth Netbook = " -x '585' -w '374'"

-- myDzenStyle = " -h '24' -y '0' -fg '#dddd00' -bg '" ++ myNormalBorderColor ++ "'"
-- myDzenStyle = " -fn 'Arial Black-8' -h '24' -y '0' -fg '#dddd00' -bg '" ++ myNormalBorderColor ++ "'"
-- myDzenStyle = " -fn 'Slashed Zero Arial-9' -h '22' -y '0' -fg '#999999' -bg '" ++ myNormalBorderColor ++ "'"
-- myDzenStyle = " -fn '-*-liberation mono-bold-r-*-*-12-*-*-*-*-*-*-*' -h '22' -y '0' -fg '#999999' -bg '" ++ myNormalBorderColor ++ "'"

-- | Very plain formatting, non-empty workspaces are highlighted,
--   urgent workspaces (e.g. active IM window) are highlighted in red
-- myDzenPP  = dzenPP
--     { ppCurrent         = dzenColor "#dddd00" "" . wrap " " " "
--     , ppHidden          = dzenColor "#00dddd" "" . wrap " " " "
--     , ppHiddenNoWindows = dzenColor "#666666" "" . wrap " " " "
--     , ppUrgent          = dzenColor "#ff0000" "" . wrap " " " "
--     , ppSep             = "  "
--     , ppLayout          = \y -> ""
--     , ppTitle           = dzenColor "#dddd00" "" . wrap " " " "
--     , ppVisible         = dzenColor "#dd00dd" "" . wrap " [" "] "
--     -- , ppSort            = getSortByXineramaRule
--     }

-- | Main function that launches xmonad
main :: IO ()
main =  do
    -- spawn "conky"
    host <- getHost
    -- status <- spawnPipe (myXmonadBar host)
    -- spawn $ (myConkyBar host)
    -- xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    -- xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "#ffde1e", "-fg", "black", "-xs", "1", "-fn", "-*-liberation mono-*-r-*-*-12-*-*-*-*-*-*-*", "-h", "22" ] } $ defaultConfig
    xmonad $ defaultConfig
        {
          terminal              = myTerminal
        , focusedBorderColor    = myFocusedBorderColor
        , normalBorderColor     = myNormalBorderColor
        , borderWidth           = myBorderWidth host
        , workspaces            = myWorkspaces host
        , keys                  = myKeys host
        , layoutHook            = myLayoutHook
        -- , logHook               = myLogHook status
        , manageHook            = manageDocks <+> myManageHook <+> namedScratchpadManageHook (myScratchPads host)
        -- delay spawning conky so as not to attach it to desktop i.e. enable toggling and show on startup!
        , startupHook           = ewmhDesktopsStartup >> setWMName "LG3D" >> myStartup host
        , focusFollowsMouse     = True
        }

-- | Conky startup delay
myStartup Desktop = spawn "$HOME/bin/autostart && sleep 4s && dconky"
myStartup Netbook = spawn "$HOME/bin/autostart && sleep 10s && dconky"

-- | For use in cross host configutions
data Host = Desktop | Netbook deriving Eq

-- | Determine the host
getHost = do
    host <- getSystemID
    case nodeName host of
         "luna"  -> return Desktop
         "monad" -> return Netbook
         _       -> return Desktop
