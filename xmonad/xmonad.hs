import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Named
import XMonad.Layout.LayoutHints
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM
import XMonad.Layout.ToggleLayouts
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import qualified XMonad.StackSet as W
import Data.Ratio ((%))

main = do
  -- launch xmobar as status bar
  xmproc <- spawnPipe "xmobar ~/.xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
    { -- use urxvt client as terminal
      terminal = "urxvtc"
      -- use Win key
    , modMask = mod4Mask
    , normalBorderColor = "#1F1F1F"
    , focusedBorderColor = "#2688DE"
    , workspaces = myWorkspaces
    , manageHook = manageHook defaultConfig <+> myManageHook
    , layoutHook = myLayoutHook
    , logHook = dynamicLogWithPP $ xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
 		-- hide layout name
		, ppLayout = const ""
                }
    } `additionalKeys`
    [
	  -- suspend key: win + F1
	  -- add USER ALL=NOPASSWD:ALL in /etc/sudoers
	  ((mod4Mask, xK_F1), spawn "sudo pm-suspend"),
	  -- lock screen key: win + shift + L
	  ((mod4Mask .|. shiftMask, xK_l), spawn "gnome-screensaver-command --lock")
    ]

myWorkspaces = ["1:dev", "2:web", "3:im", "4:vm", "5:media", "6:others"]

myManageHook = composeAll
    [ className =? "Gimp" --> doFloat 
     -- move piding to IM workspace 
     , className =? "Pidgin"         --> moveTo "3:im"     
     -- move virtualbox to 4:vm
     , className =? "VirtualBox"         --> moveTo "4:vm"
     -- vboxmanage is used to launch directly a VM
     , className =? "VBoxManage"         --> moveTo "4:vm"
     -- xmonad + trayer on all workspaces 
     , manageDocks
    ]
    where moveTo = doF . W.shift


-- layouts
-- imLayout used for pidgin
imLayout = avoidStruts $ IM (1%7) (Or (And (ClassName "Pidgin") (Role "buddy_list"))
                         (And (ClassName "Skype")  (And (Role "") (Not (Title "Options")))))

-- layouts
-- extrated from http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Thomas_ten_Cate's_xmonad.hs
basicLayout = Tall nmaster delta ratio where
    nmaster = 1
    delta   = 3/100
    ratio   = 1/2
tallLayout = named "tall" $ avoidStruts $ basicLayout
wideLayout = named "wide" $ avoidStruts $ Mirror basicLayout
singleLayout = named "single" $ avoidStruts $ noBorders Full
fullscreenLayout = named "fullscreen" $ noBorders Full
myLayoutHook = im $ normal where
    normal     = tallLayout ||| wideLayout ||| singleLayout ||| fullscreenLayout
    im         = onWorkspace "3:im" imLayout