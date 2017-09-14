import XMonad
import XMonad.Config.Kde
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Layout.Spacing
import XMonad.Util.Run -- for spawnPipe and hPutStrLn
import XMonad.Hooks.ManageDocks -- avoid xmobar
import XMonad.Hooks.DynamicLog
--import XMonad.Actions.GridSelect
import XMonad.Util.EZConfig
import XMonad.Actions.WindowBringer
--import XMonad.StackSet as W

myNormalBorderColor = "#000000"
myFocusedBorderColor = "#ff0000"
myStatusBar = "/home/aden/bin/xmobar -x0 /home/aden/.xmonad/xmobar.hs"
myWorkspaces = ["mail", "browser", "3", "4", "5", "6", "7", "8", "9"]

myModMask = mod4Mask
 
main = do
  xmproc <- spawnPipe myStatusBar
  xmonad $ kdeConfig
    { modMask = myModMask -- use the Windows button as mod
    -- , manageHook = manageDocks <+> manageHook kdeConfig <+> myManageHook
    , manageHook = manageDocks <+> myManageHook
    , layoutHook = avoidStruts $ smartSpacing 2 $ layoutHook kdeConfig
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , borderWidth = 3
    , logHook = myLogHook xmproc
    , workspaces = myWorkspaces
    } `additionalKeys` myKeys

--myWindowSwitch = case gridselectWindow defaultGSConfig of
--    Just window -> W.focusWindow window
--    Nothing     -> return ()

myKeys = [
  -- ((myModMask, xK_g), goToSelected defaultGSConfig)
  -- ((myModMask, xK_g), gridselectWindow defaultGSConfig) )
    ((myModMask, xK_g     ), gotoMenu)
  -- , ((myModMask .|. shiftMask, xK_b     ), bringMenu)
  ]

myManageHook = composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- myFloats]
    , [ title       =? t --> doFloat           | t <- myOtherFloats]
    , [ className   =? c --> doF (W.shift "2") | c <- webApps]
    ]
  where myFloats      = ["MPlayer", "Gimp", "Plasma", "plasmashell"]
        myOtherFloats = ["alsamixer"]
        webApps       = ["Firefox-bin", "Opera"] -- open on desktop 2

myLogHook xmproc = dynamicLogWithPP xmobarPP
  { ppOutput = hPutStrLn xmproc
  , ppTitle  = xmobarColor "green" "" . shorten 50
}


