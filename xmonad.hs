import XMonad
import XMonad.Config.Kde
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Layout.Spacing
import XMonad.Util.Run -- for spawnPipe and hPutStrLn
import XMonad.Hooks.ManageDocks -- avoid xmobar
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Actions.WindowBringer
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)
import XMonad.Actions.CycleWS
import XMonad.Actions.Warp (warpToScreen, warpToWindow)

myNormalBorderColor = "#000000"
myFocusedBorderColor = "#ff0000"
myStatusBar = "/home/aden/bin/xmobar -x0 /home/aden/.xmonad/xmobar.hs"
myWorkspaces = ["mail", "browser"]

modm = mod4Mask
 
main = do
  xmproc <- spawnPipe myStatusBar
  xmonad $ kdeConfig
    { modMask = modm -- use the Windows button as mod
    , manageHook = manageDocks <+> myManageHook
    , layoutHook = avoidStruts $ smartSpacing 2 $ layoutHook kdeConfig
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , borderWidth = 3
    , logHook = myLogHook xmproc
    , workspaces = myWorkspaces
    } `additionalKeys` myKeys

-- to find default key bindings, look in: https://hackage.haskell.org/package/xmonad-0.13/docs/src/XMonad-Config.html
myKeys = [
    ((modm, xK_g                    ), gotoMenu)
  , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
  , ((modm .|. shiftMask, xK_t      ), renameWorkspace def)
  , ((modm .|. shiftMask, xK_a      ), addWorkspacePrompt def)
  , ((modm .|. shiftMask, xK_s      ), selectWorkspace def)
  --, ((modm, xK_m                    ), withWorkspace def (windows . W.shift))
  --, ((modm .|. shiftMask, xK_m      ), withWorkspace def (windows . copy))
  , ((modm .|. shiftMask, xK_m      ), withWorkspace def (windows . W.shift))
  , ((modm, xK_minus), prevWS  ) -- mod-- %! Switch to the previous workspace
  , ((modm, xK_equal), nextWS  ) -- mod-= %! Switch to the next workspace
  , ((modm, xK_j ), windows W.focusDown >> warpToWindow' ) -- %! Move focus to the next window
  , ((modm, xK_k ), windows W.focusUp >> warpToWindow' ) -- %! Move focus to the previous window
  , ((modm, xK_m ), windows W.focusMaster >> warpToWindow' ) -- %! Move focus to the master window
  ]
  ++
  zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
  ++
  zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
  ++
  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  --[((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
  [((m .|. modm, key), screenSwitchAndWarp sc f)
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

warpToWindow' = warpToWindow (1/2) (1/2)

screenSwitchAndWarp sc f = do
  maybeWorkspaceId <- screenWorkspace sc
  warpToScreen sc (1/2) (1/2)
  whenJust maybeWorkspaceId (windows .f)


myManageHook = composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- myFloats]
    ]
  where myFloats      = ["MPlayer", "Gimp", "Plasma", "plasmashell"]

myLogHook xmproc = dynamicLogWithPP xmobarPP
  { ppOutput = hPutStrLn xmproc
  , ppTitle  = xmobarColor "green" "" . shorten 50
}


