import XMonad
import XMonad.Config.Kde
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Layout.Spacing
import XMonad.Util.Run -- for spawnPipe and hPutStrLn
import XMonad.Hooks.ManageDocks -- avoid xmobar
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig
import XMonad.Actions.GridSelect
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow(copy)
import XMonad.Actions.CycleWS
import XMonad.Actions.Warp (warpToScreen, warpToWindow)
import System.Process
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import XMonad.Layout.Tabbed
import XMonad.Layout.Renamed
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.SubLayouts
import XMonad.Layout.BoringWindows
import XMonad.Layout.WindowNavigation
import Data.Map as M

myNormalBorderColor = "#000000"
myFocusedBorderColor = "#ff0000"
myStatusBar :: Int -> String
myStatusBar screenNum = "/home/aden/bin/xmobar -x" ++ (show screenNum) ++ " /home/aden/.xmonad/xmobar.hs"
myWorkspaces = ["mail", "browser"]

modm = mod4Mask

gsconfig1 = defaultGSConfig { gs_cellheight = 60, gs_cellwidth = 200, gs_font = "xft:Sans-6", gs_navigate = substringSearch defaultNavigation }

main = do
  maxScreenIndex <- ((-) 1) <$> fromMaybe 1 <$> (readMaybe <$> readCreateProcess (shell "xrandr | grep connected | grep -v disconnected | wc -l") "")
  xmprocList <- mapM (spawnPipe . myStatusBar) [0..maxScreenIndex]
  xmonad $ kdeConfig
    { modMask = modm -- use the Windows button as mod
    , manageHook = manageDocks <+> myManageHook
    , layoutHook = avoidStruts $ smartSpacing 2 $ myLayout
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , borderWidth = 3
    , logHook = myLogHook xmprocList
    , workspaces = myWorkspaces
    , XMonad.keys = myKeys
    }

-- to find default key bindings, look in: https://hackage.haskell.org/package/xmonad-0.13/docs/src/XMonad-Config.html
myKeys = \conf -> M.fromList $ [
    ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- Launch terminal
  -- Window Focus
  , ((modm, xK_u     ), focusUp >> warpToWindow') -- Move focus up in window list
  , ((modm, xK_j     ), focusDown >> warpToWindow') -- Move focus down in window list
  , ((modm, xK_m     ), windows W.focusMaster >> warpToWindow' ) -- Move focus to the master window
  , ((modm .|. controlMask, xK_u), onGroup W.focusUp') -- shift tab group focus up its list
  , ((modm .|. controlMask, xK_j), onGroup W.focusDown') -- shift tab group focus down its list
  , ((modm, xK_question), goToSelected gsconfig1) -- find a window and focus on it
  , ((modm .|. shiftMask, xK_slash ), goToSelected gsconfig1) -- find a window and focus on it
  -- Window adjustment
  , ((modm .|. shiftMask, xK_u     ), windows W.swapUp    ) -- move the window up the list
  , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  ) -- move the window down the list
  , ((modm .|. shiftMask, xK_b     ), kill) -- close the focused window
  , ((modm .|. shiftMask, xK_h     ), sendMessage $ Toggle FULL) -- toggle mirroring of active layout
  , ((modm .|. shiftMask, xK_m     ), windows W.swapMaster) -- Swap the focused window and the master window
  -- Layout adjustment
  , ((modm, xK_equal ), sendMessage (IncMasterN 1)) -- Increment the number of windows in the master area
  , ((modm, xK_minus), sendMessage (IncMasterN (-1))) -- Deincrement the number of windows in the master area
  , ((modm, xK_bracketright), sendMessage Expand) -- Expand the master area
  , ((modm, xK_bracketleft), sendMessage Shrink) -- Shrink the master area
  , ((modm, xK_backslash ), sendMessage $ Toggle MIRROR) -- toggle mirroring of active layout
  -- Group adjustment
  , ((modm .|. shiftMask .|. controlMask, xK_h), withFocused (sendMessage . MergeAll)) -- merge all windows
  , ((modm .|. shiftMask .|. controlMask, xK_k), withFocused (sendMessage . UnMerge))
  , ((modm .|. shiftMask .|. controlMask, xK_u), sendMessage $ pullGroup U)
  , ((modm .|. shiftMask .|. controlMask, xK_j), sendMessage $ pullGroup D)
  -- Workspace modification
  , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
  , ((modm, xK_a      ), selectWorkspace def)
  , ((modm, xK_s      ), withWorkspace def (windows . W.shift))
  , ((modm, xK_d      ), renameWorkspace def)
  , ((modm, xK_f), prevWS  ) -- mod-- %! Switch to the previous workspace
  , ((modm, xK_g), nextWS  ) -- mod-= %! Switch to the next workspace

  -- , ((modm, xK_j ), windows W.focusDown >> warpToWindow' ) -- %! Move focus to the next window
  -- , ((modm, xK_k ), windows W.focusUp >> warpToWindow' ) -- %! Move focus to the previous window
  -- , ((modm, xK_m ), windows W.focusMaster >> warpToWindow' ) -- %! Move focus to the master window
  -- bindings for SubLayouts navigation
  -- moving windows
  -- , ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
  -- , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)
  -- , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U)
  -- , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D)
  -- merging windows
  -- , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
  -- , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
  -- adjusting focus
  ]
  ++
  -- zip (zip (repeat (modm)) [xK_1..xK_9]) (map (withNthWorkspace W.greedyView) [0..])
  -- ++
  -- zip (zip (repeat (modm .|. shiftMask)) [xK_1..xK_9]) (map (withNthWorkspace W.shift) [0..])
  -- ++
  -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
  --[((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
  [((m .|. modm, key), screenSwitchAndWarp sc f)
      | (key, sc) <- zip [xK_q, xK_w, xK_e] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

warpToWindow' = warpToWindow (1/2) (1/2)

screenSwitchAndWarp sc f = do
  maybeWorkspaceId <- screenWorkspace sc
  warpToScreen sc (1/2) (1/2)
  whenJust maybeWorkspaceId (\wsid -> (windows . f $ wsid) >> warpToWindow')

myManageHook = composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- myFloats]
    ]
  where myFloats      = ["MPlayer", "Gimp", "Plasma", "plasmashell"]

myLogHook xmprocList = dynamicLogWithPP xmobarPP
  { ppOutput = \string -> mapM_ (\handle -> hPutStrLn handle string) xmprocList
  , ppTitle  = xmobarColor "green" "" . shorten 50
}

-- myLayout = addTabs shrinkText def
--          $ subLayout [0,1,2] (Full ||| Tall 1 0.2 0.5 ||| Full)
--          $ Tall 1 0.2 0.5 ||| Full

myLayout = mkToggle (FULL ?? EOT) . mkToggle (single MIRROR) $ windowNavigation $ subTabbed $ boringWindows $
                       Tall 1 0.03 0.5

-- myLayout = mkToggle (FULL ?? EOT) . mkToggle (single MIRROR) $ (tiled ||| myTabbed)
--   where
--      -- default tiling algorithm partitions the screen into two panes
--      tiled   = renamed [Replace "Tiled"] $ Tall nmaster delta ratio
--      -- The default number of windows in the master pane
--      nmaster = 1
--      -- Default proportion of screen occupied by master pane
--      ratio   = 1/2
--      -- Percent of screen to increment by when resizing panes
--      delta   = 3/100
--      -- tabbed layout
--      myTabbed = renamed [Replace "NiceTabbed"] $ layoutN 1 (relBox 0 0 0.5 1) (Just $ relBox 0 0 1 1) (Tall 0 0.01 0.5) (layoutAll (relBox 0.5 0 1 1) (tabbed shrinkText def))
