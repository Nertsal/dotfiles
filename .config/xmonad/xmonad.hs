import           XMonad
import qualified XMonad.StackSet as StackSet
import qualified XMonad.Actions.GroupNavigation as Navigation
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.StatusBar
import           XMonad.Layout.Magnifier
import           XMonad.Layout.ThreeColumns
import           XMonad.Util.EZConfig
import           XMonad.Util.Loggers
import           XMonad.Util.Ungrab

main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp "~/.cabal/bin/xmobar" (pure myXmobarPP)) toggleStrutsKey
    $ myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig {modMask = m} = (m, xK_b)

myConfig =
  def
    { modMask = mod4Mask -- Rebind Mod to the Super key
    , workspaces = myWorkspaces
    , layoutHook = myLayout -- Custom layout
    , startupHook = myStartup -- Actions on start
    , borderWidth = 1 -- Window border
    , logHook = Navigation.historyHook -- Keep history of windows
    }
    `additionalKeysP` myKeys 

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

isVisible w ws = any ((w ==) . StackSet.tag . StackSet.workspace) (StackSet.visible ws)

lazyView w ws = if isVisible w ws then ws else StackSet.view w ws

myKeys = [ ("M-p", spawn "dmenu_path | dmenu | sh") -- dmenu
         , ("M-S-z", spawn "xset s activate") -- Screen lock
         , ("M-S-s", unGrab *> spawn "bash ~/Scripts/screenshot.sh") -- Select screenshot area
         , ("M-s", spawn "shotgun -s - | xclip -t 'image/png' -selection clipboard") -- Take the whole screenshot
         , ("M-S-<Return>", spawn "alacritty") -- Terminal
         , ("M-x", Navigation.nextMatch Navigation.History (return True)) -- Toggle between the most recent window
         -- , ("M-C-S-<Return>", spawn "neovide --multigrid") -- Neovide
         --, ("M-[", spawn "telegram-desktop")
         --, ("M-]", spawn "firefox")
         ] ++
         [ (otherModMasks ++ "M-" ++ [key], action tag)
           | (tag, key) <- zip myWorkspaces "123456789"
           , (otherModMasks, action) <- [ ("", windows . lazyView) -- `greedyView` by default
                                        , ("C-", windows . StackSet.greedyView)
                                        , ("S-", windows . StackSet.shift)
                                        ]
         ]

myLayout = tiled ||| Mirror tiled ||| threeCol ||| Full
  where
    tiled = Tall nmaster delta ratio
    threeCol = magnifiercz' 1.2 $ ThreeColMid nmaster delta ratio
    nmaster = 1 -- Number of windows in the master pane
    ratio = 1 / 2 -- Proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

myStartup :: X ()
myStartup = do
  -- Update monitor info, Setup monitor layout, Set background
  spawn "xrandr --auto; sh ~/.screenlayout/normal-1-1.sh; feh --bg-fill --no-fehbg ~/Pictures/Wallpapers/everyone.png"

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2,
      ppHidden = white . wrap " " "",
      ppHiddenNoWindows = lowWhite . wrap " " "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
