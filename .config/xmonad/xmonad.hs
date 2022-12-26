import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
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
    , layoutHook = myLayout -- Custom layout
    , startupHook = myStartup -- Actions on start
    }
    `additionalKeysP` [ ("M-p", spawn "dmenu_path | dmenu | sh")
                      , ("M-S-z", spawn "xset s activate")
                      , ("M-S-s", unGrab *> spawn "bash ~/Scripts/screenshot.sh")
                      , ("M-s", spawn "shotgun -s - | xclip -t 'image/png' -selection clipboard")
                      , ("M-S-<Return>", spawn "alacritty")
                      , ("M-C-S-<Return>", spawn "neovide --multigrid")
                    --, ("M-[", spawn "telegram-desktop")
                    --, ("M-]", spawn "firefox")
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
  spawn "xrandr --auto && sh ~/.screenlayout/normal-1-1.sh" -- Update monitor info, setup monitor layout
  spawn "feh --bg-fill --no-fehbg ~/Pictures/Wallpapers/everyone.png" -- Set background

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
