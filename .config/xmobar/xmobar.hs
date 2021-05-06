-- http://projects.haskell.org/xmobar/
-- install xmobar with these flags: --flags="with_alsa" --flags="with_mpd" --flags="with_xft"  OR --flags="all_extensions"
-- you can find weather location codes here: http://weather.noaa.gov/index.html

Config { font    = "xft:UbuntuMono Nerd Font:weight=bold:pixelsize=20:antialias=true:hinting=true"
       , additionalFonts = [ "xft:UbuntuMono Nerd Font:pixelsize=20:antialias=true:hinting=true"
                           , "xft:NotoEmoji Nerd Font:pixelsize=20"
                           ]
       , bgColor = "#1E222A"
       , fgColor = "#ff6c6b"
       , position = Static { xpos = 0 , ypos = 0, width = 3840, height = 32 }
       , lowerOnStart = True
       , hideOnStart = False
       , allDesktops = True
       , persistent = True
       , commands = [
                      Run Date "<fn=1> </fn> %b %d %Y - %H:%M " "date" 50
                    , Run Network "enp34s0" ["-t","<fn=1> </fn> <rx> kb  <fn=1> </fn> <tx> kb"] 20
                    , Run Cpu ["-t", "<fn=1> ﬙ </fn> <total>%"] 20
                    , Run Com "~/.local/bin/memory" ["--listen"] "" 40
                    , Run Com "~/.local/bin/pavolume" ["--listen"] "" 10
                    , Run MultiCoreTemp ["-t", "﨎 <avg>°C", "-hwmonitor-path", "/sys/module/k10temp/drivers/pci:k10temp/0000:00:18.3/hwmon/hwmon2"] 50
                    , Run UnsafeStdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template ="<fc=#bc7ad9>   </fc> %UnsafeStdinReader% }{<fc=#e06c75> %enp34s0% </fc><fc=#4b5363>|</fc><fc=#6bb2c0> %multicoretemp% </fc><fc=#4b5363>|</fc><fc=#bc7ad9>   %memory%</fc><fc=#4b5363>|</fc><fc=#ebcb8d> %cpu% </fc><fc=#4b5363>|</fc><fc=#71abeb> %pavolume% </fc><fc=#4b5363>|</fc><fc=#9ec07c> %date% </fc>"
       }
