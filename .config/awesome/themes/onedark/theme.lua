local gears = require("gears")
local lain  = require("lain")
local awful = require("awful")
local wibox = require("wibox")
local dpi   = require("beautiful.xresources").apply_dpi
local common = require("awful.widget.common")
local os = os
local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility
-- local quake         = require("util/quake")


local theme                                     = {}
theme.dir                                       = os.getenv("HOME") .. "/.config/awesome/themes/onedark"
theme.wallpaper                                 = theme.dir .. "/wall.png"
theme.font                                      = "UbuntuMono Nerd Font Bold 12"
theme.taglist_font                              = "UbuntuMono Nerd Font Bold 12"
theme.tasklist_font                             = "UbuntuMono Nerd Font Bold 12"
theme.fg_normal                                 = "#d8dee9"
theme.fg_focus                                  = "#9ec07c"
theme.fg_urgent                                 = "#CC9393"
theme.bg_normal                                 = "#1E222A"
theme.bg_focus                                  = "#282C34"
theme.bg_urgent                                 = "#1A1A1A"
theme.border_width                              = dpi(4)
theme.border_normal                             = "#282C34"
theme.border_focus                              = "#4d78cc"
-- #946EDF
theme.border_marked                             = "#CC9393"
theme.tasklist_bg_focus                         = "#1E222A"
theme.taglist_bg_focus                          = "#282C34"
theme.taglist_fg_focus                          = "#4d78cc"
-- #946EDF
theme.taglist_fg_occupied                       = "#e5c07a"
theme.titlebar_bg_focus                         = theme.bg_focus
theme.titlebar_bg_normal                        = theme.bg_normal
theme.titlebar_fg_focus                         = "#9ec07c"
theme.menu_height                               = dpi(16)
theme.menu_width                                = dpi(140)
theme.taglist_disable_icon                      = true
theme.layout_txt_tile                           = "[]="
theme.layout_txt_tileleft                       = "=[]"
theme.layout_txt_tilebottom                     = "[TTT]"
theme.layout_txt_tiletop                        = "[TT]"
theme.layout_txt_fairv                          = "[HHH]"
theme.layout_txt_fairh                          = "[fh]"
theme.layout_txt_spiral                         = "[@]"
theme.layout_txt_dwindle                        = "[\\]"
theme.layout_txt_max                            = "[m]"
theme.layout_txt_fullscreen                     = "[F]"
theme.layout_txt_magnifier                      = "[M]"
theme.layout_txt_floating                       = "><>"
theme.menu_submenu_icon                         = theme.dir .. "/icons/submenu.png"
theme.taglist_squares_sel                       = theme.dir .. "/icons/squarefwb.png"
theme.taglist_squares_unsel                     = theme.dir .. "/icons/squarew.png"
theme.widget_mem                                = theme.dir .. "/icons/mem.png"
theme.widget_cpu                                = theme.dir .. "/icons/cpu.png"
theme.widget_temp                               = theme.dir .. "/icons/temp.png"
theme.widget_net                                = theme.dir .. "/icons/net.png"
theme.widget_hdd                                = theme.dir .. "/icons/hdd.png"
theme.widget_music                              = theme.dir .. "/icons/note.png"
theme.widget_music_on                           = theme.dir .. "/icons/note_on.png"
theme.widget_vol                                = theme.dir .. "/icons/vol.png"
theme.widget_vol_low                            = theme.dir .. "/icons/vol_low.png"
theme.widget_vol_no                             = theme.dir .. "/icons/vol_no.png"
theme.widget_vol_mute                           = theme.dir .. "/icons/vol_mute.png"
theme.taglist_square_size                       = dpi(8)
theme.tasklist_spacing                          = dpi(10)
theme.taglist_spacing                           = dpi(10)
theme.tasklist_align                            = "left"
theme.tasklist_disable_task_name                = false
theme.tasklist_plain_task_name                  = true
theme.tasklist_disable_icon                     = true
theme.useless_gap                               = dpi(9)
theme.titlebar_close_button_focus               = theme.dir .. "/icons/titlebar/close_focus.png"
theme.titlebar_close_button_normal              = theme.dir .. "/icons/titlebar/close_normal.png"
theme.titlebar_ontop_button_focus_active        = theme.dir .. "/icons/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active       = theme.dir .. "/icons/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive      = theme.dir .. "/icons/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive     = theme.dir .. "/icons/titlebar/ontop_normal_inactive.png"
theme.titlebar_sticky_button_focus_active       = theme.dir .. "/icons/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active      = theme.dir .. "/icons/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive     = theme.dir .. "/icons/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive    = theme.dir .. "/icons/titlebar/sticky_normal_inactive.png"
theme.titlebar_floating_button_focus_active     = theme.dir .. "/icons/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active    = theme.dir .. "/icons/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive   = theme.dir .. "/icons/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive  = theme.dir .. "/icons/titlebar/floating_normal_inactive.png"
theme.titlebar_maximized_button_focus_active    = theme.dir .. "/icons/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active   = theme.dir .. "/icons/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = theme.dir .. "/icons/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = theme.dir .. "/icons/titlebar/maximized_normal_inactive.png"

-- lain related
theme.layout_txt_termfair                       = "[termfair]"
theme.layout_txt_centerwork                     = "[|M|]"
theme.layout_txt_centerfair                     = "[|M|]"

local function update_txt_layoutbox(s)
    -- Writes a string representation of the current layout in a textbox widget
    local txt_l = theme["layout_txt_" .. awful.layout.getname(awful.layout.get(s))] or ""
    s.mytxtlayoutbox:set_text(txt_l)
end

local markup = lain.util.markup
local separators = lain.util.separators

local keyboardlayout = awful.widget.keyboardlayout:new()

-- Textclock
local clockicon = wibox.widget.imagebox(theme.widget_clock)
local clock = awful.widget.watch(
    "date +'  %a %b %d - %R'", 60,
    function(widget, stdout)
        widget:set_markup(" " .. markup.fontfg("UbuntuMono Nerd Font Bold 12", "#9ec07c", stdout))
    end
)
-- Calendar
theme.cal = lain.widget.cal({
    attach_to = { clock },
    notification_preset = {
        font = "UbuntuMono Nerd Font Bold 12",
        fg   = "#9ec07c",
        bg   = theme.bg_normal
    }
})

-- MEM
local memicon = wibox.widget.imagebox(theme.widget_mem)
local mem = lain.widget.mem({
    settings = function()
        widget:set_markup(markup.fontfg("UbuntuMono Nerd Font Bold 12", "#bc7ad9", " " .. "  " .. mem_now.used .. " MB "))
    end
})

-- CPU
local cpuicon = wibox.widget.imagebox(theme.widget_cpu)
local cpu = lain.widget.cpu({
    settings = function()
        widget:set_markup(markup.fontfg("UbuntuMono Nerd Font Bold 12", "#e5c07a", " " .. "﬙  " .. cpu_now.usage .. "% "))
    end
})

-- Coretemp
local tempicon = wibox.widget.imagebox(theme.widget_temp)
local temp = lain.widget.temp({
    tempfile = "/sys/devices/platform/nct6775.2592/hwmon/hwmon2/temp7_input",
    settings = function()
        widget:set_markup(markup.fontfg("UbuntuMono Nerd Font Bold 12", "#56b6c2", " " .. "﨎 " .. coretemp_now .. "°C "))
    end
})

-- ALSA volume
local volicon = wibox.widget.imagebox(theme.widget_vol)
theme.volume = lain.widget.alsa({
    settings = function()
        if volume_now.status == "off" then
            volicon:set_image(theme.widget_vol_mute)
        elseif tonumber(volume_now.level) == 0 then
            volicon:set_image(theme.widget_vol_no)
        elseif tonumber(volume_now.level) <= 50 then
            volicon:set_image(theme.widget_vol_low)
        else
            volicon:set_image(theme.widget_vol)
        end

        widget:set_markup(markup.fontfg("UbuntuMono Nerd Font Bold 12", "#71abeb", " " .. "  " .. volume_now.level .. "% "))
    end
})
theme.volume.widget:buttons(awful.util.table.join(
                               awful.button({}, 4, function ()
                                     awful.util.spawn("amixer set Master 5%+")
                                     theme.volume.update()
                               end),
                               awful.button({}, 5, function ()
                                     awful.util.spawn("amixer set Master 5%-")
                                     theme.volume.update()
                               end)
))

-- Net
local neticon = wibox.widget.imagebox(theme.widget_net)
local net = lain.widget.net({
    settings = function()
        widget:set_markup(markup.fontfg("UbuntuMono Nerd Font Bold 12", "#e06c75", " " .. " " .. net_now.received .. "   " .. net_now.sent .. " "))
    end
})

-- Separators
local spr     = wibox.widget.textbox(' ')
local arrl_dl = separators.arrow_left(theme.bg_focus, "alpha")
local arrl_ld = separators.arrow_left("alpha", theme.bg_focus)
--local systray = wibox.widget.systray()
--systray:set_base_size(28)

function list_update(w, buttons, label, data, objects)
    -- call default widget drawing function
    common.list_update(w, buttons, label, data, objects)
    -- set widget size
    w:set_max_widget_size(300)
end


function theme.at_screen_connect(s)
    -- Quake application
   -- s.quake = quake({ app = "alacritty",argname = "--title %s",extra = "--class QuakeDD -e tmux", visible = true, width = 3827 ,  height = 0.5 })


    -- If wallpaper is a function, call it with the screen
    local wallpaper = theme.wallpaper
    if type(wallpaper) == "function" then
        wallpaper = wallpaper(s)
    end
    gears.wallpaper.maximized(wallpaper, s, true)

    -- Tags
    awful.tag(awful.util.tagnames, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

    -- Textual layoutbox
    s.mytxtlayoutbox = wibox.widget.textbox(theme["layout_txt_" .. awful.layout.getname(awful.layout.get(s))])
    awful.tag.attached_connect_signal(s, "property::selected", function () update_txt_layoutbox(s) end)
    awful.tag.attached_connect_signal(s, "property::layout", function () update_txt_layoutbox(s) end)
    s.mytxtlayoutbox:buttons(my_table.join(
                           awful.button({}, 1, function() awful.layout.inc(1) end),
                           awful.button({}, 2, function () awful.layout.set( awful.layout.layouts[1] ) end),
                           awful.button({}, 3, function() awful.layout.inc(-1) end),
                           awful.button({}, 4, function() awful.layout.inc(1) end),
                           awful.button({}, 5, function() awful.layout.inc(-1) end)))
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, awful.util.taglist_buttons)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.focused)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s, height = dpi(26), bg = theme.bg_normal, fg = theme.fg_normal })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            --awesome_icon,
            spr,
            s.mytaglist,
            spr,
            spr,
            spr,
            s.mytxtlayoutbox,
            spr,
            spr,
            spr,
            s.mypromptbox,
            spr,
            spr,
            spr,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            spr,
            arrl_ld,
            wibox.container.background(net.widget, theme.bg_focus),
            arrl_dl,
            temp.widget,
            spr,
            arrl_ld,
            wibox.container.background(cpu.widget, theme.bg_focus),
            arrl_dl,
            mem.widget,
            arrl_ld,
            wibox.container.background(theme.volume.widget, theme.bg_focus),
            arrl_dl,
            clock,
            spr,
            spr,
            --arrl_ld,
            --wibox.container.background(s.mylayoutbox, theme.bg_focus),
            wibox.widget.systray(theme.bg_focus),
        },
    }
end

return theme
