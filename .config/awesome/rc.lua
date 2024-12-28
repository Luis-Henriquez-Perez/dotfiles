-- Filename: rc.lua
-- Author: Luis Henriquez-Perez <luis@luishp.xyz>
-- Created: 2024-12-27 23:21:00
-- Description: Configure the awesome window manager.

-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")
local revelation=require("revelation")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
revelation.init()

-- This is used later as the default terminal and editor to run.
terminal = "alacritty"
editor_cmd = "emacsclient -r -a \"\""

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

beautiful.useless_gap = 10

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.corner.nw,
    -- awful.layout.suit.tile.left,
    awful.layout.suit.tile.top,
    awful.layout.suit.tile.bottom,
    -- The following layouts do not allow you to resize the client width.
    -- awful.layout.suit.fair,
    -- awful.layout.suit.fair.horizontal,
    -- awful.layout.suit.spiral,
    -- awful.layout.suit.spiral.dwindle,
    -- awful.layout.suit.max,
    -- awful.layout.suit.max.fullscreen,
    -- awful.layout.suit.magnifier,
    awful.layout.suit.corner.ne,
    -- I prefer windows on the top rather than the bottom, but trying it its not
    -- too bad.
    awful.layout.suit.corner.sw,
    awful.layout.suit.corner.se,
    -- Make floating last, as it is the one I am least likely to use.
    awful.layout.suit.floating,
}
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() hotkeys_popup.show_help(nil, awful.screen.focused()) end },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end },
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock()

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = gears.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  c:emit_signal(
                                                      "request::activate",
                                                      "tasklist",
                                                      {raise = true}
                                                  )
                                              end
                                          end),
                     awful.button({ }, 3, function()
                                              awful.menu.client_list({ theme = { width = 250 } })
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
-- screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    -- set_wallpaper(s)

    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons
    }

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons
    }

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s, visible = false})

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            mykeyboardlayout,
            wibox.widget.systray(),
            mytextclock,
            s.mylayoutbox,
        },
    }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = {}

local function globalkey(modifiers, key, action, description, group)
    -- Create a new keybinding using awful.key
    local newkey = awful.key(modifiers, key, action, {description=description, group=group})
    -- Append the new keybinding to global_keys
    globalkeys = gears.table.join(globalkeys, newkey)
end

local function client_focus_next ()
    awful.client.focus.byidx( 1)
end

local function client_focus_previous ()
    awful.client.focus.byidx(-1)
end

local function awesome_main_menu ()
    mymainmenu:show()
end

local function client_swap_next ()
    awful.client.swap.byidx( 1)
end

local function client_swap_previous ()
    awful.client.swap.byidx( -1)
end

local function screen_focus_next ()
    awful.screen.focus_relative( 1)
end

local function screen_focus_previous ()
    awful.screen.focus_relative(-1)
end

local function client_focus_last_visited ()
    awful.client.focus.history.previous()
    if client.focus then
        client.focus:raise()
    end
end

local function system_launch_terminal ()
    awful.spawn(terminal)
end

local function client_increment_width ()
    awful.tag.incmwfact( 0.05)
end

local function client_decrement_width ()
    awful.tag.incmwfact(-0.05)
end

local function layout_increase_num_master ()
    awful.tag.incnmaster( 1, nil, true)
end

local function layout_decrease_num_master ()
    awful.tag.incnmaster(-1, nil, true)
end

local function layout_increase_num_columns ()
    awful.tag.incncol( 1, nil, true)
end

local function layout_decrease_num_columns ()
    awful.tag.incncol(-1, nil, true)
end

local function layout_next ()
    awful.layout.inc( 1)
end

local function oo_next_layout ()
    awful.layout.inc(-1)
end

local function awesome_run_shell_command ()
    awful.screen.focused().mypromptbox:run()
end

local function awesome_run_lua_code ()
    awful.prompt.run { prompt = "Run Lua code: ",
                       textbox = awful.screen.focused().mypromptbox.widget,
                       exe_callback = awful.util.eval,
                       history_path = awful.util.get_cache_dir() .. "/history_evaluate"}
end

local function oo_show_menubar ()
    menubar.show()
end

local function client_rotate_forward()
   awful.client.cycle(true)
   local master = awful.client.getmaster()
   if master then
       client.focus = master
       return master:raise()
   else
      return nil
   end
end

local function client_rotate_backward()
    awful.client.cycle(false)
    local master = awful.client.getmaster()
    if master then
        client.focus = master
        return master:raise()
    else
       return nil
    end
end

local function system_take_screenshot()
    awful.spawn.with_shell("maim ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png")
    naughty.notify({title = "maim", text = "take screenshot"})
end

local function system_launch_emacs ()
    awful.spawn("emacsclient -r -a \"\"")
end

local function system_launch_firefox ()
    awful.spawn("firefox")
end

local function system_launch_qutebrowser ()
    awful.spawn("qutebrowser")
end

local function system_suspend ()
    awful.spawn("systemctl suspend")
end

local function system_suspend ()
    awful.spawn("systemctl suspend")
end

-- Annoyingly the focus after killing a client is moved to a slave instead of
-- the master window.
local function client_quit_and_focus_master (c)
    -- naughty.notify({ preset = naughty.config.presets.critical,
    --                  title = "quitting client",
    --                  text = "quiting" })
    client_quit(c)
    local master = awful.client.getmaster()
    -- Check if the master client exists and focus it
    if master then
       client.focus = master
       master:raise()
    end
end

-- This assumes that either all clients have titlebars or none of them do.  It
local function client_toggle_all_titlebars()
    local clients = client.get()
    if #clients == 0 then return end -- Exit if no clients exist

    for _, rule in ipairs(awful.rules.rules) do
        if rule.rule_any and rule.properties then
            rule.properties.titlebars_enabled = not rule.properties.titlebars_enabled
        end
    end

    for _, c in ipairs(clients) do
        awful.titlebar.toggle(c)
    end
end

-- https://www.reddit.com/r/awesomewm/comments/am1weq/how_do_i_bind_a_key_to_toggle_the_wibar/
local function awesome_toggle_wibox ()
    for s in screen do
        s.mywibox.visible = not s.mywibox.visible
    end
end

-- awesome
globalkey({ modkey, "Shift" }, "q", awesome.quit, "quit awesome", "awesome")
globalkey({ modkey, "Shift" }, "q", awesome.quit, "quit awesome", "awesome")
globalkey({ modkey, "Control" }, "r", awesome.restart, "reload awesome", "awesome")
globalkey({ modkey }, "s", hotkeys_popup.show_help, "show help", "awesome")
globalkey({ modkey }, "w", awesome_main_menu, "show main menu", "awesome")
globalkey({ modkey }, "x", awesome_run_lua_code, "lua execute prompt", "awesome")
globalkey({ modkey }, "g", awesome_toggle_wibox, "toggle wibox", "awesome")

-- system
globalkey({ modkey }, "u", system_launch_qutebrowser, "Launch qutebrowser", "system")
globalkey({ modkey }, "Return", system_launch_terminal, "Launch terminal", "system")
globalkey({ modkey }, "i", system_launch_firefox, "Launch firefox", "system")
globalkey({ modkey }, "e", system_launch_emacs, "Launch emacs", "system")
globalkey({ modkey }, "p", system_take_screenshot, "Take Screenshot", "system")
globalkey({ modkey, "Shift" }, "s" , system_suspend, "suspend", "system")

-- client
globalkey({ modkey }, "l", client_increment_width, "increase master width factor", "client")
globalkey({ modkey }, "h", client_decrement_width, "decrease master width factor", "client")
globalkey({ modkey }, "j", client_rotate_forward, "rotate forward", "client")
globalkey({ modkey }, "k", client_rotate_backward, "rotate backward", "client")
globalkey({ modkey }, "Tab", client_focus_last_visited, "go back", "client")
globalkey({ modkey }, "o", revelation, "expose of windows", "client")
globalkey({ modkey, "Shift" }, "j", client_swap_next, "swap with next client by index", "client")
globalkey({ modkey, "Shift" }, "k", client_swap_previous, "swap with previous client by index", "client")
globalkey({ modkey,}, "b", client_toggle_all_titlebars, "hide titlebars", "client")

-- layout
globalkey({ modkey, "Control" }, "h", layout_increase_num_columns, "increase the number of columns", "layout")
globalkey({ modkey, "Control" }, "l", layout_decrease_num_columns, "decrease the number of columns", "layout")
globalkey({ modkey, "Shift" }, "h", layout_increase_num_master, "increase the number of master clients", "layout")
globalkey({ modkey, "Shift" }, "l", layout_decrease_num_master, "decrease the number of master clients", "layout")
globalkey({ modkey }, "space" , layout_next, "select next", "layout")

-- tag
globalkey({ modkey }, "Left", awful.tag.viewprev , "view previous", "tag")
globalkey({ modkey }, "Right", awful.tag.viewnext , "view next", "tag")
globalkey({ modkey }, "Escape", awful.tag.history.restore, "go back", "tag")

-- screen
globalkey({ modkey, "Control" }, "j", screen_focus_next, "focus the next screen", "screen")
globalkey({ modkey, "Control" }, "k", screen_focus_previous, "focus the previous screen", "screen")

clientkeys = {}

local function clientkey(modifiers, key, action, description)
    -- Create a new keybinding using awful.key
    local newkey = awful.key(modifiers, key, action, {description=description, group="client"})
    -- Append the new keybinding to global_keys
    clientkeys = gears.table.join(clientkeys, newkey)
end

function client_fullscreen (c) c.fullscreen = not c.fullscreen c:raise() end
function client_quit (c) c:kill() end
function client_swap_with_master (c) c:swap(awful.client.getmaster()) end
function client_move_to_screen (c) c:move_to_screen() end
function client_toggle_put_on_top (c) c.ontop = not c.ontop end
function client_minimize (c) c.minimized = true end
function client_maximize (c) c.maximized = not c.maximized c:raise() end
function client_unmaximize_vertically (c) c.maximized_vertical = not c.maximized_vertical c:raise() end
function client_unmaximize_horizontally (c) c.maximized_horizontal = not c.maximized_horizontal c:raise() end

clientkey({ modkey, }, "f", client_fullscreen, "toggle fullscreen")
clientkey({ modkey, }, "o", client_move_to_screen, "move to screen")
clientkey({ modkey, }, "t", client_toggle_put_on_top, "toggle keep on top")
clientkey({ modkey, }, "n", client_minimize, "minimize")
clientkey({ modkey, }, "m", client_maximize, "(un)maximize")
clientkey({ modkey, }, "q", client_quit_and_focus_master, "close")
clientkey({ modkey, "Shift" }, "c", client_quit, "close")
clientkey({ modkey, "Shift" }, "m", client_unmaximize_horizontally, "(un)maximize horizontally")
clientkey({ modkey, "Control" }, "space", awful.client.floating.toggle , "toggle floating")
clientkey({ modkey, "Control" }, "Return", client_swap_with_master, "move to master")
clientkey({ modkey, "Control" }, "m", client_unmaximize_vertically, "(un)maximize vertically")

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     -- https://awesomewm.org/apidoc/documentation/90-FAQ.md.html
                     -- Prevent any application from starting off maximized.  I
                     -- experienced this with firefox.
                     maximized_vertical   = false,
                     maximized_horizontal = false,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
          "pinentry",
        },
        class = {
          "Arandr",
          "Blueman-manager",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
          "Wpa_gui",
          "veromix",
          "xtightvncviewer"},

        -- Note that the name property shown in xprop might be set slightly after creation of the client
        -- and the name shown there might not match defined rules here.
        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "ConfigManager",  -- Thunderbird's about:config.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }
      }, properties = { titlebars_enabled = false }
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup
      and not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", {raise = false})
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- awful.spawn(os.getenv("HOME") .. "/.bin/local/autorun.sh")
