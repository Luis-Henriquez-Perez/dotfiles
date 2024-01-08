--- awesome
-- Awesome is a window manager configured in lua.  It is a fork of dwm that is
-- designed to be more sane.
-- https://www.reddit.com/r/awesomewm/comments/sa5sd6/any_configuration_tutorial/
-- https://www.reddit.com/r/unixporn/
-- https://forum.arch-linux.cz/topic/8/awesomewm-awesome-window-manager?_=1661459317840
-- https://github.com/atsepkov/awesome-awesome-wm
-- https://www.reddit.com/r/stumpwm/comments/n7daiy/floating_dynamic_tiling/
-- https://unix.stackexchange.com/questions/495998/cli-utility-to-search-and-view-download-youtube-videos
-- https://stackoverflow.com/questions/15087379/how-to-navigate-open-programs-in-awesome-wm-using-arrow-keys-whilst-in-tile-layo
---- initial notes & challanges
-- This file came with a lot of challenges.
----- prolific use of tabs
-- I prefer spaces.  I called =untabify= on the entire buffer to
-- convert the tabs to spaces.  Also I really do not like the folding syntax the
----- unsatisfactory and unfamiliar folding syntax
-- file came with it uses "-- {{{" followed by a "-- }}}" to close off the
-- section that should be folded.  That is a lot compared to.  First, I think
-- the brackets are unnecessary.
----- auto-fill-mode did not properly update comments
-- auto-fill-mode did not properly update the comments.  I thought this would
-- have been resolved with the [[][lua-mode]] package.
----- not too fond of language syntax
-- This is more of an issue with me.  I cannot say I was too fond of the lua
-- language.  I prefer languages are low on syntax.  Mostly I prefer lisps, but
-- python I also like too.  Lua, however, is a very syntax heavy language.  And
-- lines of code are so long that they have to be split up in various little
-- pieces which I also do not like.  I do not know maybe its the way the authors
-- of this file choose to split the code sometimes.  I may try to use
-- [[][fennel]], a lisp that compiles to lua code.
----- thoughts on using qtile instead
-- I was honestly thinking of using =qtile= over this window manager mostly
-- because I like and am more familiar with python than lua.  And I know its
-- much eaiser to configure python system choices.  But the reasons I decided to
-- give it an honest shot is because its good, it is faster and lighter than
-- =qtile=--and definitely lighter than stumpwm.  The difference in startup time
-- between the awesome and stumpwm is also huge, after invoking startx awesome
-- pops up in a flash.

-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
---- Resources
-- I will admit the following two resources were not a big help.
-- https://www.addictivetips.com/ubuntu-linux-tips/guide-awesomewm-windows-manager/
---https://scaron.info/blog/getting-started-with-awesome.html
pcall(require, "luarocks.loader")

---- Libraries
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

---- Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
   naughty.notify({
   preset = naughty.config.presets.critical,
   title = "Oops, there were errors during startup!",
   text = awesome.startup_errors,
   })
end

-- Handle runtime errors after startup
do
   local in_error = false
   awesome.connect_signal("debug::error", function(err)
   -- Make sure we don't go into an endless error loop
   if in_error then
   return
   end
   in_error = true

   naughty.notify({
   preset = naughty.config.presets.critical,
   title = "Oops, an error happened!",
   text = tostring(err),
   })
   in_error = false
   end)
end

---- Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")
beautiful.useless_gap = 10
-- This is used later as the default terminal and editor to run.
terminal = "alacritty"
editor = os.getenv("EDITOR") or "emacs"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
   -- awful.layout.suit.floating,
   awful.layout.suit.tile,
   awful.layout.suit.tile.left,
   awful.layout.suit.tile.bottom,
   awful.layout.suit.tile.top,
   awful.layout.suit.fair,
   awful.layout.suit.fair.horizontal,
   awful.layout.suit.spiral,
   awful.layout.suit.spiral.dwindle,
   awful.layout.suit.max,
   awful.layout.suit.max.fullscreen,
   awful.layout.suit.magnifier,
   awful.layout.suit.corner.nw,
   -- awful.layout.suit.corner.ne,
   -- awful.layout.suit.corner.sw,
   -- awful.layout.suit.corner.se,
}

-- I do not want a menu. I am good.  Though the code could come in handy if I
-- want to set awesome up for my mom, who would appreciate a menu.
---- Menu
-- Create a launcher widget and a main menu
-- myawesomemenu = {
-- {
-- "hotkeys",
-- function()
-- hotkeys_popup.show_help(nil, awful.screen.focused())
-- end,
-- },
-- { "manual", terminal .. " -e man awesome" },
-- { "edit config", editor_cmd .. " " .. awesome.conffile },
-- { "restart", awesome.restart },
-- {
-- "quit",
-- function()
-- awesome.quit()
-- end,
-- },
-- }

-- mymainmenu = awful.menu({
-- items = {
-- { "awesome", myawesomemenu, beautiful.awesome_icon },
-- { "open terminal", terminal },
-- },
-- })

-- mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon, menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

---- Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock()

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
    awful.button({}, 1, function(t) t:view_only() end),
    awful.button({ modkey }, 1, function(t)
        if client.focus then
            client.focus:move_to_tag(t)
        end
    end),
    awful.button({}, 3, awful.tag.viewtoggle),
    awful.button({ modkey }, 3, function(t)
        if client.focus then
            client.focus:toggle_tag(t)
        end
    end),
    awful.button({}, 4, function(t)
        awful.tag.viewnext(t.screen)
    end),
    awful.button({}, 5, function(t)
        awful.tag.viewprev(t.screen)
    end)
)

local tasklist_buttons = gears.table.join(
    awful.button({}, 1, function(c)
        if c == client.focus then
            c.minimized = true
        else
            c:emit_signal("request::activate", "tasklist", { raise = true })
        end
    end),
    awful.button({}, 3, function()
        awful.menu.client_list({ theme = { width = 250 } })
    end),
    awful.button({}, 4, function()
        awful.client.focus.byidx(1)
    end),
    awful.button({}, 5, function()
        awful.client.focus.byidx(-1)
    end)
)

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
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    -- I already set the wallpaper via ~/.xprofile as well as
    -- ~/.config/cron/jobs.guile, I therefore do not need/want awesome to do it.
    -- set_wallpaper(s)
    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- I do not want this either, I am going to use a keybinding for switching
    -- layout, but no button.
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    -- s.mylayoutbox = awful.widget.layoutbox(s)
    -- s.mylayoutbox:buttons(gears.table.join(
    --     awful.button({}, 1, function() awful.layout.inc(1) end),
    --     awful.button({}, 3, function() awful.layout.inc(-1) end),
    --     awful.button({}, 4, function() awful.layout.inc(1) end),
    --     awful.button({}, 5, function() awful.layout.inc(-1) end)
    -- ))
    -- Create a taglist widget
    -- s.mytaglist = awful.widget.taglist({
    --     screen = s,
    --     filter = awful.widget.taglist.filter.all,
    --     buttons = taglist_buttons,
    -- })

    -- Create a tasklist widget
    -- s.mytasklist = awful.widget.tasklist({
    --     screen = s,
    --     filter = awful.widget.tasklist.filter.currenttags,
    --     buttons = tasklist_buttons,
    -- })

    -- Create the wibox
    -- s.mywibox = awful.wibar({ position = "top", screen = s })

    -- Add widgets to the wibox
    -- s.mywibox:setup({
    --     layout = wibox.layout.align.horizontal,
    --     { -- Left widgets
    --         layout = wibox.layout.fixed.horizontal,
    --         -- mylauncher,
    --         -- s.mytaglist,
    --         -- s.mypromptbox,
    --     },
    --     s.mytasklist, -- Middle widget
    --     { -- Right widgets
    --         layout = wibox.layout.fixed.horizontal,
    --         mykeyboardlayout,
    --         wibox.widget.systray(),
    --         mytextclock,
    --         s.mylayoutbox,
    --     },
    -- })
end)

---- Mouse bindings
root.buttons(gears.table.join(
   awful.button({}, 3, function()
   mymainmenu:toggle()
   end),
   awful.button({}, 4, awful.tag.viewnext),
   awful.button({}, 5, awful.tag.viewprev)
))

---- Custom Functions
-- Make functions one line; it is too confusing to have them in separate lines.
-- And stop it with these anonymous functions, just use named functions please.

----- master layout functions
-- Replicate master-slave stuff with dwm and what I had with stumpwm.  When I
-- pressed <C-z j> in stumpwm it would bring the next window to master.

-- This is not exactly what I want yet.
local function swap_with_master ()
   awful.client.swap.byidx(1)
   awful.client.focus.byidx(-1)
end
----- launching apps
-- I prefer the term "open" to "launch" but I want to be consistent with the
-- term that the writers of this file used.  Whether that is even worth I do not
-- know.
local function launch_emacsclient ()
   -- The option "-r" makes emacs client use an existing frame if possible, if
   -- not it creates a new one.
   awful.spawn("emacsclient -r")
end

local function launch_emacs ()
   awful.spawn("emacs")
end

local function launch_firefox ()
   awful.spawn("firefox")
end

local function launch_chromium ()
   awful.spawn("chromium")
end

local function launch_nyxt ()
   awful.spawn("nyxt")
end

local function launch_qutebrowser ()
   awful.spawn("qutebrowser")
end

local function launch_terminal ()
   awful.spawn(terminal)
end

----- take a screenshot
-- local function take_screenshot()
--    -- "maim -S"
--    -- os.execute("maim")
-- end
----- next and previous layout
-- I do not know am I going to make function wrappers around all of these one-liners.  I do not like so many anonymous lambdas.  I would rather have named functions.
local function next_layout()
   awful.layout.inc(1)
end

local function previous_layout()
   awful.layout.inc(-1)
end
---- Key bindings
-- I do not care if the lines exceed the typical 80 characters, I just want them
-- to be readable.
----- global keys
-- Something that I do here is create functions for defining keys.  As I have
-- mentioned I do not like
------ declare global keys
-- Do not put anything in these keys.  Ill do that with =define-key=. 
globalkeys = gears.table.join()
------ define a function for adding global keys
-- So I am not a fan of the style where the authors added all of the keybindings
-- in the =gears.table.join(...)= code block.  It makes for a very large code
-- block.
-- I got this function from chatgpt so well see if it works.
function global_key(modifiers, key, action, description)
    local newkey = awful.key(modifiers, key, action, description)
    globalkeys = gears.table.join(globalkeys, newkey)
end
------ define a function for adding client keys
function client_key(modifiers, key, action, description)
    local newkey = awful.key(modifiers, key, action, description)
    clientkeys = gears.table.join(clientkeys, newkey)
end
------ note on aligning keybindings
-- Oh my gosh!  It is so much easier to read the bindings if they are spaced
-- appropriately like this.  The way the bindings in the original file were
-- formatted was so hard to read.  I hated it.  This is so much better.  I feel
-- you have to do this considering how syntax heavy lua is.  But I am unsure of
-- wether I should just align bindings under a heading or all the bindings with
-- each other.  I suppose for readability only doing it with each other is
-- necessary.  If I align with all the bindings some fields will be very long.

-- I have to say I have had trouble finding a good function to align text.
------ launching applications
global_key({ modkey }, "e", launch_emacs      , { description = "Launch Emacs", group = "applications"})
-- I have experimented with starting emacs from emacsclient on first login.  I
-- have found that right now at least emacs takes a long time to startup and
-- slows the login significantly.  I do not think this is just emacs alone; I
-- think other stuff is happening in the background.
global_key({ modkey }, "E", launch_emacsclient , { description = "Launch Emacsclient" , group = "applications" })
global_key({ modkey }, "i", launch_firefox     , { description = "Launch Firefox"     , group = "applications" })
global_key({ modkey }, "c", launch_chromium    , { description = "Launch Chromium"    , group = "applications" })
global_key({ modkey }, "n", launch_nyxt        , { description = "Launch Nyxt"        , group = "applications" })
global_key({ modkey }, "t", launch_terminal    , { description = "Launch Terminal"    , group = "applications" })
------ layout manipulation
-- My idea here is to try to keep the focus on master at all times.  That's what
-- I interpreted as the point of master-slave layout, you never want to focus on
-- a slave; instead you want to swap master with a desired slave, adding the
-- focus to the new master.

-- I had this function using stumpwm.  It was bound to =C-z n=.  I found it very
-- useful.  By default awesome provides bindings for changing the focus to
-- the next/previous client and functions to switch clients.  But I never want
-- to.  I got the code for this function from chatgpt.

-- I am intending this to be called from the master window.  I want the focus to
-- remain on the master window.
local function rotate_clients_forward ()
   -- I found this by looking at the API for client: 
   -- https://awesomewm.org/doc/api/classes/client.html#lib_awful_client_focus_Functions
   awful.client.cycle(true)
   local master = awful.client.getmaster()
   -- I got this part from a little help from chatgpt.  to be honest it does
   -- confuse me a little.  So the focus is whatever client is the value of
   -- =client.focus=?  And what does raising do?  Is it necessary here.
   if master then
       client.focus = master
       master:raise()
   end
end

-- At first I thought I would not have this function but I do not know.  It is
-- nice not to have to rotate all the way back if I overstep.
local function rotate_clients_backward ()
   awful.client.cycle(false)
   local master = awful.client.getmaster()
   if master then
       client.focus = master
       master:raise()
   end
end
-- local function focus_next_client()
--    awful.client.focus.byidx( 1)
-- end

-- local function focus_previous_client()
--    awful.client.focus.byidx(-1)
-- end

global_key({ modkey }, "j", rotate_clients_forward,  { description = "rotate forwards" , group = "client" })
global_key({ modkey }, "k", rotate_clients_backward, { description = "rotate backwards", group = "client" })
-- Do not know what this does; it does not seem to be working.
-- global_key({ modkey }, "u", awful.client.urgent.jumpto, { description = "jump to urgent client"  , group = "client" })

-- Use =y= for now because I want to make sure its not bound to anything.
-- global_key({ modkey }, "y", rotate_clients_forward, { description = "Rotate clients forward", group = "client" })
-- I am not sure if I will make the corresponding =rotate_clients_backward=
-- command because I do not find it necessary.  For simplicity I will only ever
-- rotate one way.
------ essentials
-- Note that key bindings do not work with capital letters.  As in, I would not
-- be able to shorten this by using "R" or "Q" and leaving out "Shift" (I
-- tried).  Not a big deal but something to take note of.
-- global_key({ modkey, "Shift"}, "r", awesome.restart, { description = "reload awesome" , group = "awesome" })
global_key({ modkey, "Shift"}, "r", awesome.restart, { description = "reload awesome" , group = "awesome" })
global_key({ modkey, "Shift"}, "q", awesome.quit   , { description = "quit awesome"   , group = "awesome" })
------ switching layout
global_key({ modkey },           "space", next_layout    , { description = "select next"     , group = "layout" })
global_key({ modkey , "Shift" }, "space", previous_layout, { description = "select previous" , group = "layout" })

local function increase_master_width_factor ()
   awful.tag.incmwfact( 0.05)
end
 
local function decrease_master_width_factor ()
   awful.tag.incmwfact(-0.05)
end

local function increase_number_of_master_clients ()
   awful.tag.incnmaster(1, nil, true)
end

local function decrease_number_of_master_clients ()
   awful.tag.incnmaster(-1, nil, true)
end

local function increase_number_of_columns ()
   awful.tag.incncol( 1, nil, true)
end

local function decrease_number_of_columns ()
   awful.tag.incncol(-1, nil, true)
end

global_key({ modkey, "Shift" }, "l", increase_number_of_master_clients, { description = "increase master clients", group = "layout"})
global_key({ modkey, "Shift" }, "h", decrease_number_of_master_clients, { description = "decrease master clients", group = "layout"})

global_key({ modkey }, "l", increase_master_width_factor, { description = "increase master width factor", group = "layout"})
global_key({ modkey }, "h", decrease_master_width_factor, { description = "decrease master width factor", group = "layout"})

-- Not sure what this does yet.
global_key({ modkey, "Control" }, "l", increase_number_of_columns, { description = "increase master width factor", group = "layout"})
global_key({ modkey, "Control" }, "h", decrease_number_of_columns, { description = "decrease master width factor", group = "layout"})
------ keybinding menu
-- This key
global_key({ modkey }, "s", hotkeys_popup.show_help, { description = "show help", group = "awesome" })
------ run an application
global_key({ modkey }, "r", function () awful.screen.focused().mypromptbox:run() end, {description = "run prompt", group = "launcher"})
------ enable the bindings
root.keys(globalkeys)
----- client Keys
client_key({ modkey            }, "f",      function(c) c.fullscreen = not c.fullscreen c:raise() end, { description = "toggle fullscreen", group = "client" })
-- I want a shorter binding for quitting a window (or in awesome parlance a
-- "client").  The binding =MOD SHIFT c= is just too much especially for such a
-- common occurance.  I can fully understand why you would want the binding to
-- quit out of awesome to be harder to press--you would not want to risk doing
-- that by accident.  But quitting a window is too common to have a long binding
-- for in my opinion.
client_key({ modkey }, "q",      function(c) c:kill() end, { description = "close", group = "client" })
client_key({ modkey, "Shift"   }, "c",      function(c) c:kill() end, { description = "close", group = "client" })
client_key({ modkey, "Control" }, "space", awful.client.floating.toggle, { description = "toggle floating", group = "client" })
client_key({ modkey, "Control" }, "Return", function(c) c:swap(awful.client.getmaster()) end, { description = "move to master", group = "client" })
client_key({ modkey            }, "o",      function(c) c:move_to_screen() end, { description = "move to screen", group = "client" })
client_key({ modkey            }, "t",      function(c) c.ontop = not c.ontop end, { description = "toggle keep on top", group = "client" })
-- The client currently has the input focus, so it cannot be minimized, since minimized clients can't have the focus.
client_key({ modkey            }, "n",      function(c) c.minimized = true end, { description = "minimize", group = "client" })
client_key({ modkey            }, "m",      function(c) c.maximized = not c.maximized c:raise() end, { description = "(un)maximize", group = "client" })
client_key({ modkey, "Control" }, "m",      function(c) c.maximized_vertical = not c.maximized_vertical c:raise() end, { description = "(un)maximize vertically", group = "client" })
client_key({ modkey, "Shift"   }, "m",      function(c) c.maximized_horizontal = not c.maximized_horizontal c:raise() end, { description = "(un)maximize horizontally", group = "client" })

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
   globalkeys = gears.table.join(
   globalkeys,
   -- View tag only.
   awful.key({ modkey }, "#" .. i + 9, function()
   local screen = awful.screen.focused()
   local tag = screen.tags[i]
   if tag then
   tag:view_only()
   end
   end, { description = "view tag #" .. i, group = "tag" }),
   -- Toggle tag display.
   awful.key({ modkey, "Control" }, "#" .. i + 9, function()
   local screen = awful.screen.focused()
   local tag = screen.tags[i]
   if tag then
   awful.tag.viewtoggle(tag)
   end
   end, { description = "toggle tag #" .. i, group = "tag" }),
   -- Move client to tag.
   awful.key({ modkey, "Shift" }, "#" .. i + 9, function()
   if client.focus then
   local tag = client.focus.screen.tags[i]
   if tag then
   client.focus:move_to_tag(tag)
   end
   end
   end, { description = "move focused client to tag #" .. i, group = "tag" }),
   -- Toggle tag on focused client.
   awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9, function()
   if client.focus then
   local tag = client.focus.screen.tags[i]
   if tag then
   client.focus:toggle_tag(tag)
   end
   end
   end, { description = "toggle focused client on tag #" .. i, group = "tag" })
   )
end

clientbuttons = gears.table.join(
   awful.button({}, 1, function(c)
   c:emit_signal("request::activate", "mouse_click", { raise = true })
   end),
   awful.button({ modkey }, 1, function(c)
   c:emit_signal("request::activate", "mouse_click", { raise = true })
   awful.mouse.client.move(c)
   end),
   awful.button({ modkey }, 3, function(c)
   c:emit_signal("request::activate", "mouse_click", { raise = true })
   awful.mouse.client.resize(c)
   end)
)

---- Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
   -- All clients will match this rule.
   {
   rule = {},
   properties = {
   border_width = beautiful.border_width,
   border_color = beautiful.border_normal,
   focus = awful.client.focus.filter,
   raise = true,
   keys = clientkeys,
   buttons = clientbuttons,
   screen = awful.screen.preferred,
   placement = awful.placement.no_overlap + awful.placement.no_offscreen,
   },
   },

   -- Floating clients.
   {
   rule_any = {
   instance = {
   "DTA", -- Firefox addon DownThemAll.
   "copyq", -- Includes session name in class.
   "pinentry",
   },
   class = {
   "Arandr",
   "Blueman-manager",
   "Gpick",
   "Kruler",
   "MessageWin", -- kalarm.
   "Sxiv",
   "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
   "Wpa_gui",
   "veromix",
   "xtightvncviewer",
   },

   -- Note that the name property shown in xprop might be set slightly after creation of the client
   -- and the name shown there might not match defined rules here.
   name = {
   "Event Tester", -- xev.
   },
   role = {
   "AlarmWindow", -- Thunderbird's calendar.
   "ConfigManager", -- Thunderbird's about:config.
   "pop-up", -- e.g. Google Chrome's (detached) Developer Tools.
   },
   },
   properties = { floating = true },
   },

   -- Add titlebars to normal clients and dialogs
   { rule_any = { type = { "normal", "dialog" } }, properties = { titlebars_enabled = false } },

   -- Set Firefox to always map on the tag named "2" on screen 1.
   -- { rule = { class = "Firefox" },
   --   properties = { screen = 1, tag = "2" } },
}

---- Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function(c)
   -- Set the windows at the slave,
   -- i.e. put it at the end of others instead of setting it master.
   -- if not awesome.startup then awful.client.setslave(c) end

   if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
   -- Prevent clients from being unreachable after screen count changes.
   awful.placement.no_offscreen(c)
   end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
   -- buttons for the titlebar
   local buttons = gears.table.join(
   awful.button({}, 1, function()
   c:emit_signal("request::activate", "titlebar", { raise = true })
   awful.mouse.client.move(c)
   end),
   awful.button({}, 3, function()
   c:emit_signal("request::activate", "titlebar", { raise = true })
   awful.mouse.client.resize(c)
   end)
   )

   awful.titlebar(c):setup({
   { -- Left
   awful.titlebar.widget.iconwidget(c),
   buttons = buttons,
   layout = wibox.layout.fixed.horizontal,
   },
   { -- Middle
   { -- Title
   align = "center",
   widget = awful.titlebar.widget.titlewidget(c),
   },
   buttons = buttons,
   layout = wibox.layout.flex.horizontal,
   },
   { -- Right
   awful.titlebar.widget.floatingbutton(c),
   awful.titlebar.widget.maximizedbutton(c),
   awful.titlebar.widget.stickybutton(c),
   awful.titlebar.widget.ontopbutton(c),
   awful.titlebar.widget.closebutton(c),
   layout = wibox.layout.fixed.horizontal(),
   },
   layout = wibox.layout.align.horizontal,
   })
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
   c:emit_signal("request::activate", "mouse_enter", { raise = false })
end)

client.connect_signal("focus", function(c)
   c.border_color = beautiful.border_focus
end)
client.connect_signal("unfocus", function(c)
   c.border_color = beautiful.border_normal
end)
