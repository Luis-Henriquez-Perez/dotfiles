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
------ rotate clients forward
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
global_key({ modkey }, "s"     , hotkeys_popup.show_help  , { description = "show help"              , group = "awesome" })
------ run an application
global_key({ modkey }, "r", function () awful.screen.focused().mypromptbox:run() end, {description = "run prompt", group = "launcher"})
------ enable the bindings
root.keys(globalkeys)
