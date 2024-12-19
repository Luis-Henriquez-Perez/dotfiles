### qtile
# This blog post gives a very good explanation.
# https://blog.neerajadhav.in/customizing-your-workspace-with-qtile-a-look-at-my-configuration-file

# Huge tip my brothers, checkout the ~/.local/share/qtile.log file if something
# you did does not seem to work or if restarting does not successfully reset the
# config.  If that happens it is probably because of an error.  Any error that
# you get will be logged to =qtile.log=.

#### import libraries
from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
#### declare variables
mod = "mod4"
# mod1 = alt
# mod2 = "control"
# home = os.path.expanduser('~')
# browser = "firefox"

terminal = guess_terminal()
#### create a =define-key= function
# As I have mentioned in my ~/.config/awesome/rc.lua file I do not like lots of
# elements one by one to a list like this.  I would rather call a function that
# binds the key for me.
keys = []

def define_key(mod, key, command, desc=""):
    """
    Define a keybinding and add it to the 'keys' list.

    :param mod: Modifier key (e.g., ["mod4"] for the Super key)
    :param key: The key to bind
    :param command: The command to execute
    :param desc: A description for the keybinding (optional)
    """
    keys.append(Key(mod, key, command, desc=desc))
#### set keybindings
# A list of available commands that can be bound to keys can be found
# at https://docs.qtile.org/en/latest/manual/config/lazy.html
# Switch between windows
define_key([mod], "h", lazy.layout.left(), desc="Move focus to left"),
define_key([mod], "l", lazy.layout.right(), desc="Move focus to right"),
define_key([mod], "j", lazy.layout.down(), desc="Move focus down"),
define_key([mod], "k", lazy.layout.up(), desc="Move focus up"),
define_key([mod], "e", lazy.spawn("emacs"), desc="Open emacs"),
define_key([mod], "i", lazy.spawn("qutebrowser"), desc="Open qutebrowser"),
define_key([mod], "space", lazy.layout.next(), desc="Move window focus to other window"),
# Move windows between left/right columns or move up/down in current stack.
# Moving out of range in Columns layout will create new column.
define_key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
define_key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right"),
define_key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
define_key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
# Grow windows. If current window is on the edge of screen and direction
# will be to screen edge - window would shrink.
define_key([mod, "control"], "h", lazy.layout.grow_left(), desc="Grow window to the left"),
define_key([mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"),
define_key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
define_key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
define_key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
# Toggle between split and unsplit sides of stack.
# Split = all windows displayed
# Unsplit = 1 window displayed, like Max layout, but still with
# multiple stack panes
define_key([mod, "shift"], "Return",lazy.layout.toggle_split(),desc="Toggle between split and unsplit sides of stack"),
define_key([mod], "Return", lazy.spawn(terminal), desc="Launch terminal"),
# Toggle between different layouts as defined below
define_key([mod], "Tab", lazy.next_layout(), desc="Toggle between layouts"),
define_key([mod], "w", lazy.window.kill(), desc="Kill focused window"),
define_key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
define_key([mod], "f", lazy.window.toggle_fullscreen(), desc="Toggle fullscreen on the focused window"),
define_key([mod], "t", lazy.window.toggle_floating(), desc="Toggle floating on the focused window"),
define_key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
define_key([mod, "shift"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
define_key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),
define_key([mod], "r", lazy.spawncmd(), desc="Spawn a command using a prompt widget")

#### add keybindings for groups
groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(i.name),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )

#### specify layouts
layouts = [
    layout.Columns(border_focus_stack=["#d75f5f", "#8f3d3d"],
                   # Damn, **** these commas bro.  I come from lisp and I do not
                   # have to deal with commas.
                   border_width=4,
                   # https://docs.qtile.org/en/latest/manual/ref/layouts.html
                   # https://www.reddit.com/r/linux4noobs/comments/gc6y0g/how_do_i_add_gaps_to_qtile/
                   # According to [[][DT]] himself this is how you add gaps.
                   # Only thing that struck me about this is that you seem to
                   # need to do this to every layout individually.  Though I am
                   # sure there is an easy way to do it programmatically (once I
                   # re-familiarize myself with python syntax.)
                   margin = 10
                   ),
    # layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

#### Rotate windows
def rotate_windows(qtile, forward=True):
    """
    Rotate the windows in the current layout.

    Parameters:
    - qtile: The Qtile instance.
    - forward (bool): Direction of rotation. True for forward, False for backward.
    """
    current_group = qtile.current_group
    if not current_group:
        return

    windows = current_group.windows
    if len(windows) < 2:
        return

    if forward:
        # Move the first window to the end
        window = windows.pop(0)
        windows.append(window)
    else:
        # Move the last window to the beginning
        window = windows.pop()
        windows.insert(0, window)

    # Apply the new order
    for i, win in enumerate(windows):
        win.group.focus(win, stack=False)
        win.index = i

    qtile.current_layout.group.layout_all()
#### uncategorized
widget_defaults = dict(
    font="sans",
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.CurrentLayout(),
                widget.GroupBox(),
                widget.Prompt(),
                widget.WindowName(),
                widget.Chord(
                    chords_colors={
                        "launch": ("#ff0000", "#ffffff"),
                    },
                    name_transform=lambda name: name.upper(),
                ),
                widget.TextBox("default config", name="default"),
                widget.TextBox("Press &lt;M-r&gt; to spawn", foreground="#d75f5f"),
                # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
                # widget.StatusNotifier(),
                widget.Systray(),
                widget.Clock(format="%Y-%m-%d %a %I:%M %p"),
                widget.QuickExit(),
            ],
            24,
            # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
            # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
        ),
        # You can uncomment this variable if you see that on X11 floating resize/moving is laggy
        # By default we handle these events delayed to already improve performance, however your system might still be struggling
        # This variable is set to None (no cap) by default, but you can set it to 60 to indicate that you limit it to 60 events per second
        # x11_drag_polling_rate = 60,
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
