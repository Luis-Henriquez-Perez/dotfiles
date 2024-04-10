## Qutebrowser Config
# Qutebrowser is a minimal browser.  It has been developed for a long time.  It
# is configured in python.  I am using it as a replacement for firefox.  The
# main reason I was pushed into this change is the massive performance problems
# firefox was giving me.  It was consistently the program I had that kept
# clocking at 90% or more of my cpu power and thereby causing huge lags and
# delays when I would use my computer.  And in addition it is just not very
# customizable either.  In any case even if I could not use qutebrowser I would
# likely switch to falkon.

# https://github.com/gicrisf/qute-config/blob/main/config.org

config = config  # type: ConfigAPI
c = c  # type: ConfigContainer

config.load_autoconfig(False)

config.set('content.cookies.accept', 'no-3rdparty', 'chrome-devtools://*')
### always open new windows
# Allow the window manager to manage the windows.  I would rather avoid having
# tabs.
# https://www.qutebrowser.org/doc/help/settings.html
# Do not open tabs, instead open a new window.
c.tabs.tabs_are_windows = True

c.content.blocking.method = "both"

c.tabs.show = "multiple"
### do not auto-save sessions
# Note: The following commentary is copied verbatim from:
# http://www.ii.com/qutebrowser-configpy/

# Always restore open sites when qutebrowser is reopened. Without this
# option set, `:wq` (`:quit --save`) needs to be used to save open tabs
# (and restore them), while quitting qutebrowser in any other way will
# not save/restore the session. By default, this will save to the
# session which was last loaded. This behavior can be customized via the
# `session.default_name` setting.
# Type: Bool
c.auto_save.session = False
### do not wrap around when searching
# Note: The following commentary is copied verbatim from:
# http://www.ii.com/qutebrowser-configpy/

# Wrap around at the top and bottom of the page when advancing through
# text matches using `:search-next` and `:search-prev`.
# Type: Bool
c.search.wrap = False
### disable javascript everywhere except on DDG
# Disable JS globally
# c.content.javascript.enabled = False
# This is an equivalent form, I re-write in this other form for uniformity's sake in this particular block.
# c.content.javascript.enabled = False
config.set('content.javascript.enabled', False)

# Enable it on DDG
config.set('content.javascript.enabled', True, '*://duckduckgo.com/')

# And on github
# config.set('content.javascript.enabled', True, '*://duckduckgo.com/')
### provide java
# Surprisingly to me the [[][author]] I got this configuration from said that
# he noticed qutebrowser had some big performance issues compared to firefox.
# That would be insane because I moved from firefox to qutebrowser to run away
# from these performance issues.

# "Just looking at htop, looks like qutebrowser use more CPU than Firefox (in
# comparable conditions)."
# Though I will admit I do not know what "comparable conditions" mean.

# TODO: An improvement over what the [[][author]] provided would be to reload
# the page afterwards because when you enable javascript from a page where
# javascript had been disabled, you need to reload it to actually see the
# result.
# javascript enable
# Note that this applies to every webpage opened after enabling this.
# As in, it changes the default.
# I need to find away to only enable it for the
config.bind('<Space>je', ':set content.javascript.enabled true')

# javascript disable
config.bind('<Space>jd', ':set content.javascript.enabled false')
### open new pages with =<space>ff=
config.bind('<Space>ff', 'set-cmd-text -s :open')
### hide titlebars from qutebrowser
config.set('window.hide_decoration', True)
### save with monolith
config.bind('<Ctrl-S>', 'spawn --userscript save_with_monolith.sh')
