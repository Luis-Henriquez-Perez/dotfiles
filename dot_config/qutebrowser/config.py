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
# c.search.wrap = False
### enable/disable javascript except on certain sites
c.content.javascript.enabled = False
# Qutebrowser uses javascript to display directories.
config.set('content.javascript.enabled', True, '*://iproyal.com/*')
config.set('content.javascript.enabled', True, '*://www.indeed.com/*')
config.set('content.javascript.enabled', True, "*://privateemail.com/*")
config.set('content.javascript.enabled', True, "*://www.paypal.com/*")
config.set('content.javascript.enabled', True, "*://ap.www.namecheap.com/*")
config.set('content.javascript.enabled', True, "*://privateemail.com/*")
config.set('content.javascript.enabled', True, "*://www.namecheap.com/*")
config.set('content.javascript.enabled', True, "file://*")
config.set('content.javascript.enabled', False, "*://mangadass.com/*")
config.set('content.javascript.enabled', False, "*://www.geeksforgeeks.org/*")
config.set('content.javascript.enabled', True, "*://github.com/*")
config.set('content.javascript.enabled', True, "*://duckduckgo.com/*")
config.set('content.javascript.enabled', True, "*://www.freelancer.com/*")
config.set('content.javascript.enabled', True, "*://www.upwork.com/*")
config.set('content.javascript.enabled', True, "*://chatgpt.com/*")
# I need to find away to only enable it for the
# config.bind('<Ctr-e>', ':set content.javascript.enabled true')
# javascript disable
# config.bind('<Ctr>j', ':set content.javascript.enabled false')
config.bind('<Ctrl+j>', 'set content.javascript.enabled !content.javascript.enabled')
### open new pages with =<space>ff=
# config.bind('<Space>ff', 'set-cmd-text -s :open')
### hide titlebars from qutebrowser
c.window.hide_decoration = True
### save with monolith
config.bind('<Ctrl-S>', 'spawn --userscript webpage-snapshot.hy')
### use old.reddit.com instead of www.reddit.com
# Reddit is a really bloated site causing it to be super slow despite only
# needing static output for the most part.
import qutebrowser.api.interceptor

def rewrite(request: qutebrowser.api.interceptor.Request):
    if request.request_url.host() == 'www.reddit.com':
        request.request_url.setHost('old.reddit.com')
        try:
            request.redirect(request.request_url)
        except:
            pass

qutebrowser.api.interceptor.register(rewrite)
