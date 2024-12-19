;;; Fennel config
;;;; Notes on migrating to fennel
;; I do not know if its a good idea overall.  I think there is justification.
;; For example, lisp support is leagues ahead of lua support in emacs.  If I can
;; program in lisp it lets me reuse all that lisp support when editing the
;; fennel file.  Another reason is that lisp is more concise and more enjoyable
;; for me to code in than lua.  I prefer programming in lisp languages.  A third
;; reason is that I have macro support so I can automate things in a way that
;; would be more difficult in lua.

;; I essentially had to go thought the while rc.lua file.
;;;; Libraries
;; Standard awesome library
(local awful (require :awful))
(local gears (require :gears))
(require :awful.autofocus)

;; Widget and layout library
(local wibox (require :wibox))

;; Theme handling library
(local beautiful (require :beautiful))

;; Notification library
(local naughty (require :naughty))

;; Declarative object management
;; (local ruled         (require :ruled))
(local menubar       (require :menubar))
(local hotkeys_popup (require :awful.hotkeys_popup))

;; This adds other bindings for things like tmux.  I disable it because I do not
;; use tmux or want these bindings that I have not set myself.
;; Enable hotkeys help widget for VIM and other apps
;; when client with a matching name is opened:
;; (require :awful.hotkeys_popup.keys)
;;;; Error Handling
(when awesome.startup_errors
  (naughty.notify {:preset naughty.config.presets.critical
                   :title "Oops, there were errors during startup!"
                   :text awesome.startup_errors }))

(let [in-error false]
  (awesome.connect_signal
   "debug::error"
   (fn [err]
       (if in-error
           nil
         (let [in-error true]
           (naughty.notify {:preset naughty.config.presets.critical
                            :title "Oops, an error happened!"
                            :text (tostring err)}))))))
;;;; Variable Declarations
;; Themes define colours, icons, font and wallpapers
(beautiful.init (.. (gears.filesystem.get_themes_dir) "default/theme.lua"))

;; This is used later as the default terminal and editor to run.
(local terminal   :kitty)
(local editor     (or (os.getenv :EDITOR) :emacs))
(local editor-cmd (.. terminal " -e " editor))

;; Default modkey.
;; Usually, Mod4 is the key with a logo between Control and Alt.
;; If you do not like this or do not have such a key,
;; I suggest you to remap Mod4 to another key using xmodmap or other tools.
;; However, you can use another modifier like Mod1, but it may interact with others.
(local modkey :Mod4)

;; Table of layouts to cover with awful.layout.inc, order matters.
(set awful.layout.layouts [;; awful.layout.suit.floating
                           awful.layout.suit.tile
                           awful.layout.suit.tile.left
                           awful.layout.suit.tile.bottom
                           awful.layout.suit.tile.top
                           awful.layout.suit.fair
                           awful.layout.suit.fair.horizontal
                           awful.layout.suit.spiral
                           awful.layout.suit.spiral.dwindle
                           awful.layout.suit.max
                           awful.layout.suit.max.fullscreen
                           awful.layout.suit.magnifier
                           awful.layout.suit.corner.nw])

(set beautiful.useless_gap 10)
;;;; Menu
;; Create a launcher widget and a main menu
(local myawesomemenu
  [["hotkeys"     (fn [] (hotkeys_popup.show_help nil (awful.screen.focused)))]
   ["manual"      (.. terminal " -e man awesome")]
   ["edit config" (.. editor-cmd " " awesome.conffile)]
   ["restart"     awesome.restart]
   ["quit"        (fn [] (awesome.quit))]])

(local mymainmenu
  (awful.menu
    {:items [["awesome"       myawesomemenu beautiful.awesome_icon]
             ["open terminal" terminal]]}))

(local mylauncher
  (awful.widget.launcher
    {:image beautiful.awesome_icon
     :menu  mymainmenu}))

;; Menubar configuration
(set menubar.utils.terminal terminal) ;; Set the terminal for applications that require it
;;;; Wibar
;; Keyboard map indicator and switcher
(local mykeyboardlayout (awful.widget.keyboardlayout))

;; Create a textclock widget
(local mytextclock (wibox.widget.textclock))

(local taglist-buttons
  (gears.table.join
    (awful.button [] 1 (fn [tag] (tag:view_only)))
    (awful.button [modkey] 1 (fn [tag] (when client.focus (client.focus:move_to_tag tag))))
    (awful.button [] 3 awful.tag.viewtoggle)
    (awful.button [modkey] 3 (fn [tag] (when client.focus (client.focus:toggle_tag tag))))
    (awful.button [] 4 (fn [tag] (awful.tag.viewnext (tag.screen))))
    (awful.button [] 5 (fn [tag] (awful.tag.viewprev (tag.screen))))))

(local tasklist-buttons
  (gears.table.join
    (awful.button [] 1 (fn [c] (if (= c client.focus) (set c.minimized true) (c:emit_signal "request::activate" "tasklist" {:raise true}))))
    (awful.button [] 3 (fn [] (awful.menu.client_list {:theme {:width 250}})))
    (awful.button [] 4 (fn [] (awful.client.focus.byidx 1)))
    (awful.button [] 5 (fn [] (awful.client.focus.byidx -1)))))

(fn set-wallpaper [s]
  ;; Wallpaper
  (when beautiful.wallpaper
    (var wallpaper beautiful.wallpaper)
    ;; If wallpaper is a function, call it with the screen
    (when (= (type wallpaper) "function")
      (set wallpaper (wallpaper s)))
    (gears.wallpaper.maximized wallpaper s true)))

;; -- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
;; (screen.connect_signal "property::geometry" set-wallpaper)

(fn setup-screen [screen]
  ;; (set-wallpaper screen)
  (awful.tag ["1" "2" "3" "4" "5" "6" "7" "8" "9"] screen (. awful.layout.layouts 1))
  ;; Create a promptbox for each screen
  (set screen.mypromptbox (awful.widget.prompt))
  (set screen.mylayoutbox (awful.widget.layoutbox screen))
  (screen.mylayoutbox:buttons (gears.table.join
                               (awful.button [] 1 (fn [] (awful.layout.inc 1 screen awful.layout.layouts)))
                               (awful.button [] 3 (fn [] (awful.layout.inc -1 screen)))
                               (awful.button [] 4 (fn [] (awful.layout.inc 1 screen)))
                               (awful.button [] 5 (fn [] (awful.layout.inc -1 screen)))))

  (set screen.mytaglist (awful.widget.taglist {:screen screen
                                               :filter awful.widget.taglist.filter.all
                                               :buttons taglist-buttons}))
  (set screen.mytasklist (awful.widget.tasklist {:screen screen
                                                 :filter awful.widget.tasklist.filter.currenttags
                                                 :buttons tasklist-buttons}))

  (set screen.mywibox (awful.wibar {:position "top" :screen screen}))
  (screen.mywibox:setup {:layout wibox.layout.align.horizontal
                         1 {:layout wibox.layout.fixed.horizontal
                            1 mylauncher
                            2 screen.mytaglist
                            3 screen.mypromptbox}
                         2 screen.mytasklist
                         3 {:layout wibox.layout.fixed.horizontal
                            1 mykeyboardlayout
                            2 (wibox.widget.systray)
                            3 mytextclock
                            4 screen.mylayoutbox}}))

(awful.screen.connect_for_each_screen setup-screen)
;;;; Keybindings
(root.buttons (gears.table.join
               (awful.button [ ] 3 (fn [] (: mymainmenu :toggle)))
               (awful.button [ ] 4 awful.tag.viewnext)
               (awful.button [ ] 5 awful.tag.viewprev)))

(var globalkeys [])

(fn global-key [modifiers key action description]
  (local newkey (awful.key modifiers key action description))
  (set globalkeys (gears.table.join globalkeys newkey)))
;;;;; Launch Applications
;; These are some shortcuts for launching my most common applications.
(global-key [modkey] "n" (fn [] (awful.spawn "nyxt"))        {:description "Launch Nyxt"        :group "applications"})
;; I have had much trouble with firefox.  It is the one application that is
;; consistently causing me performance issues.  Therefore I am trying to move
;; towards qutebrowser (or nyxt if I can help it) and falkon.
(global-key [modkey] "i" (fn [] (awful.spawn "qutebrowser")) {:description "Launch Qutebrowser" :group "applications"})
(global-key [modkey] "t" (fn [] (awful.spawn terminal))      {:description "Launch Alacritty"   :group "applications"})
(global-key [modkey] "e" (fn [] (awful.spawn "emacs"))       {:description "Launch Emacs"       :group "applications"})
;;;;; Rotate Clients
(fn rotate-clients-forward []
  (awful.client.cycle true)
  (local master (awful.client.getmaster))
  (when master
    (set client.focus master)
    (master:raise)))

(fn rotate-clients-backward []
  (awful.client.cycle false)
  (local master (awful.client.getmaster))
  (when master
    (set client.focus master)
    (master:raise)))

(global-key [modkey] "j" rotate-clients-forward  {:description "Rotate Forward"  :group "client"})
(global-key [modkey] "k" rotate-clients-backward {:description "Rotate Backward" :group "client"})
;;;;; Quitting and Restarting Awesome
(global-key [modkey "Control"] "r" awesome.restart {:description "Restart" :group "awesome"})
(global-key [modkey "Shift"]   "q" awesome.quit    {:description "Quit"    :group "awesome"})
;;;;; Layout Manipulation
(global-key [modkey]         "space" (fn [] (awful.layout.inc  1)) {:description "next layout" :group "layout"})
(global-key [modkey "Shift"] "space" (fn [] (awful.layout.inc -1)) {:description "previous layout" :group "layout"})

(global-key [modkey "Shift"] "j" (fn [] (awful.client.swap.byidx  1)) {:description "Swap Forward" :group "client"})
(global-key [modkey "Shift"] "k" (fn [] (awful.client.swap.byidx -1)) {:description "Swap Back" :group "client"})

(global-key [modkey] "l" (fn [] (awful.tag.incmwfact  0.05)) {:description "increase master width factor" :group "layout"})
(global-key [modkey] "h" (fn [] (awful.tag.incmwfact -0.05)) {:description "decrease master width factor" :group "layout"})

(global-key [modkey "Shift"] "l" (fn [] (awful.tag.incnmaster  1 nil true)) {:description "increase number of master" :group "layout"})
(global-key [modkey "Shift"] "h" (fn [] (awful.tag.incnmaster -1 nil true)) {:description "decrease number of master" :group "layout"})

(global-key [modkey "Control"] "l" (fn [] (awful.tag.incncol 1 nil true)) {:description "increase number of columns" :group "layout"})
(global-key [modkey "Control"] "h" (fn [] (awful.tag.incncol -1 nil true)) {:description "decrease number of columns" :group "layout"})
;;;;; Open Keybinding Menu
(global-key [modkey] "s" hotkeys_popup.show_help {:description "show help" :group "awesome"})
;;;;; Run an application
(global-key [modkey] "r" (fn [] (let [s (awful.screen.focused)] (s.mypromptbox:run))) {:description "run prompt" :group "launcher"})
;;;;; screenshot
;; Function to take a screenshot using maim.
(fn take-screenshot []
  (awful.spawn.with_shell "maim ~/Pictures/screenshot_$(date +%Y%m%d_%H%M%S).png")
  (naughty.notify {:title "foo" :text "what is this" :timeout 5}))

(global-key [modkey] "p" take-screenshot
            {:description "Take Screenshot" :group "screenshot"})
;;;;; Set Global Keys
(root.keys globalkeys)
;;;;; Manipulate Tags
;; (fn view-tag-only []
;;   (local screen (awful.screen.focused))
;;   (local tag (. screen.tags i))
;;   (when tag (tag:view_only)))

;; (fn toggle-tag-display []
;;   (local screen (awful.screen.focused))
;;   (local tag (. screen.tags i))
;;   (when tag (awful.tag.viewtoggle tag)))

;; (fn move-client-to-tag []
;;   (when client.focus
;;     (local tag (client.focus.screen))
;;     (when tag
;;       (client.focus.move))))

;; (fn toggle-tag-on-focused-client []
;;   (when client.focus
;;     (let [tag (client.fc)]
;;       (client.focus:toggle_tag))))

;; ;; The original bindings for moving to a workspace were crazy long.  Pressing
;; ;; MOD, the pound key and a number seems excessive.
;; (for [i 1 9]
;;   (global-key [modkey]                   (.. "#" i) view-tag-only                 {:description (.. "view tag #" i)    :group "tag" })
;;   (global-key [modkey "Shift"]           (.. "#" i) toggle-tag-display            {:description (.. "toggle tag #" i)  :group "tag"})
;;   (global-key [modkey "Control"]         (.. "#" i) move-client-to-tag            {:description (.. "move to tag #" i) :group "tag"})
;;   (global-key [modkey "Control" "Shift"] (.. "#" i) toggle-tag-on-focused-client  {:description (.. "toggle tag #" i)  :group "tag"}))
;;;;; Client Buttons
(fn mouse-click [c]
  (c:emit_signal "request::activate" "mouse_click" {:raise true}))

(fn mouse-click-move [c]
  (mouse-click c)
  (awful.mouse.client.move c))

(fn mouse-click-resize [c]
  (mouse-click c)
  (awful.mouse.client.resize c))

(local clientbuttons (gears.table.join (awful.button [] 1 mouse-click)
                                       (awful.button [modkey] 1 mouse-click-move)
                                       (awful.button [modkey] 3 mouse-click-resize)))
;;;;; Client Keys
(var clientkeys [])

(fn client-key [modifiers key action description]
  (local newkey (awful.key modifiers key action description))
  (set clientkeys (gears.table.join clientkeys newkey)))

(client-key [modkey] "f" (fn [c] (do (tset c :fullscreen (not c.fullscreen)) (c:raise))))
(client-key [modkey] "w" (fn [c] (c:kill)) {:description "quit" :group "client"})
(client-key [modkey] "q" (fn [c] (c:kill)) {:description "quit" :group "client"})
;;;; Rules
(set awful.rules.rules [{:rule []
                         :properties {:border_width beautiful.border_width
                                      :border_color beautiful.border_normal
                                      :focus awful.client.focus.filter
                                      :raise true
                                      :keys clientkeys
                                      :buttons clientbuttons
                                      :screen awful.screen.preferred
                                      :placement (+ awful.placement.no_overlap awful.placement.no_offscreen)
                                      }}
                        {:rule_any {:instance ["DTA" "copyq" "pinentry"]
                                    :class ["Arandr"
                                            "Blueman-manager"
                                            "Gpick"
                                            "Kruler"
                                            "MessageWin"
                                            "Sxiv"
                                            "Tor Browser"
                                            "Wpa_gui"
                                            "veromix"
                                            "xtightvncviewer"]
                                    :name "Event Tester"
                                    :role ["AlarmWindow"
                                           "ConfigManager"
                                           "pop-up"]}
                         :properties {:floating true}}
                        {:rule_any {:type ["normal" "dialog"]}
                         :properties {:titlebars_enabled true}}])
;;;; Signals
;; Signals are like hooks.  They are a way to run functions when an event
;; happens.
;;;;; Check whether signals are connected
;; ;; There does not seem to be a built in way to check whether a signal is
;; ;; connected.  I have to keep track of the signals I add and remove myself.
;; (local signal-connections {})

;; ;; I make my own wrappers around =client.connect_signal= and.
;; (fn connect_signal [signal-name signal-fn]
;;   "Connect SIGNAL-FN to SIGNAL-NAME."
;;   ;; function connect_client_signal(signal_name, func)
;;   ;; if not signal_connections[signal_name] then
;;   ;; signal_connections[signal_name] = {}
;;   ;; end
;;   ;; table.insert(signal_connections[signal_name], func)
;;   ;; client.connect_signal(signal_name, func)
;;   ;; end
;;   (table.insert signal-connections[signal-name] signal-fn)
;;   (client.connect_signal))

;; (fn disconnect-signal [signal-name signal-fn]
;;   "Disconnect SIGNAL-FN from SIGNAL-NAME."
;;   ;; (if (table.contains ))
;;   (table.remove signal-connections[signal-name] signal-fn)
;;   (client.disconnect_signal))

;; (fn signal-connected? [signal-name signal-fn]
;;   "Return non-nil if")
;;;;; Titlebars
;; (fn add-titlebars-maybe [c]
;;   ;; Do not know where the =c= comes from here.
;;   (local buttons [(awful.button [] 1 (fn []
;;                                        (c:emit_signal "request::activate" "titlebar" {:raise true})
;;                                        (awful.mouse.client.move c)))

;;                   (awful.button [] 3 (fn []
;;                                        (c:emit_signal "request::activate" "titlebar" {:raise true})
;;                                        (awful.mouse.client.resize c)))])
;;   (local titlebar (awful.titlebar c))
;;   (titlebar:setup {;; left
;;                    1 {1 (awful.titlebar.widget.iconwidget c)
;;                       :buttons buttons
;;                       :layout wibox.layout.fixed.horizontal}
;;                    ;; middle
;;                    2 {1 {:align "center"
;;                          :widget (awful.titlebar.widget.titlewidget c)}
;;                       :buttons buttons
;;                       :layout wibox.layout.flex.horizontal}
;;                    ;; right
;;                    3 {1 (awful.titlebar.widget.floatingbutton c)
;;                       2 (awful.titlebar.widget.maximizedbutton c)
;;                       3 (awful.titlebar.widget.stickybutton c)
;;                       4 (awful.titlebar.widget.ontopbutton c)
;;                       5 (awful.titlebar.widget.closebutton c)
;;                       :layout (wibox.layout.fixed.horizontal)}
;;                    :layout wibox.layout.align.horizontal}))

;; (client.connect_signal :request::titlebars add-titlebars-maybe)
;;;;; When a new client window appears, make it a slave
;; Is executed when new client appears.  It is like an emacs hook.
(fn ensure-clients-are-accessible [c]
  (when (and awesome.startup
             (not c.size_hints.user_position)
             (not c.size_hints.program_position))
    (awful.placement.no_offscreen c)))

(client.connect_signal "manage" ensure-clients-are-accessible)
;;;;; When mouse is in a window, give it focus
(client.connect_signal "mouse::enter" (fn [c] (c:emit_signal :request::activate "mouse_enter" {:raise false})))
;;;;; When a window is focused, change the border color
(client.connect_signal "focus" (fn [c] (set c.border_color beautiful.border_focus)))
(client.connect_signal "unfocus" (fn [c] (set c.border_color beautiful.border_normal)))

;; (fn toggle-titlebars []
;;   (if (signal-connected? :request::titlebars add-titlebars-maybe)
;;       (disconnect-signal :request::titlebars))
;;   (client.disconnect_signal :request::titlebars add-titlebars-maybe))

;; (global-key [modkey] "p" toggle-titlebars {:description "Toggle Titlebars" :group "client"})
