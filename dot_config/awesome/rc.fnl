;;; Fennel config
;; I prefer lisp over lua's syntax and I would like the ability to have macros.

;; Mostly I was inspired and adapted much of this file from samuelt wallace's post.
;; https://samueltwallace.github.io/awesomewm/
;;;; Requirements
(local gears (require :gears))
(require "gears.surface")
(local awful (require :awful))
(require "awful.autofocus")
(local wibox (require :wibox))
(local beautiful (require :beautiful))
(local naughty (require :naughty))
(local menubar (require :menubar))
;;;; Startup errors
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
                            :text (tostring err) }))))))
;;;; variables
;; This is also from the post.  I was going to remove it but hey its certainly
;; an interesting idea.
;; (local terminal "emacsclient -e '(shell)'")
(local terminal "alacritty")

;; When I add support for emacs I will re-add this.
;; (local editor "emacsclient")
(local editor "emacs")

(local editor-cmd "emacsclient")

(local modkey "Mod4")

(local fnlconf "~/.config/awesome/rc.fnl")
;;;; layouts
;; (fn layout [] (tset awful :layout :layouts))
(tset awful :layout :layouts [
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

;; (local menu [["quit" awesome.quit]
;;              ["restart" awesome.restart]
;;              ["edit config" (.. editor-cmd " " fnlconf)]
;;              ["Emacs" (fn [] (awful.spawn "emacs"))]
;;              ["Firefox" (fn [] (awful.spawn "firefox"))]
;;              ["Zathura" (fn [] (awful.spawn "zathura"))]
;;              ["Discord" (fn [] (awful.spawn "discord"))]])
;;;; Define keybindings
(fn global_key []
  (local newkey (awful.key modifiers key action description))
  (var globalkeys (gears.table.join globalkeys newkey)))

(fn client_key []
  (local )
  (var ))
;;;; Layout bindings
