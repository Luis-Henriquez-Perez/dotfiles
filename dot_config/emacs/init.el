;; -*- mode: emacs-lisp; lexical-binding: t; no-byte-compile: t -*-
;;; initialization
;; This file starts up everything else.

;; I had been using =org= to manage my configuration for a long time.  But
;; ultimately I switched to pure elisp because of several major reasons.  One is
;; that it is how emacs and programming files in general are designed to
;; operate.  The way files are tested assumes dividing code into multiple files.
;; And while I do not think that there is anything theoretically wrong with
;; having everything in a huge file, I think that the current system and
;; debugging tools are designed with the assumption that features are divided
;; into multiple files. Using =org= means that you will need to figure out
;; how to do these things indirectly.  I think the most natural way of using
;; =org= to manage configuration files is putting everything in one file because
;; headlines fulfill the role of segregation into manageable pieces so you don't
;; need files to do that.  And you should want to do that to avoid the
;; complexity that arises from loading order. Already I think its questionable
;; to use org if you're going to also try to maintain the file hierarchy. Using
;; outshine commenting style is very minimal and unobtrusive and therefore
;; easily transferable to other programming files.  One could argue that this is
;; how org should have been implemented as sort of adding features to an
;; existing thing.

;; My approach to writing code when using one big org file is to write functions
;; and macros that work regardless of whether a pertinent feature is loaded or
;; not.  For example.  I had to do that because the whole file was evaluated at
;; once; I did not delegate to different files as a typical configuration
;; normally would.  Now that I am using a multi-file elisp configuration, it is
;; possible to code less defensively because I can control when a file is
;; loaded.  Instead of having every form be safe as in with =set!= I could just
;; have them be normal and call the file only when its needed features are ready
;; to be loaded.

;; Another thing is that unfortunately without using org I can't have multiple
;; languages in the same file.  On the upside, there is the flexibility of not
;; being strictly tied to the headling/source block syntax.  For example in the
;; abbrev file, I can have multiple abbrevs without the source block syntax;
;; whereas in my org file there had to be a heading for each of them.  Also I
;; don't need a headline for every function.  I can do the same thing with =org=
;; and put multiple functions in one source block, or give a headline multiple
;; source blocks, but that kind of thing doesn't make sense to me.

;; Probably the saddest thing about not having org-mode is that none of my links
;; will work for people viewing the code file on github for example.

;; Should I distinguish between patches and configurations?  Concretely, should
;; I just turn =oo-outline-patch= into =oo-outline-configuration=?  I feel like
;; there can be a grey area between the two.  In general I do not think I should
;; make unnecessary categories between things and should try to let the demands
;; of the problem itself speak above my own prejudices.

;; An advantage of org-mode is you don't have to deal (directly at least) with a
;; file hierarchy.  And, searching your configurations is super easy as it
;; amounts to just searching one file.

;; I had been thinking of the problem with initializing when I was working on
;; easymotion.  So the issue is that many packages need code that is evaluated
;; immediately.  I had been thinking that with an org configuration it might be
;; easier because you can put all init code in one file, but you'd still have to
;; deal with loading the configuration code at the proper time.

;; So the way I will design this is I will have multiple init files for
;; packages.  I do not like having so many files for very little lines of code
;; but.

;; As much as I want to make org work I think that I was forcing it too much.
;; First, I know that the "normal" method works but I don't know what exactly
;; will be involved with org's method.  Furthermore, I just do not think there
;; are as many tools to deal with a single large file.

;; There is a general desire to keep package information together, but with
;; every package the information is split into two components--things that will
;; be evaluated immediately and things that will be evaluated later.

;; For organizational purposes there is a general desire to have everything
;; pertaining to a package in one place.  I think this is a big reason why
;; =use-package= is such a popular macro because it lets you do this in one
;; form by having different keywords that specify how things should be loaded.
;; With an org configuration, it is nice that I can tangle source blocks that
;; are right next to each other in the file, to different files.  In an elisp
;; configuration, I would rather keep things more "pure".

;;;; set initial variables
;;;;;  disable garbage collection until I'm done with startup
;; This variable controls how often.  Setting it to =most-positive-fixnum=, a very
;; big number, essentially disables garbage collection.  The garbage collection is
;; later reset to a reasonable value.
(setq gc-cons-threshold most-positive-fixnum)

;;;;;  determine whether emacs is in debug mode
;; This variable is snatched from [[https://github.com/hlissner/doom-emacs][Doom]].  The point of this variable is to serve as
;; an indicator of whether the current Emacs instance is run for
;; debugging. Normally, when starting Emacs I suppress non-critical errors so as
;; not to interfere with startup; but if I know I'm specifically debugging my
;; configuration using this variable then I might disable the suppression of
;; errors.
(defvar oo-debug-p (or (getenv "DEBUG") init-file-debug)
  "When non-nil print debug messages.
The --debug-init flag and setting the DEBUG envar will enable this at startup.")

;;;;;  store errors that occur in hooks and advices
;; This is influenced by the excellent stackoverflow on
;; [[https://emacs.stackexchange.com/questions/669/how-to-gracefully-handle-errors-in-init-file][how-to-gracefully-handle-errors-in-init-file]].  The idea is that I don't want
;; errors to interfere with startup or with my usage of Emacs.  But I also don't
;; want to ignore them altogether.  Instead, I want to raise them when I decide
;; to I want to deal with them; from the comfort of a working Emacs
;; configuration.
(defvar oo-errors nil
  "An alist of errors that occur in advices or hooks during Emacs startup.
Each element looks like (HOOK-OR-ADVICE . ERROR).  HOOK-OR-ADVICE is a hook or
an advice symbol that raised an error.  ERROR is the error object generated by
HOOK-OR-ADVICE.")

;;;;;  define a place to store temporary files
;; It's useful to store directories which I reference frequently in variables and
;; functions.  This way I can reference the full path.  Certain directories are
;; important; and I end up referencing them alot.  One of these is my
;; cache directory.
(defvar oo-cache-dir (concat user-emacs-directory "cache/")
  "Directory containing files used for caching information.")

;;;; add lisp directory to load-path
;; The load-path is a list of paths that emacs uses to find features it
;; can load.
(push (expand-file-name "lisp" user-emacs-directory) load-path)

;;;; load base settings
(require 'oo-base-settings)

;;;; install packages
;; The million dollar question is: how should installing packages work?  For me
;; I'd like to run a script in bash or preferably elisp that just installs my
;; packages for me up front and then at that point I can just run my emacs
;; configuration.  I just want a script that will install packages.

;; For now I am just trying to migrate from an org configuration to a pure elisp
;; one so =oo-base-packages= just sets up elpaca and uses it to install my
;; packages.
(require 'oo-base-packages)

;;;; (lazy) load patches and extensions
;; This registers the files into the load-path and sets up extensions for
;; loading.  It is important to put this before the base library specifically
;; because =oo-block-macro= uses a patch.
;; (require 'oo-base-extra)

;;;; load library
(require 'oo-base-utils)
(require 'oo-block-macro)
(require 'oo-call-after-load)
(require 'oo-call-after-keymap)
(require 'oo-call-after-evil-state)

;;;; setup packages for lazy loading
