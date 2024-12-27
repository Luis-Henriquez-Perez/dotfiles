;; Filename: prune_trash.hy
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Created: 2024-12-18 04:51:21
;; Description: Prune the trash.

(import os)
(:from pathlib import path)

(fn get_size ()
  (if (is-file path)
      (return)))

(fn clean_trash ())
