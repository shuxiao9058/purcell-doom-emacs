;; init-const.el --- Desfine constants.	-*- lexical-binding: t -*-
;;
;;; Code:

(defconst sys/windowsp
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/win32p (and sys/windowsp
              (not (getenv "PROGRAMW6432")))
  "if current operation system is windows 32bit version")

(defconst sys/win64p (and sys/windowsp
              (getenv "PROGRAMW6432"))
  "if current operation system is windows 64bit verison.")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst doom-cache-dir
  (expand-file-name ".cache/" user-emacs-directory))

;; Hook(s)
(defvar doom-init-ui-hook nil
  "List of hooks to run when the theme and font is initialized (or reloaded with
`doom//reload-theme').")

(defvar doom-font nil
  "The default font to use. Expects a `font-spec'.")

(defvar doom-cn-font nil
  "The default chinese font to use..")

(defvar doom-big-font nil
  "The default large font to use when `doom-big-font-mode' is enabled. Expects a
`font-spec'.")

(defvar doom-variable-pitch-font nil
  "The default font to use for variable-pitch text. Expects a `font-spec'.")

(defvar doom-unicode-font nil
  "Fallback font for unicode glyphs. Is ignored if :feature unicode is active.
Expects a `font-spec'.");; color theme

(defvar doom-disabled-packages nil
    "disabled packages!")

(provide 'init-const)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
