;;; init-lookup.el
;; Author: Haibo Wang <nasoundead@163.com>
;; Version: 0.0.1
;; URL: https://github.com/nasoundead/.emacs.d
;; Keywords:
;; Compatibility:
;; Reference:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Package configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;; "What am I looking at?" This module helps you answer this question.
;;
;;   + `+lookup/definition': a jump-to-definition that should 'just work'
;;   + `+lookup/references': find a symbol's references in the current project
;;   + `+lookup/file': open the file referenced at point
;;   + `+lookup/online'; look up a symbol on online resources
;;   + `+lookup/in-docsets': look up in Dash docsets
;;
;; This module uses `xref', an experimental new library in Emacs. It may change
;; in the future. When xref can't be depended on it will fall back to
;; `dumb-jump' to find what you want.

(defvar +lookup-provider-url-alist
  (append '(("Google"            . "https://google.com/search?q=%s")
            ("Google images"     . "https://www.google.com/images?q=%s")
            ("Google maps"       . "https://maps.google.com/maps?q=%s")
            ("Project Gutenberg" . "http://www.gutenberg.org/ebooks/search/?query=%s")
            ("DuckDuckGo"        . "https://duckduckgo.com/?q=%s")
            ("DevDocs.io"        . "https://devdocs.io/#q=%s")
            ("StackOverflow"     . "https://stackoverflow.com/search?q=%s")
            ("Github"            . "https://github.com/search?ref=simplesearch&q=%s")
            ("Youtube"           . "https://youtube.com/results?aq=f&oq=&search_query=%s")
            ("Wolfram alpha"     . "https://wolframalpha.com/input/?i=%s")
            ("Wikipedia"         . "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
			("Rust Docs"         . "https://doc.rust-lang.org/edition-guide/?search=%s")))
  "An alist that maps online resources to their search url or a function that
produces an url. Used by `+lookup/online'.")

(defvar +lookup-open-url-fn #'browse-url
  "Function to use to open search urls.")

(defvar +lookup-definition-functions
  '(+lookup-xref-definitions-backend-fn
    +lookup-dumb-jump-backend-fn
    +lookup-project-search-backend-fn
    +lookup-evil-goto-definition-backend-fn)
  "Functions for `+lookup/definition' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-references-functions
  '(+lookup-xref-references-backend-fn
    +lookup-project-search-backend-fn)
  "Functions for `+lookup/references' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-documentation-functions
  '(+lookup-online-backend-fn)
  "Functions for `+lookup/documentation' to try, before resorting to
`dumb-jump'. Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-file-functions ()
  "Function for `+lookup/file' to try, before restoring to `find-file-at-point'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")


;; Jump to definition via `ag'/`rg'/`grep'
(use-package dumb-jump
  :init (add-hook 'after-init-hook #'dumb-jump-mode)
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-default-project sea-emacs-dir
        dumb-jump-aggressive nil)
  (with-eval-after-load 'ivy
    (setq dumb-jump-selector 'ivy))
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))

;;
;;; xref

;; The lookup commands are superior, and will consult xref if there are no
;; better backends available.
(global-set-key [remap xref-find-definitions] #'+lookup/definition)
(global-set-key [remap xref-find-references]  #'+lookup/references)

(after! xref
  ;; We already have `projectile-find-tag' and `evil-jump-to-tag', no need for
  ;; xref to be one too.
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  ;; ...however, it breaks `projectile-find-tag', unless we put it back.
  (defadvice! +lookup--projectile-find-tag-a (orig-fn)
    :around #'projectile-find-tag
    (let ((xref-backend-functions '(etags--xref-backend t)))
      (funcall orig-fn)))

  ;; Use `better-jumper' instead of xref's marker stack
  (advice-add #'xref-push-marker-stack :around #'doom-set-jump-a)

  (use-package ivy-xref
    :config
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
    (set-popup-rule! "^\\*xref\\*$" :ignore t))
  )



(provide 'init-lookup)
;;; init-lookup ends here
