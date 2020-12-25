;;; publish.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Tianyu Gu
;;
;; Author: Tianyu Gu <http://github/macdavid313>
;; Maintainer: Tianyu Gu <macdavid313@gmail.com>
;; Created: December 25, 2020
;; Modified: December 25, 2020
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;; Script for building static HTML files
;;; Code:

;; install deps
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg '(org-roam))
  (package-install pkg))
;; -- End of set up

(require 'ox-publish)
(require 'ox-html)
(require 'org-roam)
;; (require 's)
;; (require 'htmlize)

;; Don't create backup files (those ending with ~) during the publish process.
(setq make-backup-files nil)

(defvar project-dir (file-name-directory (buffer-file-name)))
(defvar publish-dir (concat project-dir "public_html/"))
(setq org-roam-directory (concat project-dir "org/"))
(defvar publish-url "https://macdavid313.xyz/wiki")

(setq org-publish-project-alist
      `(("site"
         :base-directory ,*project-dir*
         :base-extension "org"
         :publishing-directory ,*publish-dir*
         :publishing-function org-html-publish-to-html
         :recursive t
         :headline-levels 4
         :with-toc t
         :html-doctype "html5"
         :html-html5-fancy t)
         ;; :auto-sitemap t)
         ;; :exclude "node_modules"
         ;; :sitemap-title "Recent changes"
         ;; :sitemap-sort-files anti-chronologically
         ;; :sitemap-format-entry commonplace/sitemap-format-entry
         ;;:sitemap-filename "recentchanges.org")
        ("static"
         :base-directory ,project-dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|svg\\|json\\|pdf"
         :publishing-directory publish-dir
         ;; :exclude "node_modules"
         :recursive t
         :publishing-function org-publish-attachment)))

(defun org-roam-title-to-slug (title)
  "Convert TITLE to a filename-suitable slug.  Use hyphens rather than underscores."
  (cl-flet* ((nonspacing-mark-p (char)
                                (eq 'Mn (get-char-code-property char 'general-category)))
             (strip-nonspacing-marks (s)
                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                 (ucs-normalize-NFD-string s))))
             (cl-replace (title pair)
                         (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-")  ;; convert anything not alphanumeric
                    ("__*" . "-")  ;; remove sequential underscores
                    ("^_" . "")  ;; remove starting underscore
                    ("_$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
      (downcase slug))))

(setq org-roam-title-to-slug-function 'org-roam-title-to-slug)

(provide 'publish)
;;; publish.el ends here
