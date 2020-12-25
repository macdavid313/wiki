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
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (pkg '(org-roam))
  (package-install pkg))

;;; Don't create backup files (those ending with ~) during the publish process.
(setq make-backup-files nil)

(defvar project-dir (getenv "PROJECT_DIR"))
(defvar publish-dir (concat project-dir "/public_html"))
(setq org-roam-directory (concat project-dir "/org"))
(defvar publish-url "https://macdavid313.xyz/wiki")
; -- End of set up

(require 'ox-publish)
(require 'ox-html)
(require 'org-roam)
;; (require 's)
;; (require 'htmlize)

(defvar site-preamble "")

(defvar site-postamble "<script src=\"//unpkg.com/heti/umd/heti-addon.min.js\"></script>
<script>
  document.getElementById(\"content\").classList.add(\"heti\");
  document.getElementById(\"content\").classList.add(\"heti--classic\");
  const heti = new Heti('.heti');
  heti.autoSpacing(); // 自动进行中西文混排美化和标点挤压
</script>")

(defvar site-head-extra
  "<link rel=\"stylesheet\" href=\"//unpkg.com/heti/umd/heti.min.css\">
<link rel=\"stylesheet\" href=\"/wiki/static/css/style.css\">")

(setq org-publish-project-alist
      `(("html"
         :base-directory ,(concat project-dir "/org")
         :base-extension "org"
         :publishing-directory ,publish-dir
         :publishing-function org-html-publish-to-html
         :recursive t
         :headline-levels 4
         :with-toc t
         :html-doctype "html5"
         :html-html5-fancy t
         :html-preamble ,site-preamble
         :html-postamble ,site-postamble
         :html-head-include-scripts nil
         :html-head-include-default-style nil
         :html-head-extra ,site-head-extra
         :html-link-use-abs-url t
         ;; :html-container "section"
         :htmlized-source nil)
         ;; :auto-sitemap t)
         ;; :exclude "node_modules"
         ;; :sitemap-title "Recent changes"
         ;; :sitemap-sort-files anti-chronologically
         ;; :sitemap-format-entry commonplace/sitemap-format-entry
         ;; :sitemap-filename "recentchanges.org")
        ("assets"
         :base-directory ,(concat project-dir "/static")
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|svg\\|json\\|pdf"
         :publishing-directory ,publish-dir
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

(defun wiki/publish ()
  "Publish the wiki site."
  (org-publish-all))

(provide 'publish)
;;; publish.el ends here
