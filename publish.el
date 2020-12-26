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
(defvar publish-url "https://macdavid313.xyz/wiki")

(setq org-roam-directory (concat project-dir "/org"))
(setq org-roam-db-location (concat org-roam-directory "/org-roam.db"))
; -- End of set up

(require 'ox-publish)
(require 'ox-html)
(require 'org-roam)
;; (require 's)
;; (require 'htmlize)

;;; Utilities
(defun collect-all-org-files-titles ()
  "Traverse all org-roam files, sort by titles and return filenames and titles as pairs in an alist."
  (cl-sort
    (mapcar (lambda (f)
              (let ((fname (concat (file-name-base f) ".org"))
                    (title (with-current-buffer (find-file-read-only f)
                             (let ((res (car (org-roam--extract-titles-title))))
                               (kill-buffer-if-not-modified (current-buffer))
                               res))))
                (cons fname title)))
            (org-roam--list-all-files))
    #'string-lessp
    :key #'cdr))

(defun append-full-index ()
  "Append all org files to index.org in a 'Full Index' section."
  (with-temp-buffer
    (insert "\n* Full Index\n\n")
    (dolist (fname-title (collect-all-org-files-titles))
      (when (not (string-equal (car fname-title) "index.org"))
        (insert (format "- [[file:%s][%s]]\n"
                        (car fname-title)
                        (cdr fname-title)))))
    (append-to-file (point-min) (point-max)
                    (concat org-roam-directory "/index.org"))
    (kill-buffer (current-buffer))))

(defun prepare-publish (prop)
  (progn
    (append-full-index)))

;;; Configurations for publishing
(defvar site-preamble "")

(defvar site-postamble "")

(defvar site-head-extra
  "<link rel=\"shortcut icon\" href=\"/wiki/static/img/favicon.ico\">
<link rel=\"preconnect\" href=\"https://fonts.gstatic.com\">
<link href=\"https://fonts.googleapis.com/css2?family=Fira+Code&family=JetBrains+Mono&display=swap\" rel=\"stylesheet\">
<link rel=\"stylesheet\" href=\"/wiki/static/css/style.css\">")

(setq org-publish-project-alist
      `(("all" :components ("html" "assets"))
        ("html"
         :base-directory ,(concat project-dir "/org")
         :base-extension "org"
         :publishing-directory ,publish-dir
         :preparation-function prepare-publish
         :publishing-function org-html-publish-to-html
         :recursive t
         :headline-levels 4
         :with-toc t
         :html-doctype "html5"
         :html-html5-fancy t
         :html-preamble ,site-preamble
         :html-postamble ,site-postamble
         :html-head-include-scripts t
         :html-head-include-default-style t
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
         :base-extension any
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
  (org-publish "all"))

(provide 'publish)
;;; publish.el ends here
