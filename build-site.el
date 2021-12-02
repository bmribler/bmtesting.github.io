;; Workaround for Emacs 26.2 and older and gnutls 3.6.x
;; Otherwise trouble w/ HTTPS and package repos
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Set the package installation directory so that packages aren't stored in the
;; ~/.emacs.d/elpa path.
(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install dependencies
(package-install 'htmlize)

(require 'org)

;; Download the PlantUML JAR
(require 'url)

(setq plantuml-jar-path (expand-file-name ".packages/plantuml.jar"))
(setq plantuml-default-exec-mode 'jar)
(setq org-plantuml-jar-path plantuml-jar-path)
(unless (file-exists-p plantuml-jar-path)
  (url-copy-file "https://sourceforge.net/projects/plantuml/files/1.2021.12/plantuml.1.2021.12.jar/download"
		 ".packages/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(package-install 'plantuml-mode)
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
(setq org-confirm-babel-evaluate nil)

;; Load the publishing system
(require 'ox-publish)

;; Customize the HTML output
(setq org-html-validation-link nil            ;; Don't show validation link
      org-html-head-include-scripts nil       ;; Use our own scripts
      org-html-head-include-default-style nil ;; Use our own styles
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")

;; Define the publishing project
(setq org-publish-project-alist
      (list
       (list "org-site:main"
             :recursive t
             :base-directory "."
             :publishing-function 'org-html-publish-to-html
             :publishing-directory "."
             :creator "Copyright (C) 2021 The HDF Group"
             :with-author nil           ;; Don't include author name
             :with-creator t            ;; Include Emacs and Org versions in footer
             :with-toc nil              ;; Include a table of contents
             :section-numbers nil       ;; Don't include section numbers
             :time-stamp-file nil)))    ;; Don't include time stamp in file

;; Generate the site output
(org-publish-all t)

(message "Build complete!")
