(setq my/env "emacs-prod")
(setq server-name "emacs-prod")

;; Use the pre-defined noweb var (format "\"%s\"" "~/.config/emacs-common").
;; Falls back gracefully if the file isn't present.
(let* ((base "~/.config/emacs-common")
       (file (expand-file-name "api-keys.el" base)))
  (when (file-readable-p file)
    (load file nil 'nomessage)))

(require 'server)
(unless (server-running-p)
   (server-start))

;; Turn of eval protection
(setq org-confirm-babel-evaluate nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)
