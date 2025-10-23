;; Disable package.el; let straight.el handle packages.
(setq package-enable-at-startup nil
      package-quickstart nil)

;; Add .org-id location so it does not use .emacs.d 
(setq org-id-locations-file (expand-file-name ".org-id-locations" user-emacs-directory)
      org-id-track-globally t
      org-id-locations-file-relative t)
