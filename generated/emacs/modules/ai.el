;; -*- lexical-binding: t; -*-

(require 'straight) ;; ensure available

(defun my/open-chatgpt-shell ()
  "Install/load chatgpt-shell on demand, then start it."
  (interactive)
  (straight-use-package
   '(chatgpt-shell :type git :host github :repo "xenodium/chatgpt-shell"))
  (require 'chatgpt-shell)
  ;; your keys/vars (safe to set again)
  (setq chatgpt-shell-openai-key    my-openai-api-key
        chatgpt-shell-anthropic-key my-anthropic-api-key
        chatgpt-shell-google-key    my-gemini-api-key
        chatgpt-shell-save-session  t)
  (call-interactively #'chatgpt-shell))

(global-set-key (kbd "C-c g") #'my/open-chatgpt-shell)

(defun my/ollama-buddy-chat ()
  "Install/load ollama-buddy on demand, then open chat."
  (interactive)
  (straight-use-package
   '(ollama-buddy :type git :host github :repo "n20o/ollama-buddy"))
  (require 'ollama-buddy)
  (call-interactively #'ollama-buddy-chat))
