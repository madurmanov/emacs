;;; init-key-chord.el -- Initialize key chord
;;; Commentary:
;;; Code:
(require 'key-chord)
(require 'evil)

(key-chord-mode t)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

(provide 'init-key-chord)
;;; init-key-chord.el ends here
