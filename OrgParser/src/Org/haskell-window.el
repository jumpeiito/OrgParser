(defun set-buffer-and-send-input (f)
  (let ((haskell-buffer "*haskell*"))
    (if (equal major-mode 'haskell-mode)
        (progn
          (delete-other-windows)
          (split-window-right)
          (other-window 1)
          (if (buffer-live-p (get-buffer haskell-buffer))
              (progn
                (switch-to-buffer haskell-buffer)
                (goto-char (point-max))
                (funcall f)
                (comint-send-input))
            (run-haskell))))))

(defun inf-haskell-file-load ()
  (interactive)
  (let ((bufname (buffer-file-name (current-buffer))))
    (save-buffer)
    (set-buffer-and-send-input
     (lambda ()
       (insert ":load " bufname)))))

(defun inf-haskell-type-annotation ()
  (interactive "")
  (let ((target-string (word-at-point t)))
    (set-buffer-and-send-input
     (lambda ()
       (insert ":t " target-string)))
    (other-window 1)))

(define-key haskell-mode-map "\C-ck" 'inf-haskell-file-load)
(define-key haskell-mode-map "\C-ct" 'inf-haskell-type-annotation)
