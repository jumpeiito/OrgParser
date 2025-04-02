(defun inf-haskell-file-load ()
  (interactive)
  (set-buffer-and-send-input
   (lambda (bufname)
     (insert ":load " bufname))))

(defun set-buffer-and-send-input (f)
  (let ((haskell-buffer "*haskell*")
        (bufname (buffer-file-name (current-buffer))))
     (if (equal major-mode 'haskell-mode)
         (progn
           (delete-other-windows)
           (split-window-vertically)
           (other-window 1)
           (if (buffer-live-p (get-buffer haskell-buffer))
               (progn
                 (switch-to-buffer haskell-buffer)
                 (goto-char (point-max))
                 (funcall f bufname)
                 (comint-send-input))
             (run-haskell))))))

(define-key haskell-mode-map "\C-ck" 'inf-haskell-file-load)
