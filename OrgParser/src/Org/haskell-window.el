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
                (funcall f))
            (run-haskell))))))

(defun inf-haskell-file-load ()
  (interactive)
  (let ((bufname (buffer-file-name (current-buffer))))
    (save-buffer)
    (set-buffer-and-send-input
     (lambda ()
       (insert ":load " bufname)
       (comint-send-input)))))

(defun inf-haskell-type-annotation ()
  (interactive "")
  (let ((target-string (word-at-point t)))
    (set-buffer-and-send-input
     (lambda ()
       (insert ":t " target-string)
       (comint-send-input)))
    (other-window 1)))

(defun inf-haskell-type-annotation2 ()
  (interactive)
  (let ((target-string (word-at-point t))
        (curbuf        (current-buffer)))
    (set-buffer "*haskell*")
    (goto-char (point-max))
    (insert ":t " target-string)
    (comint-send-input)
    (let* ((regend (1- (line-beginning-position)))
           (regbegin (progn
                      (comint-previous-prompt 1)
                      (1+ (line-end-position))))
           (annot (s-chomp
                   (buffer-substring-no-properties regbegin regend))))
      (message annot))))

(define-key haskell-mode-map "\C-ck" 'inf-haskell-file-load)
(define-key haskell-mode-map "\C-ct" 'inf-haskell-type-annotation)
