;; this is actually shaping up pretty nicely

(defun niceify-info nil
  "Apply niceification functions to Info buffers."
  (let ((inhibit-ro-prev-value inhibit-read-only))
    (unwind-protect
         (niceify-info-headers)
         (niceify-info-refs))
    (set-buffer-modified-p nil)
    (setq inhibit-read-only inhibit-ro-prev-value)))

(defvar niceify-info-map (make-sparse-keymap)
  "Keymap applied to links created during niceification.")
(set-keymap-parent niceify-info-map Info-mode-map)
(define-key niceify-info-map [mouse-2]
  'niceify-follow-link)
(define-key niceify-info-map (kbd "RET")
  'niceify-follow-link)
(define-key niceify-info-map [follow-link]
  'mouse-face)

(defun niceify-follow-link nil
  (interactive)
  (let ((niceify-link-data (get-text-property (point) 'niceify-link))
        type name fun)
    (cond
      ((null niceify-link-data)
       (message "Not on a niceified info link"))
      (t
       (setq type (plist-get niceify-link-data :type))
       (setq name (plist-get niceify-link-data :name))
       (setq fun (intern (concat "describe-" (symbol-name type))))
       (let ((help-window-select t))
         (funcall fun name))))))

(defun niceify-info-refs nil
  "Link backtick-and-quote references to the documentation of
things they reference."
  (save-match-data
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward "`\\(.*?\\)'" nil t)
        (backward-char 1)
        (let* ((name (intern (match-string 1)))
               (to (point))
               (from (- to (length (symbol-name name))))
               (type (cond
                       ((fboundp name) 'function)
                       ((boundp name) 'variable)
                       (t 'unknown))))
          (if (and (not (eq type 'unknown))
                   (not (eq name 'nil)))
              ;; abstract me over type, for use in niceifying headers
            (add-text-properties from to
                                 `(face link
                                        link t
                                        keymap ,niceify-info-map
                                        local-map ,niceify-info-map
                                        mouse-face highlight
                                        niceify-link ,(list :type type
                                                            :name name)
                                        help-echo ,(concat "mouse-1: visit help for this "
                                                           (symbol-name type)
                                                           " in other window")))))))))

(defun niceify-info-headers nil
  "Highlight function, variable, macro, etc. description headers
in Info with arbitrary faces."
  (let ((type-face 'italic)
        (name-face 'bold)
        (args-face 'italic)
        (what-it-was inhibit-read-only))
    (let (from to line-start)
      (setq inhibit-read-only t)
      (save-match-data
        (save-excursion
          (beginning-of-buffer)
          (while (re-search-forward "^ -- " nil t)
            (save-excursion
              (beginning-of-line)
              (setq line-start (point)))

            (setq from (point))
            (re-search-forward ":" nil t)
            (setq to (point))
            (add-face-text-property from to type-face)

            (re-search-forward " " nil t)
            (setq from (point))
            (re-search-forward " " nil t)
            (setq to (point))
            (add-face-text-property from to name-face)

            (setq from (point))
            (end-of-line)
            (add-face-text-property from (point) args-face)))))))

(add-hook 'Info-selection-hook
          #'niceify-info)
