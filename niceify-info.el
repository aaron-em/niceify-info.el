;; this is actually shaping up pretty nicely
;; DONE fontify elisp examples (from 2 indents followed by `(' until matching `)')
;;  - but some elisp examples start with ; (comments)
;;  - and not all manuals follow the same indent convention;
;;    about all that can be relied upon is that code examples are
;;    indented further than paragraphs
;; DONE fontify bold *...* and italic _..._
;; CANT fontify keybindings with links to describe-key
;;  - no (obvious?) way to know which keymap corresponds with which manual
;;  - also custom bindings may differ from those described
;; TODO MAYBE make reversible
;; TODO what about autoloads not currently loaded
;; TODO make distributable

(defun niceify-info nil
  "Apply niceification functions to Info buffers."
  (let ((inhibit-ro-prev-value inhibit-read-only))
    (unwind-protect
         (progn
           (niceify-info-emphasis)
           (niceify-info-headers)
           (niceify-info-refs)
           (niceify-info-code-samples))
      (set-buffer-modified-p nil)
      (setq inhibit-read-only inhibit-ro-prev-value))))

(defvar niceify-info-map (make-sparse-keymap)
  "Keymap applied to links created during niceification.")
(set-keymap-parent niceify-info-map Info-mode-map)
(define-key niceify-info-map [mouse-2]
  'niceify-follow-link)
(define-key niceify-info-map (kbd "RET")
  'niceify-follow-link)
(define-key niceify-info-map [follow-link]
  'mouse-face)

(defun niceify-info-code-samples nil
  "Attempt to fontify Emacs Lisp code samples."
  (let ((paragraph-indent-depth 0)
        possible-sample-regex
        sample-start-regex
        sample-start sample-end sample-content sample-fontified)
    (save-excursion
      (save-match-data
        (beginning-of-buffer)
        (while (not (looking-at " +"))
          (next-line)
          (beginning-of-line))
        (while (looking-at " ")
          (incf paragraph-indent-depth)
          (forward-char 1))
        (setq possible-sample-regex
              (concat "^ \\{"
                      (number-to-string (1+ paragraph-indent-depth))
                      ",\\}"))
        (setq sample-start-regex (concat possible-sample-regex "[(;]"))
        (beginning-of-buffer)
        (while (not (eobp))
          (next-line)
          (beginning-of-line)
          (cond
            ((and (null sample-start)
                  (looking-at sample-start-regex))
             (progn
               (setq sample-start (point))))
            ((and (not (null sample-start))
                  (not (looking-at possible-sample-regex)))
             (progn
               (setq sample-end (point))
               (setq sample-content
                     (buffer-substring-no-properties sample-start sample-end))
               (with-temp-buffer
                 (insert sample-content)
                 (emacs-lisp-mode)
                 (font-lock-fontify-buffer)
                 (setq sample-fontified
                       (buffer-substring (point-min) (point-max))))
               (goto-char sample-start)
               (delete-region sample-start sample-end)
               (insert sample-fontified)
               (goto-char sample-end)
               (setq sample-start nil)
               (setq sample-end nil)))))))))

(defun niceify-info-emphasis nil
  "Fontify *bold* and _underlined_ emphases."
  (let ((face-map '(("_" . italic)
                    ("*" . bold)))
        emphasis-char)
    (save-match-data
      (save-excursion
        (beginning-of-buffer)
        (while (re-search-forward "[	
 ]\\([\\_*]\\)\\([^	
 ].*?[^	
 ]\\)\\1\\(?:[	
 ]\\|$\\)" nil t)
          (setq emphasis-char (match-string 1))
          (add-text-properties (match-beginning 2)
                               (match-end 2)
                               `(face ,(cdr (assoc emphasis-char face-map)))))))))

;; FIXME rename me
(defun niceify-follow-link nil
  "Follow a link produced by Info niceification."
  (interactive)
  (let ((niceify-link-props (get-text-property (point) 'niceify-link-props))
        type name fun)
    (cond
      ((null niceify-link-props)
       (message "Not on a niceified info link"))
      (t
       (setq type (plist-get niceify-link-props :type))
       (setq name (plist-get niceify-link-props :name))
       (setq fun (intern (concat "describe-" (symbol-name type))))
       (let ((help-window-select t))
         (funcall fun name))))))

(defun niceify-info-fontify-as-elisp (from to)
  "Fontify a region as Emacs Lisp source."
  (let ((content (buffer-substring-no-properties from to))
        fontified)
  (with-temp-buffer
    (insert content)
    (emacs-lisp-mode)
    (font-lock-fontify-buffer)
    (setq fontified (buffer-substring (point-min) (point-max))))
  (goto-char from)
  (delete-region from to)
  (insert fontified)))

(defun niceify-info-add-link (from to type name)
  "Niceify a reference.

Specifically, apply a set of text properties, over the range of
buffer positions between FROM and TO, which together constitute a
niceification link, and which cause the newly added link to
interoperate correctly with those added by Info-mode itself."
  (add-text-properties from to
                       (list 'face 'link
                             'link 't
                             'keymap niceify-info-map
                             'mouse-face 'highlight
                             'niceify-link-props (list :type type
                                                       :name name)
                             'help-echo (concat "mouse-1: visit documentation for this "
                                                (symbol-name type)))))

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
              (niceify-info-add-link from to type name)
              (niceify-info-fontify-as-elisp from to)))))))

(defun niceify-info-headers nil
  "Highlight function, variable, macro, etc. description headers
in Info with arbitrary faces."
  (let* ((args-face 'italic)
         (indent-spaces 5) ;; NB this may not be immutable, though seems so
         (further-indent-regex
          (concat " \\{" (number-to-string (* indent-spaces 2)) ",\\}"))
         type
         name
         (type-map '((command . function)
                     (user\ option . variable)
                     (function . function)
                     (variable . variable)
                     (constant . constant)
                     (const . constant)
                     (face . variable)
                     (hook . variable)
                     (macro . function)
                     (method . function)
                     (option . variable)))
         (face-map '((function . font-lock-function-name-face)
                     (variable . font-lock-variable-name-face)
                     (constant . font-lock-constant-face))))
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
            (backward-char 1)
            (setq to (point))
            (setq type
                  (cdr (assoc 
                        (intern (downcase (buffer-substring-no-properties from to)))
                        type-map)))
            (add-face-text-property from to
                                    (cdr (assoc type face-map)))

            (re-search-forward " " nil t)
            (setq from (point))
            (while (not (or (looking-at " ")
                            (eolp)))
              (forward-char 1))
            (setq to (point))
            (niceify-info-add-link from to
                                   type
                                   (intern (buffer-substring-no-properties from to)))

            (setq from (point))
            (end-of-line)

            (while (save-excursion
                     (forward-char 1)
                     (looking-at further-indent-regex))
              (forward-char 1)
              (end-of-line))
            
            (add-face-text-property from (point) args-face)))))))

(add-hook 'Info-selection-hook
          #'niceify-info)
