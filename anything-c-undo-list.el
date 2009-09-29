;;; anything-c-undo-list.el --- anything config for `buffer-undo-list' and point-undo.el

;; Copyright (C) 2009 Takeshi Banse <takebi@laafc.net>
;; Author: Takeshi Banse <takebi@laafc.net>
;; Keywords: convenience, anything

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Some anything configurations for `buffer-undo-list' and poin-undo.el.
;; Exposes anything-c-buffer-undo-list-*, anything-c-point-undo-list-*,
;; anything-c-undo-list-* and acul-* symbols.

;;; Setup:
;;
;; (require 'anything-c-buffer-undo-list)
;; (define-key global-map "\C-c\C-o" 'anything-c-undo-list)

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-c-undo-list'
;;    Preconfigured `anything' for undo list.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-c-buffer-undo-list-examine-limit'
;;    *Number of the `buffer-undo-list' items to be examined by `anything-c-buffer-undo-list'.
;;    default = 150
;;  `anything-c-buffer-undo-list-collect-limit'
;;    *Number of items to be listed by `anything-c-buffer-undo-list'.
;;    default = most-positive-fixnum
;;  `anything-c-point-undo-list-examine-limit'
;;    *Number of the `point-undo-list' items to be examined by `anything-c-point-undo-list'.
;;    default = anything-c-buffer-undo-list-examine-limit
;;  `anything-c-point-undo-list-collect-limit'
;;    *Number of items to be listed by `anything-c-point-undo-list'.
;;    default = most-positive-fixnum

;;; Code:

(require 'anything)
(require 'anything-config)
(require 'cl)
(require 'point-undo)

(defun acul-memoize (f)
  (lexical-let ((f f) (cache (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (anything-aif (gethash args cache)
          it
        (puthash args (apply f args) cache)))))
(defun acul-compose (&rest fns)
  (if fns
    (lexical-let ((fn1 (car (last fns)))
                  (fns (butlast fns)))
      (lambda (&rest args)
        (reduce 'funcall fns
                :initial-value (apply fn1 args)
                :from-end t)))
    'identity))
(defun acul-display-to-real-line (candidate)
  ;; Changed anything-c-display-to-real-line's content regexp from '+' to '*'.
  (if (string-match "^ *\\([0-9]+\\):\\(.*\\)$" candidate)
    (list (string-to-number (match-string 1 candidate))
          (match-string 2 candidate))
    (error "Line number not found")))

;;; anything-c-source-buffer-undo-list-*
(defun* acul-collect-buffer-undo-list* (&optional (xs buffer-undo-list))
  (loop for x in xs
        with acc = nil
        with c = 0
        ;; (BEG . END)
        if (and (consp x) (integerp (car x)) (integerp (cdr x)))
        collect (car x) into acc and collect (cdr x) into acc
        ;; (TEXT . POSITION)
        else if (and (consp x) (stringp (car x)))
        collect (abs (cdr x)) into acc
        ;; (t HIGH . LOW);;
        ;; (nil PROPERTY VALUE BEG . END)
        else if (and (consp x) (null (car x)))
        collect (fourth x) into acc and collect (cdr (last x)) into acc
        ;; (apply FUN-NAME . ARGS);;
        ;; (apply DELTA BEG END FUN-NAME . ARGS);;
        ;; (MARKER . DISTANCE);;
        ;; POSITION;;
        finally return acc))
(defun acul-make-linum-of ()
  "Create memoized/safe version of `line-number-at-pos'"
  (acul-memoize (lambda (z) (line-number-at-pos (min z (point-max))))))
(defun* acul-make-point-line-filter2
    (delta &optional (posy-of 'identity) (posx-of 'identity))
  (lexical-let ((delta delta) (posy-of posy-of) (posx-of posx-of)
                (linum-of (acul-make-linum-of)))
    (lambda (x ys)
      (anything-aif (notany (lambda (y)
                              (let ((px (funcall posx-of x))
                                    (py (funcall posy-of y)))
                                (or (= px py)
                                    (< (abs (- (funcall linum-of px)
                                               (funcall linum-of py)))
                                       delta))))
                            ys)
          (values it (funcall (acul-compose linum-of posx-of) x))
        (values nil nil)))))
(defun* acul-collector2 (filter2 xs x &optional (adjust-point 'identity))
  (multiple-value-bind (result line)
      (funcall filter2 (funcall adjust-point x) xs)
    (if result
      (values (cons (cons x line) xs) t)
      (values xs nil))))
(defun acul-collect (xs collector2 limit)
  ;; Like (subseq (reduce collector2 xs :initial-value nil) 0 limit).
  ;; I want to limit the collector2's computations not the results.
  (nreverse (loop for x in xs
                  with c = 0
                  with acc = nil
                  when x
                  do (multiple-value-bind (value added)
                         (funcall collector2 acc x)
                       (setq acc value)
                       (when added (incf c)))
                  when (= limit c) return acc
                  finally return acc)))
(defun acul-candidates (xs collect collect-limit point-of &optional propertize)
  (loop for (x . l) in (acul-collect xs collect collect-limit)
        when (< (funcall point-of x) (point-max))
        collect (save-excursion
                  (goto-char (funcall point-of x))
                  (let ((s (format "%5d:%s" l (buffer-substring
                                               (line-beginning-position)
                                               (line-end-position)))))
                    (if propertize
                      (funcall propertize s x l (1+ 5) point-of)
                      s)))))

(defcustom anything-c-buffer-undo-list-examine-limit 150
  "*Number of the `buffer-undo-list' items to be examined by `anything-c-buffer-undo-list'."
  :type 'integer
  :group 'anything-config)
(defcustom anything-c-buffer-undo-list-collect-limit most-positive-fixnum
  "*Number of items to be listed by `anything-c-buffer-undo-list'."
  :type 'integer
  :group 'anything-config)
(defun anything-c-buffer-undo-list-candidates (&optional propertize)
  (with-current-buffer anything-current-buffer
    (apply 'acul-candidates
           (acul-collect-buffer-undo-list*
            (subseq buffer-undo-list
                    0 anything-c-buffer-undo-list-examine-limit))
           (apply-partially 'acul-collector2
                            (acul-make-point-line-filter2 1 'car))
           (max (or (anything-attr 'candidate-number-limit) 0)
                anything-c-buffer-undo-list-collect-limit)
           'identity
           (list propertize))))
(defun acul-source-base (name ass)
  (append ass
          `((name . ,name)
            ;;(type . line)
            (display-to-real . acul-display-to-real-line)
            (action ("Go to Line" . anything-c-action-line-goto))
            (recenter)
            (adjust))))
(defvar anything-c-source-buffer-undo-list
  (acul-source-base "Buffer undo list"
                    '((candidates . anything-c-buffer-undo-list-candidates))))
;; (let ((anything-after-initialize-hook (lambda () (anything-follow-mode t)))) (anything 'anything-c-source-buffer-undo-list))

(defun acul-propertize (string value line padding point-of)
  (let* ((pt (funcall point-of value))
         (fix-point (if (= (line-end-position) (funcall point-of value))
                      '1-
                      'identity)))
    (add-text-properties (+ padding (funcall fix-point (current-column)))
                         (1+ (+ padding (funcall fix-point (current-column))))
                         '(face bold)
                         string)
    (propertize string 'point (funcall fix-point pt))))
(defun anything-c-buffer-undo-list-candidates+ ()
  (let ((propertize
         (lambda (string value line padding point-of)
           (propertize (acul-propertize string value line padding point-of)
                       'after-goto
                       (lexical-let ((pt (funcall point-of value)))
                         (lambda ()
                           (goto-char pt)
                           (when (and (= 0 (current-column))
                                      (string-match
                                       "[[:space:]]"
                                       (buffer-substring-no-properties
                                        (point) (1+ (point)))))
                             (beginning-of-line-text))))))))
    (anything-c-buffer-undo-list-candidates propertize)))
(defun acul-action-transformer (_actions _selection)
  '(("Go to Line"
     . (lambda (selection)
         (anything-c-action-line-goto selection)
         (anything-aif (get-text-property 0 'after-goto (cadr selection))
             (funcall it))))))

(defface anything-c-undo-list-highlight-point-overlay-face
  '((t (:inherit anything-selection-face)))
  "Face for point overlay face used by Buffer/Point undo list."
  :group 'anything-config)
(defvar acul-highlight-point-overlay-face 'anything-c-undo-list-highlight-point-overlay-face)
(defvar acul-highlight-point-overlay nil)
(defun acul-action-highlight-point (selection)
  (unless acul-highlight-point-overlay
    (setq acul-highlight-point-overlay (make-overlay 0 0)))
  (anything-aif (get-text-property 0 'point (cadr selection))
      (move-overlay acul-highlight-point-overlay it (1+ it)))
  (overlay-put acul-highlight-point-overlay
               'face 'highlight))
(defun acul-cleanup-highlight-point ()
  (anything-aif acul-highlight-point-overlay (delete-overlay it)))

(defvar anything-c-source-buffer-undo-list+
  (acul-source-base "Buffer undo list"
                    `((candidates . anything-c-buffer-undo-list-candidates+)
                      (persistent-action
                       . (lambda (selection)
                           (anything-c-action-line-goto selection)
                           (acul-action-highlight-point selection)))
                      (cleanup . acul-cleanup-highlight-point)
                      (action-transformer . acul-action-transformer))))
;; (let ((anything-after-initialize-hook (lambda () (anything-follow-mode t)))) (anything 'anything-c-source-buffer-undo-list+))

;;; anything-c-point-undo-list-*
(defcustom anything-c-point-undo-list-examine-limit
  anything-c-buffer-undo-list-examine-limit
  "*Number of the `point-undo-list' items to be examined by `anything-c-point-undo-list'."
  :type 'integer
  :group 'anything-config)
(defcustom anything-c-point-undo-list-collect-limit most-positive-fixnum
  "*Number of items to be listed by `anything-c-point-undo-list'."
  :type 'integer
  :group 'anything-config)
(defun anything-c-point-undo-list-candidates (&optional propertize)
  (with-current-buffer anything-current-buffer
    (apply 'acul-candidates
           (subseq point-undo-list
                   0 anything-c-point-undo-list-examine-limit)
           (apply-partially 'acul-collector2
                            (or (anything-attr 'filter2)
                                (acul-make-point-line-filter2 1 'caar 'car)))
           (max (or (anything-attr 'candidate-number-limit) 0)
                anything-c-point-undo-list-collect-limit)
           'car
           (list propertize))))
(defvar anything-c-source-point-undo-list
  (acul-source-base "Point undo list"
                    `((candidates . anything-c-point-undo-list-candidates)
                      (filter2
                       . ,(acul-make-point-line-filter2 1 'caar 'car)))))
;; (let ((anything-after-initialize-hook (lambda () (anything-follow-mode t)))) (anything 'anything-c-source-point-undo-list))

(defun* anything-c-point-undo-list-candidates+
    (&optional (set-window-start-p nil))
  (let ((propertize
         (lambda (string value line padding point-of)
           (propertize (acul-propertize string value line padding point-of)
                       'after-goto
                       (lexical-let ((value value)
                                     (set-window-start-p set-window-start-p))
                         (lambda ()
                           (goto-char (car value))
                           (when set-window-start-p
                             (set-window-start (get-buffer-window)
                                               (cdr value)))))))))
    (anything-c-point-undo-list-candidates propertize)))
(defvar anything-c-source-point-undo-list+
  (acul-source-base "Point undo list"
                    `((candidates . anything-c-point-undo-list-candidates+)
                      (persistent-action
                       . (lambda (selection)
                           (anything-c-action-line-goto selection)
                           (acul-action-highlight-point selection)))
                      (cleanup . acul-cleanup-highlight-point)
                      (filter2
                       . ,(lexical-let ((linum (acul-make-linum-of)))
                            (lambda (x ys)
                              (let ((skipp
                                     (lambda (x ys)
                                       (or (find-if
                                            (apply-partially '= (car x))
                                            ys :key 'caar)
                                           (save-excursion
                                             (goto-char (car x))
                                             (string-match
                                              "[[:space:]]"
                                              (buffer-substring-no-properties
                                               (point) (min
                                                        (1+ (point))
                                                        (point-max)))))))))
                                (if (not (funcall skipp x ys))
                                  (values t (funcall linum (car x)))
                                  (values nil nil))))))
                      (action-transformer . acul-action-transformer))))
;; (let ((anything-after-initialize-hook (lambda () (anything-follow-mode t)))) (anything 'anything-c-source-point-undo-list+))

(defvar anything-c-undo-list-sources
  '(anything-c-source-buffer-undo-list+
    anything-c-source-point-undo-list+))
(defun anything-c-undo-list ()
  "Preconfigured `anything' for undo list."
  (interactive)
  (let ((anything-after-initialize-hook (lambda () (anything-follow-mode t))))
    (anything-other-buffer anything-c-undo-list-sources
                           "*anything undo list*")))

;; WIP: Hilight at the filtered-candidate-transformer phase.
;; TODO: Out-of-sync.
;;(require 'highlight)
;;(defvar acul-highlights-storage (make-hash-table :test 'eq))
;; (defun acul-highlights-initialize (source)
;;   (puthash source (make-hash-table :test 'equal) acul-highlights-storage))
;; (defun acul-highlights-put (source key value)
;;   (puthash key value (gethash source acul-highlights)))
;; (defun acul-highlight-current-buffer-function (source string value point-of)
;;   (let ((fix-point
;;          (let ((pt (funcall point-of value)))
;;            (save-excursion
;;              (goto-char pt)
;;              (cond ((= (line-end-position) (line-beginning-position)) nil)
;;                    ((= pt (line-end-position)) '1-)
;;                    (t 'identity))))))
;;     (unless (null fix-point)
;;       (lexical-let ((pt (funcall (acul-compose fix-point point-of) value)))
;;         (put-text-property 0 1 'pt pt string)
;;         (acul-highlights-put source
;;                              pt
;;                              (cons (lambda (face)
;;                                      (hlt-highlight-region pt (1+ pt) face))
;;                                    (lambda ()
;;                                      (hlt-unhighlight-region pt (1+ pt)))))))))
;; (defadvice acul-propertize (after acul-highlight-current-buffer activate)
;;   (acul-highlight-current-buffer-function (anything-get-current-source)
;;                                           string value point-of))
;; (defun* acul-highlight-current-buffer-clear
;;     (&optional (buffer anything-current-buffer))
;;   (with-current-buffer buffer
;;     (maphash (lambda (_k v)
;;                (funcall (acul-compose 'funcall 'cdr) v))
;;              acul-highlights)))
;; (defun acul-make-candidate-transformer (highlight-face)
;;   (lexical-let ((highlight-face highlight-face))
;;     (lambda (cands _source)
;;       (prog1 cands
;;         (with-current-buffer anything-current-buffer
;;           (acul-highlight-current-buffer-clear)
;;           (loop for x in cands
;;                 do (let ((pt (get-text-property 0 'pt x)))
;;                      (anything-aif (gethash pt acul-highlights)
;;                          (funcall (car it) highlight-face)))))))))
;; (defvar anything-c-source-point-undo-list+
;;   (acul-source-base "Point undo list"
;;                     `((init . acul-highlights-initialize)
;;                       (cleanup . acul-highlight-current-buffer-clear)
;;                       (candidates . anything-c-point-undo-list-candidates+)
;;                       (persistent-action . anything-c-action-line-goto)
;;                       (action-transformer . acul-action-transformer)
;;                       (filtered-candidate-transformer
;;                        . ,(acul-make-candidate-transformer 'highlight)))))

(provide 'anything-c-undo-list)
;;; anything-c-undo-list ends here
