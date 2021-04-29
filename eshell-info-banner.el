;;; eshell-info-banner.el --- System information as your Eshell banner -*- lexical-binding: t -*-

;; Author: Lucien Cartier-Tilet <lucien@phundrak.com>
;; Maintainer: Lucien Cartier-Tilet
;; Version: 0.1.0
;; Package-Requires: ((emacs "24") (dash "2") (f "0.20") (s "1))
;; Homepage: https://labs.phundrak.com/phundrak/eshell-info-banner.el

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:


;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)

                                        ; Groups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup eshell-info-banner nil
  "System information as your Eshell banner."
  :prefix "eshell-info-banner-"
  :link '(url-link :tag "Gitea" "https://labs.phundrak.com/phundrak/eshell-info-banner.el"))

                                        ; Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst eshell-info-banner--min-length-left 8
  "Minimum length of text on the left hand side of the banner.")

                                        ; Custom variables ;;;;;;;;;;;;;;;;;;;;

(defcustom eshell-info-banner--max-length-part 13
  "Maximum length of a partition’s ruler."
  :group 'eshell-info-banner
  :type 'integer)

(defcustom eshell-info-banner--percentage-critical 90
  "When a percentage becomes critical."
  :group 'eshell-info-banner
  :type 'float)

(defcustom eshell-info-banner--percentage-warning 75
  "When to warn about a percentage."
  :group 'eshell-info-banner
  :type 'float)

(defcustom eshell-info-banner--progress-bar-char "="
  "Character to fill the progress bars with."
  :group 'eshell-info-banner
  :type 'char)

                                        ; Faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface eshell-info-banner-background-face
  '((t :inherit font-lock-comment-face))
  "Face for \"empty\" part of progress bars."
  :group 'eshell-info-banner)

(defface eshell-info-banner-normal-face
  '((t :inherit font-lock-string-face))
  "Face for eshell-info-banner progress bars displaying acceptable levels."
  :group 'eshell-info-banner)

(defface eshell-info-banner-warning-face
  '((t :inherit warning))
  "Face for eshell-info-banner progress bars displaying high levels."
  :group 'eshell-info-banner)

(defface eshell-info-banner-critical-face
  '((t :inherit error))
  "Face for eshell-info-banner progress bars displaying critical levels."
  :group 'eshell-info-banner)

                                        ; Structs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct eshell-info-banner--mounted-partitions
  "Object representing a mounted partition found in the system."
  path size used percent)

                                        ; Macros ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro eshell-info-banner--with-face (str &rest properties)
  "Helper macro for applying face `PROPERTIES' to `STR'."
  `(propertize ,str 'face (list ,@properties)))

                                        ; Internal functions ;;;;;;;;;;;;;;;;;;

(defun eshell-info-banner--abbr-path (path &optional abbr)
  "Remove `$HOME' from `PATH', abbreviate parent dirs if `ABBR' non nil.

Abbreviate `PATH' by removing the value of `HOME' if it is
present in the former, and if `ABBR' is t then all parent
directories of the current `PATH' are abbreviated to only one
character.  If an abbreviated directory starts with a dot, then
include it before the abbreviated name of the directory,
e.g. \".config\" -> \".c\".

For public use, `PATH' should be a string representing a UNIX
path.  For internal use, `PATH' cna also be a list. If `PATH' is
neither of these, an error will be thrown by the function."
  (cond
   ((stringp path) (abbreviate-file-name
                    (if abbr
                        (eshell-info-banner--abbr-path (f-split (eshell-info-banner--abbr-path path)))
                      path)))
   ((null path) "")
   ((listp path)
    (f-join (if (length= path 1)
                (car path)
              (let* ((dir        (car path))
                     (first-char (substring dir 0 1)))
                (if (string= "." first-char)
                    (substring dir 0 2)
                  first-char)))
            (eshell-info-banner--abbr-path (cdr path))))
   (t (error "Invalid argument %s, neither stringp or listp" path))))

(defun eshell-info-banner--get-mounted-partitions ()
  "Detect mounted partitions on the system.

Return detected partitions as a list of structs."
  (let ((partitions (split-string (shell-command-to-string "df -lH") (regexp-quote "\n") t)))
    (-keep (lambda (partition)
             (let* ((partition  (split-string partition " " t))
                    (filesystem (nth 0 partition))
                    (size       (nth 1 partition))
                    (used       (nth 2 partition))
                    (percent    (nth 4 partition))
                    (mount      (nth 5 partition)))
               (when (string-prefix-p "/dev" filesystem t)
                 (make-eshell-info-banner--mounted-partitions
                  :path (if (length> mount eshell-info-banner--max-length-part)
                            mount
                          (eshell-info-banner--abbr-path mount t))
                  :size size
                  :used used
                  :percent (string-to-number
                            (string-trim-left percent (regexp-quote "%")))))))
           partitions)))

(defun eshell-info-banner--get-left-pad (initial-pad partitions)
  "Get left padding for the various rulers.

If `PARTITIONS' have a name short enough, then return
`INITIAL-PAD', otherwise return enough length to display the
shortened name of the partitions with a long name."
  (if partitions
      (let ((part-length (length (eshell-info-banner--mounted-partitions-path (car partitions)))))
        (eshell-info-banner--get-left-pad (if (> part-length initial-pad)
                                              part-length
                                            initial-pad)
                                          (cdr partitions)))
    initial-pad))

(defun eshell-info-banner--get-color-percentage (percentage)
  "Display a `PERCENTAGE' with its according face."
  (let ((percentage (if (stringp percentage)
                        (string-to-number percentage)
                      percentage)))
    (cond
     ((> percentage eshell-info-banner--percentage-critical)
      'eshell-info-banner-critical-face)
     ((> percentage eshell-info-banner--percentage-warning)
      'eshell-info-banner-warning-face)
     (t 'eshell-info-banner-normal-face))))

(defun eshell-info-banner--progress-bar (length percentage)
  "Display a progress bar `LENGTH' long and `PERCENTAGE' full.
The full path will be displayed filled with the character
specified by `eshell-info-banner--progress-bar-char' up to
`PERCENTAGE' percents.  The rest will be empty."
  (let* ((length-filled (if (= 0 percentage)
                            0
                          (/ (* length percentage) 100)))
         (length-empty  (- length length-filled)))
    (concat
     (eshell-info-banner--with-face "[" :weight 'bold)
     (eshell-info-banner--with-face (s-repeat length-filled eshell-info-banner--progress-bar-char)
                                    :weight 'bold
                                    :inherit (eshell-info-banner--get-color-percentage percentage))
     (eshell-info-banner--with-face (s-repeat length-empty eshell-info-banner--progress-bar-char)
                                    :weight 'bold :inherit 'eshell-info-banner-background-face)
     (eshell-info-banner--with-face "]" :weight 'bold))))

(defun eshell-info-banner--display-memory (type used total text-padding bar-length)
  "Display a memory’s usage with a progress bar.
Displayed progress bars will have this appearance:

TYPE......: [=========] XXG / XXG  (XX%)

The `TYPE' of memory will be the text on the far left, while
`USED' and `TOTAL' will be displayed on the right of the progress
bar.  From them, a percentage will be computed which will be used
to display a colored percentage of the progress bar and it will
be displayed on the far right.

`TEXT-PADDING' will determine how many dots are necessary between
`TYPE' and the colon.

`BAR-LENGTH' determines the length of the progress bar to be
displayed."
  (let ((percentage (if (= used 0)
                        0
                      (/ (* 100 used) total))))
    (concat (s-pad-right text-padding "." type)
            ": "
            (eshell-info-banner--progress-bar bar-length percentage)
            (format " %6s / %-5s (%s%%)\n"
                    (file-size-human-readable used)
                    (file-size-human-readable total)
                    (eshell-info-banner--with-face (number-to-string percentage)
                                                   :inherit (eshell-info-banner--get-color-percentage percentage))))))

(defun eshell-info-banner--display-partition (partition text-padding bar-length)
  "Display a progress bar showing how full a `PARTITION' is.

`BAR-LENGTH' represents the total length of the progress bar,
while `TEXT-PADDING' indicates how many dots are to be put
between the partition’s name and the colon following it.

See also `eshell-info-banner--display-memory'."
  (let ((percentage (eshell-info-banner--mounted-partitions-percent partition)))
    (concat (s-pad-right text-padding "."
                         (eshell-info-banner--with-face (eshell-info-banner--mounted-partitions-path partition)
                                                        :weight 'bold))
            ": "
            (eshell-info-banner--progress-bar bar-length percentage)
            (format " %6s/%-5s (%s%%)\n"
                    (eshell-info-banner--mounted-partitions-used partition)
                    (eshell-info-banner--mounted-partitions-size partition)
                    (eshell-info-banner--with-face
                     (number-to-string percentage)
                     :inherit (eshell-info-banner--get-color-percentage percentage))))))



(provide 'eshell-info-banner)

;;; eshell-info-banner.el ends here
