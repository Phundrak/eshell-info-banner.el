;;; eshell-info-banner.el --- System information as your Eshell banner -*- lexical-binding: t -*-

;; Author: Lucien Cartier-Tilet <lucien@phundrak.com>
;; Maintainer: Lucien Cartier-Tilet
;; Version: 0.2.0
;; Package-Requires: ((emacs "24") (dash "2") (f "0.20") (s "1"))
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
(require 'em-banner)

                                        ; Groups ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup eshell-info-banner ()
  "System information as your Eshell banner."
  :prefix "eshell-info-banner-"
  :link '(url-link :tag "Gitea" "https://labs.phundrak.com/phundrak/eshell-info-banner.el"))

                                        ; Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst eshell-info-banner--min-length-left 8
  "Minimum length of text on the left hand side of the banner.")

                                        ; Custom variables ;;;;;;;;;;;;;;;;;;;;

(defcustom eshell-info-banner-shorten-path-from 7
  "From which length should a path be shortened?"
  :group 'eshell-info-banner
  :type 'integer)

(defcustom eshell-info-banner-width 80
  "Width of the info banner to be shown in Eshell."
  :group 'eshell-info-banner
  :type 'integer)

(defcustom eshell-info-banner-progress-bar-char "="
  "Character to fill the progress bars with."
  :group 'eshell-info-banner
  :type 'char)

(defcustom eshell-info-banner-warning-percentage 75
  "When to warn about a percentage."
  :group 'eshell-info-banner
  :type 'float)

(defcustom eshell-info-banner-critical-percentage 90
  "When a percentage becomes critical."
  :group 'eshell-info-banner
  :type 'float)

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
                  :path (if (length> mount eshell-info-banner-shorten-path-from)
                            (eshell-info-banner--abbr-path mount t)
                          mount)
                  :size size
                  :used used
                  :percent (string-to-number
                            (string-trim-left percent (regexp-quote "%")))))))
           partitions)))

(defun eshell-info-banner--get-longest-path (partitions &optional len)
  "Find the length of the longest partition path in `PARTITIONS'.

The variable `LEN' should only be used internally and represents
the longest path so far, or the minimum length of text present on
the left side of the banner."
  (let ((len (if (null len)
                 eshell-info-banner--min-length-left
               len)))
    (if (null partitions)
        len
      (let* ((path (eshell-info-banner--mounted-partitions-path (car partitions)))
             (len (max (length path) len)))
        (eshell-info-banner--get-longest-path (cdr partitions) len)))))

(defun eshell-info-banner--get-color-percentage (percentage)
  "Display a `PERCENTAGE' with its according face."
  (let ((percentage (if (stringp percentage)
                        (string-to-number percentage)
                      percentage)))
    (cond
     ((>= percentage eshell-info-banner-critical-percentage)
      'eshell-info-banner-critical-face)
     ((>= percentage eshell-info-banner-warning-percentage)
      'eshell-info-banner-warning-face)
     (t 'eshell-info-banner-normal-face))))

(defun eshell-info-banner--progress-bar (length percentage &optional invert)
  "Display a progress bar `LENGTH' long and `PERCENTAGE' full.
The full path will be displayed filled with the character
specified by `eshell-info-banner-progress-bar-char' up to
`PERCENTAGE' percents.  The rest will be empty.

If `INVERT' is t, then consider the percentage to approach
critical levels close to 0 rather than 100."
  (message "Length: %s" length)
  (let* ((length-filled     (if (= 0 percentage)
                                0
                              (/ (* length percentage) 100)))
         (length-empty      (- length length-filled))
         (percentage-level (if invert
                               (- 100 percentage)
                             percentage)))
    (concat
     (eshell-info-banner--with-face "[" :weight 'bold)
     (eshell-info-banner--with-face (s-repeat length-filled eshell-info-banner-progress-bar-char)
                                    :weight 'bold
                                    :inherit (eshell-info-banner--get-color-percentage percentage-level))
     (eshell-info-banner--with-face (s-repeat length-empty eshell-info-banner-progress-bar-char)
                                    :weight 'bold
                                    :inherit 'eshell-info-banner-background-face)
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
            (format " %6s / %-5s (%3s%%)\n"
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
            (format " %6s / %-5s (%3s%%)"
                    (eshell-info-banner--mounted-partitions-used partition)
                    (eshell-info-banner--mounted-partitions-size partition)
                    (eshell-info-banner--with-face
                     (number-to-string percentage)
                     :inherit (eshell-info-banner--get-color-percentage percentage))))))

(defun eshell-info-banner--display-battery (text-padding bar-length)
  "If the computer has a battery, display its level.

Pad the left text with dots by `TEXT-PADDING' characters.

`BAR-LENGTH' indicates the length in characters of the progress
bar.

The usage of `eshell-info-banner-warning-percentage' and
`eshell-info-banner-critical-percentage' is reversed, and can be
thought of as the “percentage of discharge” of the computer.
Thus, setting the warning at 75% will be translated as showing
the warning face with a battery level of 25% or less."
  (let ((battery-level (battery)))
    (if (string= battery-level "Battery status not available")
        ""
      (let ((percentage (save-match-data
                          (string-match "\\([0-9]+\\)\\.[0-9]+%" battery-level)
                          (string-to-number (substring battery-level (match-beginning 1) (match-end 1))))))
        (concat (s-pad-right text-padding "." "Battery")
                ": "
                (eshell-info-banner--progress-bar bar-length
                                                  percentage
                                                  t)
                (s-repeat 16 " ")
                (format "(%3s%%)\n" (eshell-info-banner--with-face
                                     (number-to-string percentage)
                                     :inherit (eshell-info-banner--get-color-percentage (- 100.0 percentage)))))))))

                                        ; Public functions ;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun eshell-info-banner ()
  "Banner for Eshell displaying system information."
  (let* ((partitions    (eshell-info-banner--get-mounted-partitions))
         (os            (replace-regexp-in-string
                         ".*\"\\(.+\\)\""
                         "\\1"
                         (car (-filter (lambda (line)
                                         (s-contains? "PRETTY_NAME" line))
                                       (s-lines (with-temp-buffer
                                                  (insert-file-contents "/etc/os-release")
                                                  (buffer-string)))))))
         (hostname      (system-name))
         (uptime        (s-chop-prefix "up "
                                       (s-trim (shell-command-to-string "uptime -p"))))
         (kernel        (concat (s-trim (shell-command-to-string "uname -s"))
                                " "
                                (s-trim (shell-command-to-string "uname -r"))))
         (left-padding  (eshell-info-banner--get-longest-path partitions))
         (left-text     (max (length os)
                             (length hostname)))
         (left-length   (+ left-padding 2 left-text)) ; + ": "
         (right-text    (+ (length "Kernel: ")
                           (max (length uptime)
                                (length kernel))))
         (tot-width     (max (+ left-length right-text 3)
                             eshell-info-banner-width))
         (middle-padding (- tot-width right-text left-padding 4))
         (memory        (-map (lambda (line)
                                (s-split " " line t))
                              (s-split "\n"
                                       (shell-command-to-string "free -b | tail -2")
                                       t)))
         (ram           (nth 0 memory))
         (swap          (nth 1 memory))
         (bar-length    (- tot-width left-padding 4 22)))
    (concat (format "%s\n" (s-repeat tot-width eshell-info-banner-progress-bar-char))
            (format "%s: %s Kernel.: %s\n"
                    (s-pad-right left-padding
                                 "."
                                 "OS")
                    (s-pad-right middle-padding " " (eshell-info-banner--with-face os :weight 'bold))
                    kernel)
            (format "%s: %s Uptime.: %s\n"
                    (s-pad-right left-padding "." "Hostname")
                    (s-pad-right middle-padding " " (eshell-info-banner--with-face hostname :weight 'bold))
                    uptime)
            (eshell-info-banner--display-battery left-padding bar-length)
            (eshell-info-banner--display-memory "Ram"
                                                (string-to-number (nth 2 ram))
                                                (string-to-number (nth 1 ram))
                                                left-padding
                                                bar-length)
            (eshell-info-banner--display-memory "Swap"
                                                (string-to-number (nth 2 swap))
                                                (string-to-number (nth 1 swap))
                                                left-padding
                                                bar-length)
            (mapconcat (lambda (partition)
                         (eshell-info-banner--display-partition partition left-padding bar-length))
                       partitions
                       "\n")
            (format "\n%s\n" (s-repeat tot-width eshell-info-banner-progress-bar-char)))))

;;;###autoload
(defun eshell-info-banner-update-banner ()
  "Update the Eshell banner."
  (setq eshell-banner-message (eshell-info-banner)))

(provide 'eshell-info-banner)

;;; eshell-info-banner.el ends here
