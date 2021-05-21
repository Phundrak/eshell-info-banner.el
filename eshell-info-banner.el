;;; eshell-info-banner.el --- System information as your Eshell banner -*- lexical-binding: t -*-

;; Author: Lucien Cartier-Tilet <lucien@phundrak.com>
;; Maintainer: Lucien Cartier-Tilet <lucien@phundrak.com>
;; Version: 0.4.1
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

;; `eshell-info-banner' is a utility for cerating an informative
;; banner akin to fish_greeting if fish shell but for Eshell.  It can
;; provide information on:
;; - the OS’ name
;; - the OS’ kernel
;; - the hostname
;; - the uptime
;; - the system’s memory usage (RAM, swap, disk)
;; - the battery status
;; It can be TRAMP-aware or not, depending on the user’s preferences.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'em-banner)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                Group                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup eshell-info-banner ()
  "System information as your Eshell banner."
  :prefix "eshell-info-banner-"
  :link '(url-link :tag "Gitea" "https://labs.phundrak.com/phundrak/eshell-info-banner.el")
  :link '(url-link :tag "Github" "https://github.com/Phundrak/eshell-info-banner.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;              Constants              ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst eshell-info-banner--min-length-left 8
  "Minimum length of text on the left hand side of the banner.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Custom variables          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom eshell-info-banner-tramp-aware t
  "Make `eshell-info-banner' TRAMP aware."
  :group 'eshell-info-banner
  :type 'boolean
  :safe #'booleanp)

(defcustom eshell-info-banner-shorten-path-from 7
  "From which length should a path be shortened?"
  :group 'eshell-info-banner
  :type 'integer
  :safe #'integer-or-marker-p)

(defcustom eshell-info-banner-width 80
  "Width of the info banner to be shown in Eshell."
  :group 'eshell-info-banner
  :type 'integer
  :safe #'integer-or-marker-p)

(defcustom eshell-info-banner-progress-bar-char "="
  "Character to fill the progress bars with."
  :group 'eshell-info-banner
  :type 'string
  :safe #'stringp)

(defcustom eshell-info-banner-warning-percentage 75
  "When to warn about a percentage."
  :group 'eshell-info-banner
  :type 'float
  :safe #'floatp)

(defcustom eshell-info-banner-critical-percentage 90
  "When a percentage becomes critical."
  :group 'eshell-info-banner
  :type 'float
  :safe #'floatp)

(defcustom eshell-info-banner-partition-prefixes '("/dev")
  "List of prefixes for detecting which partitions to display."
  :group 'eshell-info-banner
  :type 'list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                Faces                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;                Macros               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro eshell-info-banner--with-face (str &rest properties)
  "Helper macro for applying face `PROPERTIES' to `STR'."
  `(propertize ,str 'face (list ,@properties)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;          Internal functions         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                        ; Misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eshell-info-banner--get-uptime ()
  "Get uptime of machine if `uptime' is available.

If the executable `uptime' is not found, return nil."
  (when (executable-find "uptime")
    (let ((uptime-str (shell-command-to-string "uptime -p")))
      (if (not (string-match-p "invalid" uptime-str))
          (s-chop-prefix "up " (s-trim uptime-str))
        (let ((uptime-str (shell-command-to-string "uptime")))
          (save-match-data
            (string-match " *[0-9:]+ *up *\\([0-9:]+\\)," uptime-str)
            (substring-no-properties uptime-str
                                     (match-beginning 1)
                                     (match-end 1))))))))

(eshell-info-banner--get-uptime)

                                        ; Partitions ;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct eshell-info-banner--mounted-partitions
  "Object representing a mounted partition found in the system."
  path size used percent)

(defun eshell-info-banner--get-longest-path (partitions)
  "Return the length of the longest partition path in `PARTITIONS'.

The returned value is in any case greater than `eshell-info-banner--min-length-left'."
  (-reduce-from (lambda (len partition)
                  (max len
                       (length (eshell-info-banner--mounted-partitions-path partition))))
                eshell-info-banner--min-length-left
                partitions))

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
                        (eshell-info-banner--abbr-path
                         (f-split (eshell-info-banner--abbr-path path)))
                      path)))
   ((null path) "")
   ((listp path)
    (f-join (if (= (length path) 1)
                (car path)
              (let* ((dir        (car path))
                     (first-char (substring dir 0 1)))
                (if (string= "." first-char)
                    (substring dir 0 2)
                  first-char)))
            (eshell-info-banner--abbr-path (cdr path))))
   (t (error "Invalid argument %s, neither stringp or listp" path))))

(defun eshell-info-banner--get-mounted-partitions/gnu ()
  "Detect mounted partitions on a Linux system.

Return detected partitions as a list of structs.  See
`eshell-info-banner-partition-prefixes' to see how partitions are
chosen."
  (let ((partitions (split-string (shell-command-to-string "LANG=C df -lH") (regexp-quote "\n") t)))
    (-keep (lambda (partition)
             (let* ((partition  (split-string partition " " t))
                    (filesystem (nth 0 partition))
                    (size       (nth 1 partition))
                    (used       (nth 2 partition))
                    (percent    (nth 4 partition))
                    (mount      (nth 5 partition)))
               (when (seq-some (lambda (prefix)
                                 (string-prefix-p prefix filesystem t))
                               eshell-info-banner-partition-prefixes)
                 (make-eshell-info-banner--mounted-partitions
                  :path (if (> (length mount) eshell-info-banner-shorten-path-from)
                            (eshell-info-banner--abbr-path mount t)
                          mount)
                  :size size
                  :used used
                  :percent (string-to-number
                            (string-trim-left percent (regexp-quote "%")))))))
           partitions)))

(defun eshell-info-banner--get-mounted-partitions/windows ()
  "Detect mounted partitions on a Windows system.

Return detected partitions as a list of structs.  See
`eshell-info-banner-partition-prefixes' to see how partitions are
chosen."
  (progn
    (message "Partition detection for Windows and DOS not yet supported.")
    nil))

(defun eshell-info-banner--get-mounted-partitions/darwin ()
  "Detect mounted partitions on a Darwin/macOS system.

Return detected partitions as a list of structs.  See
`eshell-info-banner-partition-prefixes' to see how partitions are
chosen."
  (progn
    (message "Partition detection for macOS and Darwin-based OSes not yet supported.")
    nil))

(defun eshell-info-banner--get-mounted-partitions ()
  "Detect mounted partitions on the system.

Return detected partitions as a list of structs."
  (pcase system-type
    ((or 'gnu 'gnu/linux 'gnu/kfreebsd)
     (eshell-info-banner--get-mounted-partitions/gnu))
    ((or 'ms-dos 'windows-nt 'cygwin)
     (eshell-info-banner--get-mounted-partitions/windows))
    ('darwin
     (eshell-info-banner--get-mounted-partitions/darwin))
    (other
     (progn
       (message "Partition detection for %s not yet supported." other)
       nil))))

(defun eshell-info-banner--partition-to-string (partition text-padding bar-length)
  "Display a progress bar showing how full a `PARTITION' is.

For TEXT-PADDING and BAR-LENGTH, see the documentation of
`eshell-info-banner--display-memory'."
  (let ((percentage (eshell-info-banner--mounted-partitions-percent partition)))
    (concat (s-pad-right text-padding
                         "."
                         (eshell-info-banner--with-face
                          (eshell-info-banner--mounted-partitions-path partition)
                          :weight 'bold))
            ": "
            (eshell-info-banner--progress-bar bar-length percentage)
            (format " %6s / %-5s (%3s%%)"
                    (eshell-info-banner--mounted-partitions-used partition)
                    (eshell-info-banner--mounted-partitions-size partition)
                    (eshell-info-banner--with-face
                     (number-to-string percentage)
                     :inherit (eshell-info-banner--get-color-percentage percentage))))))

(defun eshell-info-banner--display-partitions (text-padding bar-length)
  "Display the detected mounted partitions of the system.

For TEXT-PADDING and BAR-LENGTH, see the documentation of
`eshell-info-banner--display-memory'."
  (mapconcat (lambda (partition)
               (eshell-info-banner--partition-to-string partition text-padding bar-length))
             (eshell-info-banner--get-mounted-partitions)
             "\n"))


                                        ; Memory ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun eshell-info-banner--get-memory/linux ()
  "Get memory usage for GNU/Linux and Hurd."
  (-map (lambda (line)
          (let* ((line (split-string line " " t)))
            (list (s-chop-suffix ":" (nth 0 line))   ; name
                  (string-to-number (nth 1 line))    ; total
                  (string-to-number (nth 2 line))))) ; used
        (split-string (shell-command-to-string "LANG=C free -b | tail -2")
                      "\n"
                      t)))

(defun eshell-info-banner--get-memory/bsd ()
  "Get memory usage for *BSD."
  (message "Memory usage not yet implemented for BSD")
  nil)

(defun eshell-info-banner--get-memory/darwin ()
  "Get memory usage for macOS and Darwin-based OSes."
  (message "Memory usage not yet implemented for macOS and Darwin-based OSes")
  nil)

(defun eshell-info-banner--get-memory/windows ()
  "Get memory usage for Window."
  (message "Memory usage not yet implemented for Windows and DOS")
  nil)

(defun eshell-info-banner--get-memory ()
  "Get memory usage of current operating system.

Return a list of either one or two elements.  The first element
represents the RAM, the second represents the swap.  Both are
lists and contain three elements: the name of the memory, the
total amount of memory available, and the amount of used memory,
in bytes."
  (pcase system-type
    ((or 'gnu 'gnu/linux)
     (eshell-info-banner--get-memory/linux))
    ('gnu/kfreebsd
     (eshell-info-banner--get-memory/bsd))
    ('darwin
     (eshell-info-banner--get-memory/darwin))
    ((or 'ms-dos 'windows-nt 'cygwin)
     (eshell-info-banner--get-memory/windows))
    (os (message "Memory usage not yet implemented for %s" os)
        nil)))

(defun eshell-info-banner--memory-to-string (type total used text-padding bar-length)
  "Display a memory’s usage with a progress bar.

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
            (format " %6s / %-5s (%3s%%)"
                    (file-size-human-readable used)
                    (file-size-human-readable total)
                    (eshell-info-banner--with-face
                     (number-to-string percentage)
                     :inherit (eshell-info-banner--get-color-percentage percentage))))))

(defun eshell-info-banner--display-memory (text-padding bar-length)
  "Display memories detected on your system.

This function will create a string used by `eshell-info-banner'
in order to display memories detected by the package, generally
the Ram at least, sometimes the swap too.  Displayed progress
bars will have this appearance:

TYPE......: [=========] XXG / XXG  (XX%)

`TEXT-PADDING': the space allocated to the text at the left of the
progress bar.

`BAR-LENGTH': the length of the progress bar."
  (mapconcat (lambda (mem)
               (eshell-info-banner--memory-to-string (nth 0 mem) (nth 1 mem)
                                                     (nth 2 mem) text-padding
                                                     bar-length))
             (eshell-info-banner--get-memory)
             "\n"))


                                        ; Display information ;;;;;;;;;;;;;;;;;

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
    (if (or (string= battery-level "Battery status not available")
            (string-match-p (regexp-quote "N/A") battery-level))
        ""
      (let ((percentage (save-match-data
                          (string-match "\\([0-9]+\\)\\(\\.[0-9]\\)?%" battery-level)
                          (string-to-number (substring battery-level
                                                       (match-beginning 1)
                                                       (match-end 1))))))
        (concat (s-pad-right text-padding "." "Battery")
                ": "
                (eshell-info-banner--progress-bar bar-length
                                                  percentage
                                                  t)
                (s-repeat 16 " ")
                (format "(%3s%%)\n"
                        (eshell-info-banner--with-face
                         (number-to-string percentage)
                         :inherit (eshell-info-banner--get-color-percentage (- 100.0 percentage)))))))))


                                        ; Operating system identification ;;;;;;;;;;;;;;;;;;
(defun eshell-info-banner--get-os-information-from-release-file (&optional release-file)
  "Read the operating system from the given RELEASE-FILE.

If RELEASE-FILE is nil, use '/etc/os-release'."
  (let ((prefix (if eshell-info-banner-tramp-aware (file-remote-p default-directory) "")))
    (with-temp-buffer
      (insert-file-contents (concat prefix (or release-file "/etc/os-release")))
      (goto-char (point-min))
      (re-search-forward "PRETTY_NAME=\"\\(.*\\)\"")
      (match-string 1))))

(defun eshell-info-banner--get-os-information-from-hostnamectl ()
  "Read the operating system via hostnamectl."
  (let ((default-directory (if eshell-info-banner-tramp-aware default-directory "~")))
    (with-temp-buffer
      (process-file "hostnamectl" nil t nil)
      (re-search-backward "Operating System: \\(.*\\)")
      (match-string 1))))

(defun eshell-info-banner--get-os-information-from-lsb-release ()
  "Read the operating system information from lsb_release."
  (shell-command-to-string "lsb_release -d -s"))

(defun eshell-info-banner--get-os-information-from-registry ()
  "Read the operating system information from the Windows registry."
  (let ((win32-name "Windows")
        (win32-build "Unknown"))
    (with-temp-buffer
      (call-process "reg" nil t nil "query" "HKLM\\Software\\Microsoft\\Windows NT\\CurrentVersion")
      (goto-char (point-min))
      (while (re-search-forward "\\([^[:blank:]]+\\) *\\(REG_[^[:blank:]]+\\) *\\(.+\\)" nil t)
        (cond
         ((string= "ProductName" (match-string 1)) (setq win32-name (match-string 3)))
         ((string= "BuildLab" (match-string 1)) (setq win32-build (match-string 3)))))
      (format "%s (%s)" win32-name win32-build))))

(defun eshell-info-banner--get-os-information ()
  "Get operating system identifying information."
  (let ((prefix (if eshell-info-banner-tramp-aware (file-remote-p default-directory) "")))
    (cond
     ;; Linux
     ((executable-find "hostnamectl" eshell-info-banner-tramp-aware)
      (eshell-info-banner--get-os-information-from-hostnamectl))
     ((executable-find "lsb_release" eshell-info-banner-tramp-aware)
      (eshell-info-banner--get-os-information-from-lsb-release))
     ((file-exists-p (concat prefix "/etc/os-release"))
      (eshell-info-banner--get-os-information-from-release-file))

     ;; Windows
     ((executable-find "reg")
      (eshell-info-banner--get-os-information-from-registry))
     (t "Unknown"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;           Public functions          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun eshell-info-banner ()
  "Banner for Eshell displaying system information."
  (let* ((default-directory (if eshell-info-banner-tramp-aware default-directory "~"))
         (os            (eshell-info-banner--get-os-information))
         (hostname      (if  eshell-info-banner-tramp-aware
                            (or (file-remote-p default-directory 'host) (system-name))
                          (system-name)))
         (uptime        (eshell-info-banner--get-uptime))
         (kernel        (s-trim (shell-command-to-string "uname -sr")))
         (partitions    (eshell-info-banner--get-mounted-partitions))
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
            (eshell-info-banner--display-memory left-padding bar-length)
            "\n"
            (eshell-info-banner--display-partitions left-padding bar-length)
            (format "\n%s\n" (s-repeat tot-width eshell-info-banner-progress-bar-char)))))

;;;###autoload
(defun eshell-info-banner-update-banner ()
  "Update the Eshell banner."
  (setq eshell-banner-message (eshell-info-banner)))

(provide 'eshell-info-banner)

;;; eshell-info-banner.el ends here
