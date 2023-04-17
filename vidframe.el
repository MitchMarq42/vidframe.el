;;; vidframe.el --- play videos as pictures, in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  mitch

;; Author: mitch <mitch@mitchmarq42.xyz>
;; Keywords:files,multimedia,emulations

;; Package-Requires: ((empv) (stream))

;; This program is free software; you can redistribute it and/or modify
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

;; based on a comment from Benzanol on the SystemCrafters discord...

;;; Code:
(require 'rx)
(require 'empv)
(require 'stream)

(defvar vidframe-cache-dir "/tmp/vidframe-el/cache/")
(defvar vid-playing-p nil)
(defvar vidframe--buffer "*vidframe*")

(defun number-in-string (string)
  "Given STRING (typically filename of an MP4), return the number in it."
  (let* ((firstind (string-match
		    (rx (+ num) (* (or alpha "-")) ".")
		    string))
	 (lastind (string-match
		   (rx (* (or alpha "-")) "." (** 1 3 alnum) eol)
		   string)))
    (if firstind
	(string-trim (substring string firstind lastind)
		     nil (rx "." (** 2 3 alnum)))
      "")))
(defun num-in-str< (first second)
  "Return t if FIRST is less than SECOND, otherwise nil."
  (let* ((numifyfun (lambda (str)
		      (string-to-number (number-in-string str))))
	 (firstnum (funcall numifyfun first))
	 (secondnum (funcall numifyfun second)))
    (< firstnum secondnum)))

(defun vidframe--get-fps (file)
  "Given video FILE, get frames per second using ffmpeg."
  (string-to-number (string-trim
		     (shell-command-to-string
		      (format
		       "ffmpeg -i %s 2>&1 |
			  sed -n \"s/.*, \\(.*\\) fp.*/\\1/p\""
		       file)))))

(defun vidframe--dump-frames (file)
  "Dump every frame of FILE into temp directory."
  (let ((framedir (expand-file-name "frames/" vidframe--this-file-cache)))
    (mkdir framedir 'p)
    (async-shell-command
     (format "ffmpeg -i %s %s/frame-%%07d.jpg"
	     file framedir)
     (get-buffer-create " *vidframe-frame-dump*"))))

(defun vidframe--display-image (image)
  "Replace contents of vidframe buffer with IMAGE.

  Only use in vidframe buffer, or bad things happen."
  (erase-buffer)
  (insert-image (create-image image)))

(defun vidframe-display-next-image ()
  "Show the next image in `vidframe-remaining-frames' in vidframe buffer."
  (let* ((inhibit-read-only t)
	 (next-image (stream-pop vidframe-remaining-frames)))
    (with-current-buffer (get-buffer vidframe--buffer)
      (with-silent-modifications
	(vidframe--display-image vidframe-image)
	(setq vidframe-image next-image)))))

(defun vidframe--extract-sound (file)
  "Extract sound from FILE."
  (let ((sounddir (expand-file-name "audio/" vidframe--this-file-cache)))
    (mkdir sounddir 'p)
    (async-shell-command
     (format
      "ffmpeg -i %s -vn -acodec copy %s/audio.aac"
      file sounddir)
     (get-buffer-create " *vidframe-sound-dump*"))))

(defun vidframe-play ()
  (interactive)
  (setq vidframe-timer
	(run-with-timer 0 vidframe-current-interval
			#'vidframe-display-next-image))
  (empv-resume)
  (setq vid-playing-p t)
  ;; (unless (eq (window-buffer) (get-buffer vidframe--buffer))
  ;;   (pop-to-buffer (get-buffer vidframe--buffer)
  ;; 		   #'display-buffer-reuse-window))
  )
(defun vidframe-pause ()
  (interactive)
  (cancel-timer vidframe-timer)
  (empv-pause)
  (setq vid-playing-p nil))

(defun vidframe-pause-or-resume ()
  "If vidframe is paused, resume.  If playing, pause."
  (interactive)
  (if vid-playing-p
      (vidframe-pause)
    (vidframe-play)))

(defun vidframe--clear-cache ()
  "Delete all cached files used by vidframe.el."
  (interactive)
  (delete-directory vidframe--this-file-cache 'recursive))

(defun vidframe-setup (file)
  "Set up the vidframe for FILE."
  (setq vidframe--this-file-cache
	(expand-file-name (concat (file-name-base file) "/")
			  vidframe-cache-dir))
  (unless (file-exists-p vidframe--this-file-cache)
    (mkdir vidframe--this-file-cache 'p)
    (vidframe--dump-frames file)
    (vidframe--extract-sound file))
  (setq vidframe-current-interval (/ 1 (vidframe--get-fps file)))
  (setq vidframe-frames
	(stream-of-directory-files
	 (expand-file-name "frames/" vidframe--this-file-cache)
	 'full))
  (while (null (stream-first vidframe-frames))
    (sit-for 1))
  (setq vidframe-remaining-frames vidframe-frames)
  (setq vidframe-image (stream-first vidframe-remaining-frames))

  ;; (pop-to-buffer (get-buffer-create vidframe--buffer))
  (empv-exit)
  (empv-enqueue
   (format "%s/audio/audio.aac" vidframe--this-file-cache))
  (setq vid-playing-p t)
  (vidframe-play))
;; (vidframe-setup "/home/mitch/testdir/conan1.mp4")

;;;###autoload
(defvar-keymap vidframe-mode-map
  "SPC" #'vidframe-pause-or-resume)

;;;###autoload
(define-derived-mode vidframe-mode fundamental-mode "VidFrame"
  "Major mode for playing video files."
  :group 'vidframe
  (local-set-key "p" 'vidframe-pause-or-resume)
  (setq vidframe--buffer (current-buffer))
  (vidframe-setup (expand-file-name (buffer-file-name))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mp4\\'" . vidframe-mode))

(provide 'vidframe)
;;; vidframe.el ends here
