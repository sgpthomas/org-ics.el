;;; org-ics.el --- A tool to import .ics calendar files into Org files. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (request s dash f)

;;; Code:
(require 'request)
(require 's)
(require 'dash)
(require 'f)

(defun org-ics/split-lines (lines acc build)
  (if (null lines)
      acc
    (let* ((line (car lines))
	   (rest (cdr lines)))
      ;; (message "'%s'" line)
      (if (s-starts-with? " " line)
	  (progn
	    ;; (message "yes")
	    (org-ics/split-lines rest
				 acc
				 (cons (s-chomp (s-trim line)) build)))
	(let* ((line (s-trim line))
	       (build-acc (s-join "" (cons line build)))
	       (acc (if (null build)
			(cons line acc)
		      (cons build-acc acc))))
	  (org-ics/split-lines rest acc '()))))))

(defun org-ics/prepare-input (text)
  (org-ics/split-lines (reverse (s-split "\n" text)) '() '()))

;; (with-current-buffer "test.org"
;;   (erase-buffer)
;;   (insert (s-join "\n"
;; 		  (org-ics/prepare-input (f-read-text "~/OrgFiles/org-data/test.ics")))))

(defun org-ics/split (string)
  (let ((parts (s-split-up-to ":" string 1)))
    `(,(car parts) . ,(cadr parts))))

(defun org-ics/parse (lines acc)
  (if (null lines)
      (reverse acc)
    (let* ((line (car lines))
	   (pair (org-ics/split line))
	   (key (car pair))
	   (val (cdr pair)))
      (cond ((s-equals? key "BEGIN")
	     (let ((section (org-ics/parse (cdr lines) '())))
	       (org-ics/parse (car section) (cons `(,val . ,(cdr section)) acc))))
	    ((s-equals? key "END")
	     (cons (cdr lines) (reverse acc)))
	    (t
	     (let ((pair `(,key . ,(format "%s" val))))
	       (org-ics/parse (cdr lines) (cons pair acc))))))))

(defun org-ics/to-org-event (event)
  (let* ((summary (cdr (assoc "SUMMARY" event)))
	 (uid (cdr (assoc "UID" event)))
	 (dtstart (cdr (--find (s-starts-with? "DTSTART" (car it)) event)))
	 (dtend (cdr (--find (s-starts-with? "DTEND" (car it)) event)))
	 (descr (assoc "DESCRIPTION" event))
	 (location (cdr (assoc "LOCATION" event)))
	 )
    (s-join "\n"
	    (-concat
	     (list (format "* %s" summary)
		   (format ":PROPERTIES:")
		   (format ":END:")
		   ""
		   ;; (format "%s" descr)
		   )
	     (list (format "%s" (s-split "\n" (cdr descr))))
	     ;; 
	     ))))

(defun org-ics/process (text)
  (let* ((input (org-ics/prepare-input text))
	 (data (org-ics/parse input '()))
	 (cal (cdr (assoc "VCALENDAR" data)))
	 (events (--filter (s-equals? (car it) "VEVENT") cal))
	 (org-events (--map (org-ics/to-org-event (cdr it)) events))
	 (res org-events))
    (s-join "\n" res)
    )
  )

(defun org-ics/import-ics-url-to-org (ics-url org-file-name)
  "Download .ics file form `ics-url' and save to `org-file-name'."
  (let ((msg '()))
    (request ics-url
      :sync t
      :complete (cl-function (lambda (&key data &allow-other-keys) (setq msg data))))
    (with-current-buffer (get-buffer-create org-file-name)
      (erase-buffer)
      (insert (org-ics/process msg))
      (pp-buffer))))

(defun org-ics/import-ics-file-to-org (ics-file org-file-name)
  "Download .ics file form `ics-url' and save to `org-file-name'."
  (let ((text (f-read-text ics-file)))
    (with-current-buffer (get-buffer-create org-file-name)
      (erase-buffer)
      (insert (org-ics/process text))
      (pp-buffer))))

;; (org-ics/import-ics-url-to-org
;;  "https://calendar.google.com/calendar/ical/sgtpeacock%40utexas.edu/private-6382215cc9d4e1bb8659bbe82e5f7a0a/basic.ics"
;;  "test.org"
;;  )
;; (org-ics/import-ics-file-to-org "~/OrgFiles/org-data/test.ics" "test.org")

(provide 'org-ics)
