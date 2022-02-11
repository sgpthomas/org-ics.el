;;; org-ics.el --- A tool to import .ics calendar files into Org files. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (request s dash f)

;;; Code:
(require 'request)
(require 's)
(require 'dash)
(require 'f)

(defun org-ics/split (string)
  "Split the ics line `string' into key and value."
  (let ((parts (s-split-up-to ":" string 1)))
    `(,(car parts) . ,(cadr parts))))

(defun org-ics/split-lines (lines acc build)
  "Recursively go through .ics file and parse into 'lines'.
   This is trickier than it should be because the .ics format
   only allows lines of 75 characters. Any text that's longer
   must be split up into multiple lines. This makes parsing more
   difficult.

   This works by going through the file backwards. If a line begins
   with a space, we add it to the `build' list. This indicates that
   we are currently building up a new line. When we finally get to a
   normal line, we add the stuff in `build' to the constructed line."

  (if (null lines)
      acc
    (let* ((line (car lines))
	   (rest (cdr lines)))
      (if (s-starts-with? " " line)
	  (org-ics/split-lines rest
			       acc
			       (cons (s-chop-prefix " " line) build))
	(let* ((build-acc (s-join "" (cons line build)))
	       (acc (if (null build)
			(cons line acc)
		      (cons build-acc acc))))
	  (org-ics/split-lines rest acc '()))))))

(defun org-ics/prepare-input (text)
  "This function normalizes the input and produces a list where an
   element in the list corresponds to an entry in the .ics file."

  (org-ics/split-lines (reverse (s-split "\n" text)) '() '()))

(defun org-ics/parse (lines acc)
  "Parse an .ics file into a tree."

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
	     (let ((pair `(,key . ,val)))
	       (org-ics/parse (cdr lines) (cons pair acc))))))))

(defun org-ics/unescape (string)
  "Unescape commas and newlines."
  
  (let* ((a (replace-regexp-in-string "\\\\n" "\n" string))
	 (b (replace-regexp-in-string "\\\\\\(.\\)" "\\1" a)))
    b))

(defun org-ics/to-org-event (event)
  "Produce a string representing an org event from an event structure."

  (let* ((summary (cdr (assoc "SUMMARY" event)))
	 (uid (cdr (assoc "UID" event)))
	 (dtstart (cdr (--find (s-starts-with? "DTSTART" (car it)) event)))
	 (dtend (cdr (--find (s-starts-with? "DTEND" (car it)) event)))
	 (descr (cdr (assoc "DESCRIPTION" event)))
	 (location (cdr (assoc "LOCATION" event))))
    (s-join "\n"
	    (list (format "* %s" summary)
		  ":PROPERTIES:"
		  ":END:"
		  ""
		  (org-ics/unescape descr)))))

(defun org-ics/process (text)
  "Process the text of an .ics file into a .org file."

  (let* ((input (org-ics/prepare-input text))
	 (data (org-ics/parse input '()))
	 (cal (cdr (assoc "VCALENDAR" data)))
	 (events (--filter (s-equals? (car it) "VEVENT") cal))
	 (org-events (--map (org-ics/to-org-event (cdr it)) events))
	 (res org-events))
    (s-concat
     "#+TITLE: Calendar\n"
     "#+CATEGORY: Calendar\n"
     "#+FILETAGS: EVENT\n"
     "\n"
     (s-join "\n" res))))

(defun org-ics/import-ics-url-to-org (ics-url org-file-name)
  "Download .ics file form `ics-url' and save to `org-file-name'."

  (let ((msg '()))
    (request ics-url
      :sync t
      :complete (cl-function (lambda (&key data &allow-other-keys) (setq msg data))))
    (with-current-buffer (get-buffer-create org-file-name)
      (erase-buffer)
      (insert (org-ics/process msg)))))

(defun org-ics/import-ics-file-to-org (ics-file org-file-name)
  "Download .ics file form `ics-url' and save to `org-file-name'."

  (let ((text (f-read-text ics-file)))
    (with-current-buffer (get-buffer-create org-file-name)
      (erase-buffer)
      (insert (org-ics/process text)))))

;; (org-ics/import-ics-url-to-org
;;  "https://calendar.google.com/calendar/ical/sgtpeacock%40utexas.edu/private-6382215cc9d4e1bb8659bbe82e5f7a0a/basic.ics"
;;  "test.org"
;;  )
;; (org-ics/import-ics-file-to-org "~/OrgFiles/org-data/test.ics" "test.org")

(provide 'org-ics)
