;;; org-ics.el --- A tool to import .ics calendar files into Org files. -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Samuel Thomas

;; Author: Samuel Thomas <sgt@cs.utexas.edu>
;; Package-Requires: (request s dash f)

;;; Code:
(require 'request)
(require 's)
(require 'dash)
(require 'f)

;; ==== User Configuration ==== ;;

(defcustom org-ics/calendars nil
  "List of calendars to load into org files.")

(cl-defstruct org-ics/calendar
  "Represents a user defined calendar."
  name file url destination category file-tag)

(defun org-ics/parse-calendar (cal)
  (make-org-ics/calendar
   :name (plist-get cal :name)
   :file (plist-get cal :file)
   :url (plist-get cal :url)
   :destination (plist-get cal :destination)
   :category (plist-get cal :category)
   :file-tag (plist-get cal :file-tag)))

;; ==== ICS Parsing ==== ;;

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

;; ==== Event Structure ==== ;;

;;   spec: https://datatracker.ietf.org/doc/html/rfc5545#section-3.8.5.3
(cl-defstruct org-ics/repeat
  "A structure representing a repeat definition in an ics file."
  date-start
  freq week-start until
  byyear bymonth byday
  count
  (interval 1))

(defun org-ics/parse-repeat (dtstart repeat)
  "Parse a repeat string into a repeat struct."

  (if (or (null dtstart) (null repeat))
      (make-org-ics/repeat)
    (let* (;; (dtstart (decode-time))
	   ;; (repeat "FREQ=WEEKLY;WKST=SU;UNTIL=20220507T045959Z;BYDAY=TU,TH")
	   (parts (s-split ";" repeat))
	   (parts (--map (s-split "=" it) parts))
	   (until (cadr (assoc "UNTIL" parts)))
	   (byday (cadr (assoc "BYDAY" parts))))
      (make-org-ics/repeat
       :date-start dtstart
       :freq (cadr (assoc "FREQ" parts))
       :week-start (cadr (assoc "WKST" parts))
       :until (when until (iso8601-parse until))
       :byyear (cadr (assoc "BYYEAR" parts))
       :bymonth (cadr (assoc "BYMONTH" parts))
       :byday (when byday (s-split "," byday))
       :count (cadr (assoc "COUNT" parts))
       :interval (cadr (assoc "INTERVAL" parts))))))

(defun org-ics/repeat-string (repeat-data)
  (let* ((interval (org-ics/repeat-interval repeat-data))
	 (freq (org-ics/repeat-freq repeat-data))
	 (freq-letter (cond ((s-equals? freq "WEEKLY") "w")
			    ((s-equals? freq "DAILY") "d")
			    ((s-equals? freq "YEARLY") "y"))))
    (format "+%s%s" interval freq)))

(cl-defstruct org-ics/event
  "Represents an .ics event."
  summary description
  id location status
  start-date end-date
  repeat raw-repeat)

(defun org-ics/parse-event (event)
  "Parse an event association list into an event structure."
  (let* ((start-date (cdr (--find (s-starts-with? "DTSTART" (car it)) event)))
	 (start-date (iso8601-parse start-date))
	 (end-date (cdr (--find (s-starts-with? "DTEND" (car it)) event)))
	 (end-date (iso8601-parse end-date)))
    (make-org-ics/event
     :summary (cdr (assoc "SUMMARY" event))
     :description (cdr (assoc "DESCRIPTION" event))
     :id (cdr (assoc "UID" event))
     :location (cdr (assoc "LOCATION" event))
     :status (cdr (assoc "STATUS" event))
     :start-date start-date
     :end-date end-date
     :repeat (org-ics/parse-repeat
	      start-date
	      (cdr (assoc "RRULE" event)))
     :raw-repeat (cdr (assoc "RRULE" event)))))

(defun org-ics/to-org-event (event)
  "Produce a string representing an org event from an event structure."

  (let* ((summary (org-ics/event-summary event))
	 (uid (org-ics/event-id event))
	 (dtstart (org-ics/event-start-date event))
	 (dtend (org-ics/event-end-date event))
	 (descr (org-ics/event-description event))
	 (location (org-ics/event-location event))
	 (status (org-ics/event-status event))
	 (repeat (org-ics/event-repeat event)))
    (s-join "\n"
	    (list (format "* %s" summary)
		  ":PROPERTIES:"
		  (format ":ID: %s" uid)
		  (format ":LOCATION: %s" location)
		  (format ":STATUS: %s" status)
		  ":END:"
		  ""
		  (format "%s"
			  (org-ics/parse-date dtstart dtend repeat))
		  (format "repeat: %s" repeat)
		  (format "raw: %s" (org-ics/event-raw-repeat event))
		  (org-ics/unescape descr)))))

;; ==== ====

(defun org-ics/unescape (string)
  "Unescape commas and newlines."
  
  (let* ((a (replace-regexp-in-string "\\\\n" "\n" string))
	 (b (replace-regexp-in-string "\\\\\\(.\\)" "\\1" a)))
    b))

(defun org-ics/format (time-string)
  (let* ((tz-delta (make-decoded-time :hour -6))
	 (time (decoded-time-add time-string tz-delta))
	 (year (decoded-time-year time))
	 (month (decoded-time-month time))
	 (day (decoded-time-day time))
	 (hour (decoded-time-hour time))
	 (min (decoded-time-minute time))
	 (tz (decoded-time-zone time))

	 (date (if (or (null year) (null month) (null day))
		   ""
		 (format "%04d-%02d-%02d" year month day)))
	 (time (if (or (null hour) (null min))
		   ""
		 (format "%02d:%02d" hour min))))
    (cons date time)))

(defun org-ics/parse-date (start end &optional repeat)
  (let* ((start-str (org-ics/format start))
	 (end-str (org-ics/format end))
	 (repeat-str (if (null repeat) ""
		       ""
		       ;; (org-ics/repeat-string (org-ics/parse-repeat repeat start))
		       )))
    (if (s-equals? (car start-str) (car end-str))
	(format "<%s %s-%s %s>"
		(car start-str) (cdr start-str) (cdr end-str)
		repeat-str)
      (format "<%s %s>--<%s %s>"
	      (car start-str) (cdr start-str)
	      (car end-str) (cdr end-str)))))

(defun org-ics/event-filter (event)
  "Make sure that the event is not happening more than a week in the past."

  (let* ((dtstart (org-ics/event-start-date event))
	 (delta (make-decoded-time :day 7))
	 (start (decoded-time-add dtstart delta))
	 (current-time (decode-time)))
    (time-less-p (encode-time current-time) (encode-time start))))

(defun org-ics/expand-repeat (event)
  "Expand events that repeat multiple times per week into separate events."

  (let* (;; (repeat (cdr (assoc "RRULE" event)))
	 (event '(event))
	 (repeat "FREQ=WEEKLY;WKST=SU;UNTIL=20220507T045959Z;BYDAY=TU,TH")
	 (data (org-ics/parse-repeat repeat))
	 (byday-str (cadr (assoc "BYDAY" data)))
	 (byday (s-split "," byday-str))
	 )
    (if (null byday)
	(list event)
      (--map (format "BYDAY=%s" it) byday))
    )
("BYDAY=TU" "BYDAY=TH")

  )

(defun org-ics/process (text)
  "Process the text of an .ics file into a .org file."

  (let* ((input (org-ics/prepare-input text))
	 (data (org-ics/parse input '()))
	 (cal (cdr (assoc "VCALENDAR" data)))
	 (events (--filter (s-equals? (car it) "VEVENT") cal))
	 (events (--map (org-ics/parse-event (cdr it)) events))
	 (events (-filter 'org-ics/event-filter events))
	 ;; (events (-flatten-n 1 (-map 'org-ics/expand-repeat events)))
	 (org-events (-map 'org-ics/to-org-event events))
	 (res org-events))
    (s-concat
     "#+TITLE: Calendar\n"
     "#+CATEGORY: Calendar\n"
     "#+FILETAGS: EVENT\n"
     "\n"
     (s-join "\n" res))))

;; ==== User interface ==== ;;

(defun org-ics/import-ics-url-to-org (cal)
  "Download .ics file form `ics-url' and save to `org-file-name'."

  (let ((msg '()))
    (request (org-ics/calendar-url cal)
      :sync t
      :complete (cl-function (lambda (&key data &allow-other-keys) (setq msg data))))
    (with-current-buffer (find-file-noselect (org-ics/calendar-destination cal))
      (erase-buffer)
      (insert (org-ics/process msg))
      (save-buffer))))

(defun org-ics/import-ics-file-to-org (cal)
  "Download .ics file form `ics-url' and save to `org-file-name'."

  (let ((text (f-read-text (org-ics/calendar-file cal))))
    (with-current-buffer (find-file-noselect org-ics/calendar-destination cal)
      (erase-buffer)
      (insert (org-ics/process text))
      (save-buffer))))

(defun org-ics/import-all ()
  "Import all calendars defined in `org-ics/calendars'."
  (interactive)
  (let* ((cals (-map 'org-ics/parse-calendar org-ics/calendars)))
    (--map (cond ((org-ics/calendar-file it) (org-ics/import-ics-file-to-org it))
		 ((org-ics/calendar-url it) (org-ics/import-ics-url-to-org it)))
	   cals)))

;; (org-ics/import-ics-url-to-org
;;  "https://calendar.google.com/calendar/ical/sgtpeacock%40utexas.edu/private-6382215cc9d4e1bb8659bbe82e5f7a0a/basic.ics"
;;  "test.org"
;;  )
;; (org-ics/import-ics-file-to-org "~/OrgFiles/org-data/test.ics" "test.org")


(provide 'org-ics)
