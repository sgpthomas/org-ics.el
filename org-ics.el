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
  (let ((parts (s-split ":" string)))
    `(,(car parts) . ,(cadr parts))))

;; ([string], parse) -> ([string], res)
(defun org-ics/build (lines acc)
  (if (null lines) acc
    ;; else
    (let* ((head (car lines))
	   (rest (cdr lines))
	   (sp (org-ics/split head))
	   (field (car sp))
	   (data (cdr sp))
	   (res (cond ((s-equals? field "BEGIN")
		       (let* ((nxt (org-ics/build rest '()))
			      (rest (car nxt))
			      (parse (cdr nxt)))
			 ;; (message "%s" nxt)
			 ;; (cons (car aft)
			 ;;       (append acc
			 ;; 	       `((,data . ,(cdr nxt)))
			 ;; 	       ()
			 ;; 	       ))
			 (append acc
				 parse
				 (org-ics/build rest acc))))
		      ((s-equals? field "END")
		       (cons rest acc))
		      (t
		       ;; (cons rest (append acc `(,sp)))
		       (org-ics/build rest (append acc `(,sp)))
		       ))))
      ;; (org-ics/build (car res) (cdr res))
      res
      )))

(defun org-ics/process (ics-data)
  (let ((res (cadr (org-ics/build (s-split "\n" ics-data) '()))))
    ;; (format "%s" (s-join "\n" (--map (format "%s" it) (cdr res))))
    (format "%s" res)
    )
  )

;; (defun org-ics/process (ics-data)
;;   (let* ((lines (s-split "\n" ics-data))
;; 	 (res (--reduce-from (cond ((s-starts-with? "BEGIN" it)
;; 				    (cons (append (car acc) `(,(cdr (org-ics/split it))))
;; 					  (cdr acc)))
;; 				   ((s-starts-with? "END" it)
;; 				    (cons '()
;; 					  (append (cdr acc) (list (car acc)))))
;; 				   (t
;; 				    (cons (append (car acc) `(,(org-ics/split it)))
;; 					  (cdr acc))
;; 				    ))
;; 			     '(() . ())
;; 			     lines
;; 			     )))
;;     (format "%s" (s-join "\n" (--map (format "%s" it) (cdr res))))
;;     )
;;   )

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
      (insert (org-ics/process text))
      (pp-buffer))))


;; (org-ics/import-ics-url-to-org
;;  "https://calendar.google.com/calendar/ical/sgtpeacock%40utexas.edu/private-6382215cc9d4e1bb8659bbe82e5f7a0a/basic.ics"
;;  "test.org"
;;  )
;; (org-ics/import-ics-file-to-org "~/OrgFiles/org-data/test.ics"
;; 				"test.org")
(provide 'org-ics)
