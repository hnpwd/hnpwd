;;;; HNPWD Utilities
;;;; ===============

(defun fstr (fmt &rest args)
  "Format string using specified format and arguments."
  (apply #'format nil fmt args))

(defun jstr (&rest strings)
  "Join strings into a single string."
  (apply #'concatenate 'string strings))

(defun write-file (filename text)
  "Write text to file and close the file."
  (with-open-file (f filename :direction :output :if-exists :supersede)
    (write-sequence text f)))

(defun read-list (filename)
  "Read Lisp file."
  (with-open-file (f filename) (read f)))

(defun read-entries (filename)
  "Read website entries from the data file."
  (remove-if
   (lambda (item)
     (or (equal item '(:end))
         (string= (getf item :site) "")))
   (read-list filename)))

(defun has-duplicates-p (items)
  "Check if there are duplicates in the given items."
  (< (length (remove-duplicates items :test #'equal))
     (length items)))

(defun string-starts-with-p (prefix string)
  "Check if the given string starts with the given prefix."
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun string-trim-prefix (prefix string)
  "Remove the given prefix from the given string."
  (if (string-starts-with-p prefix string)
      (subseq string (length prefix))
      string))

(defun weekday-name (weekday-index)
  "Given an index, return the corresponding day of week."
  (nth weekday-index '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))

(defun month-name (month-number)
  "Given a number, return the corresponding month."
  (nth (1- month-number) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                           "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

(defun format-date (universal-time)
  "Convert universal-time (integer) to RFC-2822 date string."
  (multiple-value-bind (second minute hour date month year day dst)
      (decode-universal-time universal-time 0)
    (declare (ignore dst))
    (fstr "~a, ~2,'0d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d UTC"
          (weekday-name day) date (month-name month) year
          hour minute second)))

(defun parse-host (url)
  "Extract the domain name from the given URL."
  (let* ((host-start (+ (search "://" url) 3))
         (host-end (position #\/ url :start host-start))
         (host (subseq url host-start host-end)))
    (setf host (string-trim-prefix "www." host))
    (setf host (string-trim-prefix "blog." host))))
