;;;; HN Personal Websites Directory Generator
;;;; ========================================

(require "uiop")


;;; General Definitions
;;; -------------------

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

(defun has-duplicates-p (items)
  "Check if there are duplicates in the given items."
  (< (length (remove-duplicates items :test #'equal))
     (length items)))

(defun string-starts-with-p (prefix string)
  "Check if the given string starts with the given prefix."
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

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
    (when (string-starts-with-p "www." host)
      (setf host (subseq host 4)))
    host))


;;; Validations
;;; -----------

(defun validate-name-order (items)
  "Check that entries are arranged in the order of names."
  (let ((prev-name)
        (curr-name)
        (errors))
    (dolist (item items)
      (setf curr-name (getf item :name))
      (when (and prev-name (string< curr-name prev-name))
        (push (fstr "~a / ~a: Entries must be sorted alphabetically by name"
                    prev-name curr-name) errors))
      (setf prev-name curr-name))
    (reverse errors)))

(defun validate-bio-basic (items)
  "Check that bio entries look good."
  (let ((max-len 80)
        (bio)
        (errors))
    (dolist (item items)
      (when (setf bio (getf item :bio))
        (when (> (length bio) max-len)
          (push (fstr "~a: Bio of length ~a exceeds maximum allowed length ~a"
                      (getf item :name) (length bio) max-len) errors))
        (when (position #\& bio)
          (push (fstr "~a: Bio must not contain ampersand (&)"
                      (getf item :name)) errors))
        (when (char/= (char bio (1- (length bio))) #\.)
          (push (fstr "~a: Bio must end with a full stop (period)"
                      (getf item :name)) errors))
        (when (search ", and" bio)
          (push (fstr (jstr "~a: Bio must not contain comma before 'and' ("
                            "avoid Oxford comma)") (getf item :name)) errors))))
    (reverse errors)))

(defun validate-bio-spacing (items)
  "Check if the bio uses double spacing convention."
  ;; Checking the double-spacing convention is a non-trivial problem
  ;; and in fact Emacs has entire packages dedicated to this problem.
  ;; Here we implement a very trivial algorithm that is not exhaustive
  ;; in its checks but catches most of the typical problems.
  ;; Essentially, this looks for the sequence <lower-case> <full-stop>
  ;; <space> <non-space> and flags it.  It will catch violations like
  ;; "This. That." but it will miss violations like "HN. Rocks."  This
  ;; is good enough for a small project like this.
  (let ((bio)
        (errors))
    (dolist (item items)
      (when (setf bio (getf item :bio))
        (dotimes (i (- (length bio) 3))
          (when (and (not (upper-case-p (char bio i)))
                     (char= (char bio (+ i 1)) #\.)
                     (char= (char bio (+ i 2)) #\Space)
                     (char/= (char bio (+ i 3)) #\Space))
            (push (fstr (jstr "~a ('~a'): A full stop (period) in the middle of "
                              "bio must be followed by two spaces.")
                        (getf item :name) (subseq bio i (+ i 4))) errors)
            (return)))))
    (reverse errors)))

(defun pick-urls (item)
  "Pick all URL values from the given entry."
  (remove nil (list (getf item :site)
                    (getf item :blog)
                    (getf item :feed)
                    (getf item :about)
                    (getf item :now))))

(defun validate-urls (items)
  "Check that root URLs have a trailing slash."
  (let ((errors))
    (dolist (item items)
      (dolist (url (pick-urls item))
        (unless (or (string-starts-with-p "http://" url)
                    (string-starts-with-p "https://" url))
          (push (fstr "~a <~a>: URL must start with 'http://' or 'https://'"
                      (getf item :name) url) errors))
        (when (< (count #\/ url) 3)
          (push (fstr "~a <~a>: URL must have at least three slashes"
                      (getf item :name) url) errors))))
    (reverse errors)))

(defun validate-unique-urls (items)
  "Check that there are no duplicates in the URLs within the same entry."
  (let ((errors))
    (dolist (item items)
      (when (has-duplicates-p (remove nil (pick-urls item)))
        (push (fstr "~a: Entry must not have duplicate URLs"
                    (getf item :name)) errors)))
    (reverse errors)))

(defun validate-hn-uids (items)
  "Check that HN user IDs are not links."
  ;; Quoting HN registration error due to invalid characters:
  ;;
  ;; "Usernames can only contain letters, digits, dashes and
  ;; underscores, and must be between 2 and 15 characters long. Please
  ;; choose another."
  ;;
  ;; While we don't care about performing a strict check of HN
  ;; usernames (a human reviewer will do that by actually visiting the
  ;; HN user profile before approving a new entry), we do want to give
  ;; users immediate feedback during CI checks when they inadvertently
  ;; enter HN profile URL instead of their HN username.
  (let ((errors))
    (dolist (item items)
      (when (or (position #\: (getf item :hnuid))
                (position #\. (getf item :hnuid)))
        (push (fstr "~a: HNUID must be just the HN username"
                    (getf item :name)) errors)))
    (reverse errors)))

(defun validate (items)
  (let ((errors (append (validate-name-order items)
                        (validate-urls items)
                        (validate-unique-urls items)
                        (validate-bio-basic items)
                        (validate-bio-spacing items)
                        (validate-hn-uids items))))
    (when (consp errors)
      (loop for error in errors
            do (format *error-output* "ERROR: ~a~%" error))
      (uiop:quit 1))))


;;; Tool Definitions
;;; ----------------

(defun read-entries ()
  "Read website entries from the data file."
  (remove-if
   (lambda (item)
     (or (equal item '(:end))
         (string= (getf item :site) "")))
   (read-list "pwd.lisp")))

(defun select-opml-entries (items)
  "Select entries that can be included in OPML."
  (remove-if-not (lambda (item)
                   (and (getf item :name)
                        (getf item :feed)
                        (getf item :site))) items))

(defun make-opml-outline (item)
  "Create an outline element for the specified website entry."
  (fstr "      <outline type=\"rss\" text=\"~a\" title=\"~a\" xmlUrl=\"~a\" htmlUrl=\"~a\"/>~%"
        (getf item :name)
        (getf item :name)
        (getf item :feed)
        (getf item :site)))

(defun make-opml (items)
  "Create OPML file for all feeds."
  (setf items (select-opml-entries items))
  (with-output-to-string (s)
    (format s "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%")
    (format s "<opml version=\"2.0\">~%")
    (format s "  <head>~%")
    (format s "    <title>HN Personal Websites</title>~%")
    (format s "    <dateCreated>~a</dateCreated>~%"
            (format-date (encode-universal-time 0 0 0 14 1 2026 0)))
    (format s "    <dateModified>~a</dateModified>~%"
            (format-date (get-universal-time)))
    (format s "  </head>~%")
    (format s "  <body>~%")
    (format s "    <!-- ~a entries -->~%" (length items))
    (format s "    <outline text=\"HN Personal Websites\" title=\"HN Personal Websites\">~%")
    (loop for item in items
          do (format s "~a" (make-opml-outline item)))
    (format s "    </outline>~%")
    (format s "  </body>~%")
    (format s "</opml>~%")))

(defun make-site-link (url)
  (fstr "<a href=\"~a\">~a</a>" url (parse-host url)))

(defun make-nav-link (href text)
  "Create an HTML link."
  (with-output-to-string (s)
    (when href
      (format s "          ~a<a href=\"~a\">~a</a>~%"
              (if (string= text "Website") "" "| ") href text))))

(defun make-user-link (user text)
  "Create an HTML link."
  (with-output-to-string (s)
    (when user
      (format s "          | <a href=\"https://news.ycombinator.com/user?id=~a\">~a</a>~%" user text))))

(defun make-site-bio (bio)
  "Create HTML snippet to display bio."
  (with-output-to-string (s)
    (when bio
      (format s "        <p>~a</p>~%" bio))))

(defun make-html-card (item)
  "Create an HTML section for the specified website entry."
  (with-output-to-string (s)
    (format s "      <section>~%")
    (format s "        <h2>~a</h2>~%" (getf item :name))
    (format s "        <h3>~a</h3>~%" (make-site-link (getf item :site)))
    (format s "        <nav>~%")
    (format s (make-nav-link (getf item :site) "Website"))
    (format s (make-nav-link (getf item :blog) "Blog"))
    (format s (make-nav-link (getf item :about) "About"))
    (format s (make-nav-link (getf item :now) "Now"))
    (format s (make-nav-link (getf item :feed) "Feed"))
    (format s (make-user-link (getf item :hnuid) "HN"))
    (format s "        </nav>~%")
    (format s (make-site-bio (getf item :bio)))
    (format s "      </section>~%")))

(defun make-html (items)
  "Create HTML page with all website entries."
  (with-output-to-string (s)
    (format s "<!DOCTYPE html>~%")
    (format s "<html lang=\"en\">~%")
    (format s "  <head>~%")
    (format s "    <title>HN Personal Websites Directory</title>~%")
    (format s "    <meta charset=\"UTF-8\">~%")
    (format s "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">~%")
    (format s "    <link rel=\"stylesheet\" href=\"style.css\">~%")
    (format s "    <link rel=\"icon\" type=\"image/png\" href=\"favicon.png\">~%")
    (format s "    <script src=\"script.js\"></script>~%")
    (format s "  </head>~%")
    (format s "  <body>~%")
    (format s "    <h1>HN Personal Websites</h1>~%")
    (format s "    <div>(~a websites)</div>~%" (length items))
    (format s "    <main>~%")
    (loop for item in items
          do (format s "~a" (make-html-card item)))
    (format s "    </main>~%")
    (format s "    <footer>~%")
    (format s "      <hr>~%")
    (format s "      <nav>~%")
    (format s "        <a href=\"https://github.com/hnpwd/hnpwd.github.io#readme\">README</a>~%")
    (format s "        <a href=\"pwd.opml\">OPML</a>~%")
    (format s "        <a href=\"https://web.libera.chat/#hnpwd\">IRC</a>~%")
    (format s "      </nav>~%")
    (format s "      <p>~%")
    (format s "        This website is not affiliated with Y&nbsp;Combinator.~%")
    (format s "        This is a community-maintained directory of~%")
    (format s "        personal websites by active members of the HN community.~%")
    (format s "      </p>~%")
    (format s "      <p>~%")
    (format s "        Last updated on ~a.~%" (format-date (get-universal-time)))
    (format s "      </p>~%")
    (format s "    </footer>~%")
    (format s "  </body>~%")
    (format s "</html>~%")))

(defvar *main-mode* t
  "Run main function iff T.  Should be set to NIL in tests.")

(defun validate-only-p (&optional (args (uiop:command-line-arguments)))
  "Check if --validate-only flag was passed on command line."
  (find "--validate-only" args :test #'string=))

(defun main ()
  "Create artefacts."
  (let ((entries (read-entries)))
    (validate entries)
    (unless (validate-only-p)
      (write-file "pwd.opml" (make-opml entries))
      (write-file "index.html" (make-html entries)))))

(when *main-mode*
  (main))
