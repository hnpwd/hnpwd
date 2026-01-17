;;;; HNPWD Linter
;;;; ============

(load "util.lisp")

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
  "Run all validations."
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

(defvar *main-mode* t
  "Run main function iff T.  Should be set to NIL in tests.")

(defun main ()
  "Validate directory data."
  (validate (read-entries "pwd.lisp")))

(when *main-mode*
  (main))
