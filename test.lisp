;;;; Tests
;;;; =====

;;;; The tests in this file test the Lisp source code in gen.lisp.
;;;; These tests does not validate user data in pwd.lisp.
;;;; Validation of user data in pwd.lisp is done by gen.lisp itself
;;;; just before generating the HTML and OPML artefacts.

(require "uiop")


;;; Test Definitions
;;; ----------------

(defparameter *pass* 0)
(defparameter *fail* 0)
(defvar *quit* nil)

(defun set-dev-path ()
  "Utility function to be run manually in development environment."
  (setf *default-pathname-defaults* (truename "~/git/hnpwd/")))

(defmacro test-case (name &body body)
  "Execute a test case and print pass or fail status."
  `(progn
     (let ((test-name (string-downcase ',name)))
       (format t "~&~a: " test-name)
       (handler-case (progn ,@body)
         (:no-error (c)
           (declare (ignore c))
           (incf *pass*)
           (format t "pass~%")
           t)
         (error (c)
           (incf *fail*)
           (format t "FAIL~%")
           (format t "~&  ~a: error: ~a~%" test-name c))))))

(defun test-done ()
  "Print test statistics."
  (format t "~&~%PASS: ~a~%" *pass*)
  (when (plusp *fail*)
    (format t "~&FAIL: ~a~%" *fail*))
  (when *quit*
    (format t "~&~%DONE~%~%")
    (uiop:quit (if (zerop *fail*) 0 1)))
  (zerop *fail*))


;;; Begin Test Cases
;;; ----------------

(defvar *main-mode* nil)
(load "gen.lisp")


;;; Test Cases
;;; ----------

(test-case fstr
  (assert (string= (fstr "") ""))
  (assert (string= (fstr "~a" "hello") "hello"))
  (assert (string= (fstr "~a ~a" "hello" "world") "hello world")))

(test-case write-file
  (write-file "tmp.txt" "foo")
  (let ((text (uiop:read-file-string "tmp.txt")))
    (delete-file "tmp.txt")
    (assert (string= text "foo"))))

(test-case read-list
  (write-file "tmp.txt" "((:a \"apple\") (:b \"ball\"))")
  (let ((data (read-list "tmp.txt")))
    (delete-file "tmp.txt")
    (assert (equal data '((:a "apple") (:b "ball"))))))

(test-case has-duplicates-p
  (assert (has-duplicates-p '(10 10)))
  (assert (has-duplicates-p '(10 20 30 40 10 50)))
  (assert (not (has-duplicates-p '())))
  (assert (not (has-duplicates-p '(10))))
  (assert (not (has-duplicates-p '(10 20 30)))))

(test-case string-starts-with-p
  (assert (string-starts-with-p "" ""))
  (assert (string-starts-with-p "" "foo"))
  (assert (string-starts-with-p "foo" "foo"))
  (assert (string-starts-with-p "foo" "foobar"))
  (assert (not (string-starts-with-p "foo" "")))
  (assert (not (string-starts-with-p "foo" "barfoo"))))

(test-case format-date
  (assert (string= (format-date (encode-universal-time 0 0 0 14 1 2026 0))
                   "Wed, 14 Jan 2026 00:00:00 UTC"))
  (assert (string= (format-date (encode-universal-time 0 0 0 14 1 2026 5))
                   "Wed, 14 Jan 2026 05:00:00 UTC"))
  (assert (string= (format-date (encode-universal-time 0 0 0 14 1 2026 -11/2))
                   "Tue, 13 Jan 2026 18:30:00 UTC")))

(test-case parse-host
  (assert (string= (parse-host "https://foo/") "foo"))
  (assert (string= (parse-host "https://foo/bar") "foo"))
  (assert (string= (parse-host "https://foo/bar/") "foo"))
  (assert (string= (parse-host "https://example.com/") "example.com"))
  (assert (string= (parse-host "https://www.example.com/") "example.com"))
  (assert (string= (parse-host "https://www.example.com/bar/") "example.com")))

(test-case validate-name-order
  (assert (=  (length (validate-name-order '((:name "Alice")))) 0))
  (assert (=  (length (validate-name-order '((:name "Alice")
                                             (:name "Bob")))) 0))
  (assert (=  (length (validate-name-order '((:name "Alice")
                                             (:name "Bob")
                                             (:name "Carol")))) 0))
  (assert (=  (length (validate-name-order '((:name "Bob")
                                             (:name "Alice")))) 1))
  (assert (=  (length (validate-name-order '((:name "Bob")
                                             (:name "Alice")
                                             (:name "Carol")))) 1))
  (assert (=  (length (validate-name-order '((:name "Carol")
                                             (:name "Bob")
                                             (:name "Alice")))) 2)))

(test-case validate-bio-basic
  ;; No errors.
  (assert (=  (length (validate-bio-basic '((:bio "foo, bar and baz.")))) 0))
  (assert (=  (length (validate-bio-basic '((:bio "foo, bar & baz.")))) 1))
  ;; 1 error: Ampersand not allowed.
  (assert (=  (length (validate-bio-basic '((:bio "foo, bar & baz.")))) 1))
  ;; 1 error: Full stop missing.
  (assert (=  (length (validate-bio-basic '((:bio "foo, bar and baz")))) 1))
  ;; 1 error: Oxford comma not allowed.
  (assert (=  (length (validate-bio-basic '((:bio "foo, bar, and baz.")))) 1))
  ;; 3 error: Ampersand, Oxford comma and full stop missing.
  (assert (=  (length (validate-bio-basic '((:bio "foo & bar, and baz")))) 3))
  ;; No errors.  Maximum bio length allowed is 80 chars.
  (let ((bio "1234567890123456789012345678901234567890123456789012345678901234567890123456789."))
    (assert (= (length (validate-bio-basic `((:bio ,bio)))) 0)))
  ;; 1 error: Bio length is 81 but maximum length is 80.
  (let ((bio "12345678901234567890123456789012345678901234567890123456789012345678901234567890."))
    (assert (= (length (validate-bio-basic `((:bio ,bio)))) 1))))

(test-case validate-bio-spacing
  (assert (= (length (validate-bio-spacing '((:bio "Foo.  Bar.  Baz.")))) 0))
  (assert (= (length (validate-bio-spacing '((:bio "Foo. Bar.  Baz.")))) 1))
  (assert (= (length (validate-bio-spacing '((:bio "Foo.  Bar.  Baz.")
                                             (:bio "Foo. Bar.  Baz.")
                                             (:bio "Foo. Bar. Baz.")))) 2)))

(test-case validate-urls-protocol
  (assert (= (length (validate-urls '((:site "example/foo/bar/baz/")))) 1))
  (assert (= (length (validate-urls '((:site "www.example.com")))) 2)))

(test-case validate-urls-slashes
  (assert (= (length (validate-urls '((:site "http://site/")))) 0))
  (assert (= (length (validate-urls '((:site "http://site")))) 1))
  (assert (= (length (validate-urls '((:blog "http://blog")))) 1))
  (assert (= (length (validate-urls '((:feed "http://feed")))) 1))
  (assert (= (length (validate-urls '((:about "http://about")))) 1))
  (assert (= (length (validate-urls '((:now "http://now")))) 1))
  (assert (= (length (validate-urls '((:site "http://site/"
                                       :blog "http://blog/"
                                       :feed "http://feed/"
                                       :about "http://about/"
                                       :now "http://now/")))) 0))
  (assert (= (length (validate-urls '((:site "http://site"
                                       :blog "http://blog"
                                       :feed "http://feed"
                                       :about "http://about"
                                       :now "http://now")))) 5)))

(test-case validate-unique-urls
  (assert (= (length (validate-unique-urls '((:site "http://site/")))) 0))
  (assert (= (length (validate-unique-urls '((:site "http://site/"
                                              :blog "http://blog/"
                                              :feed "http://feed/"
                                              :about "http://about/"
                                              :now "http://now/")))) 0))
  (assert (= (length (validate-unique-urls '((:site "http://site/"
                                              :blog "http://site/"
                                              :feed "http://feed/"
                                              :about "http://about/"
                                              :now "http://now/")))) 1))
  (assert (= (length (validate-unique-urls '((:site "http://site/"
                                              :blog "http://blog/"
                                              :feed "http://blog/"
                                              :about "http://about/"
                                              :now "http://now/")))) 1))
  (assert (= (length (validate-unique-urls '((:site "http://site/"
                                              :blog "http://blog/"
                                              :feed "http://feed/"
                                              :about "http://feed/"
                                              :now "http://now/")))) 1))
  (assert (= (length (validate-unique-urls '((:site "http://site/"
                                              :blog "http://blog/"
                                              :feed "http://feed/"
                                              :about "http://about/"
                                              :now "http://about/")))) 1))
  (assert (= (length (validate-unique-urls '((:site "http://site/"
                                              :blog "http://site/"
                                              :feed "http://site/"
                                              :about "http://site/"
                                              :now "http://site/")))) 1)))

(test-case validate-hn-uids
  (assert (= (length (validate-hn-uids '((:hnuid "foo")))) 0))
  (assert (= (length (validate-hn-uids '((:hnuid "foo-bar_baz99")))) 0))
  (assert (= (length (validate-hn-uids '((:hnuid "http://foo/")))) 1))
  (assert (= (length (validate-hn-uids '((:hnuid "https://foo/")))) 1)))

(test-case validate-only-p
  (assert (validate-only-p '("--validate-only")))
  (assert (validate-only-p '("--foo" "--validate-only" "--bar")))
  (assert (not (validate-only-p '())))
  (assert (not (validate-only-p '("--foo" "--bar")))))


;;; The End
;;; -------

(test-done)
