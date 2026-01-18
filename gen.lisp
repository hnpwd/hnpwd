;;;; HN Personal Websites Directory Generator
;;;; ========================================

(require "uiop")
(load "util")

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
  (fstr "<a href=\"~a\">~a</a>" url (parse-short-domain url)))

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
    (format s "~a" (make-nav-link (getf item :site) "Website"))
    (format s "~a" (make-nav-link (getf item :blog) "Blog"))
    (format s "~a" (make-nav-link (getf item :about) "About"))
    (format s "~a" (make-nav-link (getf item :now) "Now"))
    (format s "~a" (make-nav-link (getf item :feed) "Feed"))
    (format s "~a" (make-user-link (getf item :hnuid) "HN"))
    (format s "        </nav>~%")
    (format s "~a" (make-site-bio (getf item :bio)))
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
    (format s "        <a href=\"https://github.com/hnpwd/hnpwd#readme\">README</a>~%")
    (format s "        <a href=\"hnpwd.opml\">OPML</a>~%")
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

(defvar *gen-mode* t
  "Run main function iff T.  Should be set to NIL in tests.")

(defun gen ()
  "Create artefacts."
  (let ((entries (read-entries "pwd.lisp")))
    (write-file "web/hnpwd.opml" (make-opml entries))
    (write-file "web/index.html" (make-html entries))))

(when *gen-mode*
  (gen))
