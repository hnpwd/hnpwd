HN Personal Websites Directory
==============================

HN Personal Websites Directory (HNPWD) is a community-maintained
directory of personal websites by members of the Hacker News (HN)
community, along with an OPML file containing their RSS feeds.  Please
visit the following link to view these resources:

* Directory Website: [hnpwd.github.io][site]
* RSS Feeds OPML: [pwd.opml][opml]

[site]: https://hnpwd.github.io/
[opml]: https://hnpwd.github.io/pwd.opml
[lisp]: pwd.lisp


Contents
--------

* [Add Your Website](#add-your-website)
  * [Criteria](#criteria)
  * [Add via PR](#add-via-pr)
  * [Add via Issue](#add-via-issue)
* [FAQ](#faq)
* [Licence](#licence)
* [Contact](#contact)


Add Your Website
----------------

### Criteria

Please check whether a website meets the following criteria before
requesting its inclusion in the directory:

 1. The website must be a personal website, where the author of the
    website has full control of its design and content.  Websites
    hosted on personal domains clearly qualify.  Websites hosted on
    services such as Neocities, GitHub Pages or Codeberg Pages also
    qualify, as these platforms allow full control over the site's
    design.  However, blogs hosted on publishing platforms such as
    Medium or Substack do not qualify.

 2. The website must contain substantive content such as articles,
    blog posts, projects or games developed by the author.  Websites
    that consist only of a CV or portfolio are not suitable for
    inclusion in our directory.  This requirement helps keep the
    directory interesting and meaningful, and prevents it from
    becoming a collection of minimal or placeholder websites with
    little value to the community.


### Add via PR

Add the website details to [pwd.lisp][lisp] and submit a pull request.

An example entry is provided at the bottom of the [pwd.lisp][lisp]
file and can be copied and modified as a starting point.  When
creating a pull request, please follow these guidelines:

 1. Keep all website entries sorted alphabetically by name.

 2. Ensure that the bio text does not exceed 80 characters.

 3. End the bio text with a full stop (period).

 4. Do not use ampersand ('&') in the bio text.  Write the word 'and'
    instead.

 5. Do not use Oxford comma (serial comma) in the bio text.  That is,
    do not write:

    ```
    Writes about C, C++, and Go.
    ```

    Instead, write:

    ```
    Writes about C, C++ and Go.
    ```

 6. Separate two sentences by double spaces.  That is, do not write:

    ```
    Game developer. Writes about computer graphics.
    ```

    Instead, write:

    ```
    Game developer.  Writes about computer graphics.
    ```

 7. Begin URLs with `http://` or `https://`.

 8. If a URL points to the root directory, include the trailing slash.
    For example, write `https://example.com/` (not
    `https://example.com`).  This avoids an extra redirect for many
    clients.

 9. If certain information is not available or should not be included,
    remove the corresponding property entirely.  For example, if no
    'about' link exists, delete the entire `:about` line from the
    entry.

While we recommend that you follow these guidelines, do not worry too
much about getting everything right on the first attempt.  Automated
checks run on every pull request and if you miss something, the
failing checks will tell you what needs fixing.

Aside: The 4th (no Oxford comma) and 5th points above (double spacing
convention) are likely going to feel awkward to many people.  This
project uses British English (BrE) and Oxford comma is generally not
used in BrE.  Further, the original author of this project uses Emacs
and has configured it to follow the rather old convention of
separating sentences with double spaces.  As a result, we require
sentences to be separated by double spaces here as well.  Yes, you
lose some precious bytes for your 80 character bio.  Consider it the
price you pay for including your website in this directory.


### Add via Issue

If creating a pull request is too much hassle for you, [create an
issue][ISSUES] instead and post the website details in the following
format:

````lisp
```
 (:name "Your first name and last name"
  :site "https://example.com/"
  :blog "https://example.com/blog/ (Only if you have a separate blog URL)"
  :feed "https://example.com/feed.xml"
  :about "https://example.com/about/"
  :now "https://example.com/now/"
  :hnuid "example (Your HN user ID)"
  :bio "A brief third-person description of yourself, not exceeding 80 characters.")
```
````

While creating the issue, do follow all the guidelines mentioned in
the previous section.


FAQ
---

 1. How is this different from the OPML available at
    [outcoldman/hackernews-personal-blogs][outcoldman]?

    The OPML file there is more comprehensive.  It currently includes
    over 1200 RSS feeds, whereas our project only has a small number
    of entries so far.  We hope that, with contributions from
    community members, our list will grow over time.  An important
    goal of this project is to maintain the list of websites together
    as a community.

    This project also goes beyond providing an OPML file.  In addition
    to the [OPML][opml], it offers a [website][site] that lists the
    websites individually along with a short bio of each author.

 2. Why did you not start by importing
    [outcoldman/hackernews-personal-blogs][outcoldman]?

    We have not yet had the time to do so.  The [data format][lisp]
    used by this project differs from theirs and requires additional
    information for each blog entry.  Translating the existing list
    into our format therefore takes a non-trivial amount of effort,
    which we have not been able to commit to yet.

    Contributions that add entries to this project, whether sourced
    from that list or elsewhere, are very welcome.

[outcoldman]: https://github.com/outcoldman/hackernews-personal-blogs


Licence
-------

This is free and open source software.  You can use, copy, modify,
merge, publish, distribute, sublicence and/or sell copies of it, under
the terms of the MIT Licence.  See [LICENSE.md][L] for details.

This software is provided "AS IS", WITHOUT WARRANTY OF ANY KIND,
express or implied.  See [LICENSE.md][L] for details.

[L]: LICENSE.md


Contact
-------

To report bugs or ask questions, [create issues][ISSUES].

Alternatively, you can also join our IRC channel [#hnpwd][IRC] on
Libera Chat to ask questions or just generally hang out with the
community.

[ISSUES]: https://github.com/hnpwd/hnpwd.github.io/issues
[IRC]: https://web.libera.chat/#hnpwd
