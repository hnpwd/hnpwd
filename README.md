HN Personal Websites Directory
==============================

HN Personal Websites Directory (HNPWD) is a community-maintained
directory of personal websites by members of the Hacker News (HN)
community, along with an OPML file containing their RSS feeds.  Please
visit the following link to view these resources:

* Directory Website: [hnpwd.github.io][site]
* RSS Feeds OPML: [hnpwd.opml][opml]

[site]: https://hnpwd.github.io/
[opml]: https://hnpwd.github.io/hnpwd.opml
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

Please check whether a website meets **all** of the following criteria
before requesting its inclusion in the directory:

 1. **Personal Website**

    The website must be a personal website.  It should not be a
    company website or a community website.  The website should belong
    to an individual.

 2. **English Language Website**

    HN is an English-language forum, so this directory includes only
    websites with English-language content.  Websites may also contain
    content in other languages, provided they have a significant
    amount of English content and visitors can easily find and
    navigate to it.  Websites that are entirely or primarily
    non-English are not suitable for inclusion in this directory.

 3. **Author-Controlled Website**

    The website must be a personal website where the author has full
    control over its design, content and presentation.  This includes
    control over layout, styling and monetisation.  Websites hosted on
    platforms that inject advertisements, branding or other third
    party content do not meet this criterion.  Similarly, websites on
    platforms that limit the author's control over design or
    presentation in any way do not qualify.

    Websites hosted on personal domains clearly qualify.  Websites
    hosted on services such as Neocities, GitHub Pages or Codeberg
    Pages also qualify, as these platforms allow full control over the
    site's design.  However, blogs hosted on publishing platforms such
    as Medium, Substack, Blogspot, the free WordPress.com service,
    etc. do not qualify.

    Note: This requirement is about control, not quality.  Many
    excellent blogs fall outside the scope of this directory.

 4. **Substantive Personal Content**

    The website must contain substantive content such as articles,
    blog posts, projects or games developed by the author.  This
    content must be hosted on the website itself.  Websites that
    merely link to content hosted elsewhere are not accepted.
    Websites consisting only of a CV or portfolio are also not
    suitable for inclusion.  This requirement helps keep the directory
    interesting and prevents it from becoming a collection of minimal
    or placeholder websites with little value to the community.

    Websites with only a small number of pages or articles are usually
    not included immediately.  Instead, we wait until more content is
    added before considering them for inclusion in the directory.  As
    a general guideline, we expect 5 or more substantive articles or
    other original works (excluding usual meta pages such as about,
    contact, etc.).  Exceptions may be made in some cases, for example
    when a site has only a few posts but one or more of them have been
    very well received on HN.  This requirement helps ensure that
    visitors arriving from the directory will find enough interesting
    content to explore.

 4. **Human Authorship**

    Most of the content on the website must be clearly authored by the
    individual claiming ownership of the website.  Websites containing
    plagiarised material are immediately disqualified.  Websites
    consisting primarily of automatically generated content, including
    syndicated technology feeds, AI-generated blogspam or similar
    material, are not eligible for inclusion in this directory.
    Content that has been reviewed or edited by AI tools is acceptable
    but content that is purely AI-generated is not.  This requirement
    exists to ensure a genuine human presence and authorship behind
    every website included in the directory.  This is consistent with
    the spirit of HN, which does not permit automatically generated
    comments and is intended as a forum for humans.


### Add via PR

Add the website details to [pwd.lisp][lisp] and submit a pull request.

A template entry is provided at the bottom of [pwd.lisp][lisp] and can
be copied and modified as a starting point.  When creating a pull
request, please follow these guidelines:

 1. Keep all website entries sorted alphabetically by name.

 2. Write the bio text in the third person.  That is, do not write:

    ```
    I write about functional programming.
    ```

    Instead, write about:

    ```
    Writes about functional programming.
    ```

    The intention is to reflect that the curators are adding your
    website to this project.  Although you are the one authoring the
    pull request, it is intended to help the curators include your
    website more quickly.  Therefore, the language should match what
    the curators would have written if they were adding the website
    themselves.

 3. Ensure that the bio text does not exceed 80 characters.

 4. End the bio text with a full stop (period).

 5. Do not use ampersand ('&') in the bio text.  Write the word 'and'
    instead.

 6. Do not use Oxford comma (serial comma) in the bio text.  That is,
    do not write:

    ```
    Writes about C, C++, and Go.
    ```

    Instead, write:

    ```
    Writes about C, C++ and Go.
    ```

 7. Separate two sentences by double spaces.  That is, do not write:

    ```
    Game developer. Writes about computer graphics.
    ```

    Instead, write:

    ```
    Game developer.  Writes about computer graphics.
    ```

 8. Begin URLs with `http://` or `https://`.

 9. If a URL points to the root directory, include the trailing slash.
    For example, write `https://example.com/` (not
    `https://example.com`).  This avoids an extra redirect for many
    clients.

10. If certain information is not available or should not be included,
    remove the corresponding property entirely.  For example, if no
    'about' link exists, delete the entire `:about` line from the
    entry.

While we recommend that you follow these guidelines, do not worry too
much about getting everything right on the first attempt.  Automated
checks run on every pull request and if you miss something, the
failing checks will tell you what needs fixing.

Aside: The 6th (no Oxford comma) and 7th points above (double spacing
convention) are likely going to feel awkward to many people.  This
project uses British English (BrE) and Oxford comma is generally not
used in BrE.  Further, the original author of this project uses Emacs
and has configured it to follow the rather old convention of
separating sentences with double spaces.  As a result, we require
sentences to be separated by double spaces here as well.  Yes, you
lose some precious bytes for your 80 character bio.  Consider it the
price you pay for including your website in this directory.


### Add via Issue

If creating a pull request is too much hassle for you, submit your
website by creating an issue.  You can do this by following this link:
[new-site][].

While creating the issue, do follow all the guidelines mentioned in
the previous section.

[new-site]: https://github.com/hnpwd/hnpwd/issues/new?template=new_site.md


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

 3. I posted my website to your [megathread on HN][hnt].  Why is it
    not added to the directory yet?

    We received over 2000 website suggestions on that thread.  Each
    submission requires a brief visual review to ensure it is a good
    fit for this directory, and this process cannot be fully
    automated.  As a result, it is likely to take several months to
    work through all the suggestions we received.

    To track our progress, see our [backlog tables][backlog-tables].

 4. Can I contribute my time and help you make progress faster?

    Yes, certainly.  Currently, we are only two people working through
    the backlog.  If you would like to help, please join us in our IRC
    channel [#hnpwd][IRC].  You may need to stay connected for a while
    after asking, as not all volunteers are present at all hours of
    the day.

 5. On the directory website, how is the order of the personal
    websites decided?

    If JavaScript is disabled in your web browser, the websites are
    displayed in alphabetical order by the author's name.

    If JavaScript is enabled, the websites are displayed in a
    pseudorandom order.  This order changes every hour.

[outcoldman]: https://github.com/outcoldman/hackernews-personal-blogs
[hnt]: https://news.ycombinator.com/item?id=46618714
[backlog-tables]: https://github.com/hnpwd/backlog#backlog-tables


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

[ISSUES]: https://github.com/hnpwd/hnpwd/issues
[IRC]: https://web.libera.chat/#hnpwd
