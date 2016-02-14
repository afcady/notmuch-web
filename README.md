# Notmuch Web Client

## Quickstart

~~~ {.bash}
$ git clone https://github.com/afcady/notmuch-web.git
$ cd notmuch-web
$ stack build
$ stack exec notmuch-web Production
~~~

You can now visit [http://localhost:3000](http://localhost:3000) and use your
system username and password.

## About

This project is an email client for [notmuch](http://notmuchmail.org) written in
[Haskell](http://www.haskell.org) and [Yesod](http://www.yesodweb.com).  The code implements a web
server and uses [bootstrap](http://twbs.github.io/bootstrap/) and [jquery](http://jquery.com/)
for the UI.  The current features are

* Search messages - [search
  screenshot](https://bitbucket.org/wuzzeb/notmuch-web/src/tip/screenshots/search.png) and [pager
  screenshot](https://bitbucket.org/wuzzeb/notmuch-web/src/tip/screenshots/pager.png)
    * link default searches from the navigation bar
    * view results in a table, with customizable buttons for retagging each thread
    * view results in a pager, which shows the thread content together with a navigation bar with
      "Next" button plus tagging buttons.
    * Opensearch.xml integration, so you can add notmuch-web as a search engine to your web browser.
* View individual threads
    * a visible tree structure and the ability to collapse individual messages
    * download attachments from messages
    * customizable tagging buttons on each message
    * On small screens (under 700 pixels width like phones), parse text messages as markdown and
      display the original text message together with the resulting markdown HTML.  This allows the
      message to be flowed to fit the small screen, but still allows one to view the original
      message.  On larger screens, the original message is displayed by default but the markdown
      HTML is available in a tab.
* Compose email
    * sending with attachments
    * supports reply and reply all
    * for composing messages, I recommend [Its All
      Text](https://addons.mozilla.org/en-us/firefox/addon/its-all-text/) to use your favorite
      editor.
    * An optional address book.  Addresses can either be loaded from
      [abook](http://abook.sourceforge.net/) on the server (abook must be in the path) or from
      Google Contacts (loading from Google contacts requires a setting change in settings.yml).
* Execute a raw notmuch command and view the results

[ChangeLog](https://bitbucket.org/wuzzeb/notmuch-web/src/tip/ChangeLog)

## Install

#### Binaries 

One nice feature of GHC (the Haskell compiler) is the ability to statically link all the Haskell
libraries to not require Haskell to be installed (C libraries are still dynamically linked).  I have
therefore built the latest release; you can find the tarballs on the [download
page](https://bitbucket.org/wuzzeb/notmuch-web/downloads).  The only prerequisites are glibc,
libgmp, and zlib.  The binaries are missing one feature: use of
[libicu](http://site.icu-project.org/) to decode text/html message parts that have a charset that is
not ISO-8859 or UTF-8 (UTF-8 and ISO-8859 are decoded internally without the help of libicu).  The
reason for this restriction is that libicu is not generally binary compatible between different
versions, so I could not link against a version of libicu that worked on many distributions.  This
restriction will be removed once the next version of notmuch is released (it will include some
patches I submitted).

#### Keter

Although I haven't used it, you might investigate [keter](https://github.com/snoyberg/keter) for
automatic building and deployment. See [this blog
post](http://www.yesodweb.com/blog/2012/05/keter-app-deployment) and [this
one](http://www.yesodweb.com/blog/2012/05/keter-its-alive) for more information.

## Configuration

When launching the notmuch-web binary, it expects several files to be located in one subdirectory
of the current directory.  No other configuration or install is needed, so you can copy the
notmuch-web binary and this folders anywhere, even deploy on computers with no Haskell
installed.  _Note: in versions of notmuch-web before 0.2.0 there was a second folder which can be
deleted when you upgrade; see the Changelog._

The required folder is named `config` and must be in the current directory.  The
config folder must contain:

* robots.txt - this file is served as robots.txt as the root and by default denies all search
  engines.  You can edit to your taste.
* favicon.ico - this is served as the favicon and defaults to the notmuch favicon.  You can replace
  with your own icon if you like.
* client_session_key.aes - notmuch-web uses AES encrypted cookies to store session information and
  the AES key is stored in this file.  If the file does not exist a new key is randomly generated and
  placed in this file.  Make sure you keep this file private.
* settings.yml - the [yaml](http://en.wikipedia.org/wiki/YAML) file containing all the
  configuration.  The binary tarballs contain a file settings.yml.example which should be copied to
  settings.yml.  See the next section for the settings.

### Settings

notmuch-web has four modes: Development, Testing, Staging, and Production.  The mode is specified on
the command line when launching notmuch-web.  The settings.yml file is split into four sections, one
for each mode and notmuch-web will only load settings from the matching section of settings.yml.
Using yaml references a collection of default settings are copied into each section of the
configuration.

The default settings.yml file is well commented so I won't explain it here.

## Running

To run, execute "notmuch-web Production".  This will start the server listening on the configured
port.  notmuch-web accesses notmuch by launching the "notmuch" binary found in the PATH, so you must
make sure the user running notmuch-web has notmuch in the PATH, is able to access the email
directory, and has a valid notmuch configuration.  You might need to set the NOTMUCH\_CONFIG
environment variable before launching notmuch-web.

I use a systemd unit to start notmuch-web as my login user at boot, having notmuch-web listen on
port 3000.  I then run [nginx](http://nginx.org/) with SSL on port 443, and have nginx reverse proxy
to notmuch-web.

#### SSL

Currently, notmuch-web does not implement SSL (although since Yesod/Warp supports SSL we could
support it without too much work so it is on the TODO list).  Therefore, you will want to
reverse-proxy notmuch-web.  I use [nginx](http://nginx.org/).

#### Sending

When you send a message, notmuch-web sends it using the executable "/usr/sbin/sendmail".  If you are
not running your own email server, I suggest [msmtp](http://msmtp.sourceforge.net/).  I use msmtp on
my laptop, it works great.  Just make sure the user you run notmuch-web as has the correct msmtp
configuration setup.
