GitBunker
=========

GitBunker is a set of tools to allow you to manage access to you host's
repositories. The main purpose is to manage a database (sqlite3) with ssh-keys
and a DB for user rights on specific repository (per group of user, or
individualy).

There are 4 major products
* the library which provide you a common interface to use the product
* a CLI interface to add/create/list data bases
* the AuthorizedKeysCommand for openssh (see below)
* the gitbunker-shell for git remote requests (see below)

Library
=======

Database
--------

This is the common interfaces to manage the authorized keys to access an
account on the host.

GitCommand
----------

TODO

BINARY
======

bunkerdb
--------

The CLI interface to create database, add/get/del user.

    nicolas@gitbunker.com$ bunkerdb create nicolas
    nicolas@gitbunker.com$ bunkerdb add nicolas <nicolas' key> "nicolas"
    nicolas@gitbunker.com$ bunkerdb add nicolas <vincent's key> "vincent" "check_hituserright vincent"


So, I created a database of authorized keys for the nicolas user with two
entries:
* One for Nicolas with his given key. So he can connect through ssh to
the nicolas@gitbunker.com.
* One for Vincent with his given key and a command. This command will be used
  by the ssh daemon

check\_authorizedkeys
---------------------

This is the OpenSSH interface. It's only to read the database and returns the
authorized keys for a given host's user.

It returns the content of the appropriate database. This is the same output as
the default authorized keys flat file (.ssh/authorized\_keys).

    nicolas@gitbunker.com: check_authorizedkeys nicolas
    <nicolas' key>
    <vincent's key> command="check_hituserright vincent"

How to Install:
* create a user without login: *useradd -s /usr/sbin/nologin bunkerkeys*
* create the directory in */var/lib/bunkerDB* with authorization to read/write
  for bunkerkeys
* add the given options in */etc/ssh/sshd\_config*:
  * AuthorizedKeysCommand /usr/local/bin/check\_authorizedkeys
  * AuthorizedKeysCommandUser bunkerkeys
* reload (or restart) the ssh deamon

check\_hituserright
-------------------

This is a default command to check the user's right to a repository.
In our examples, it will be called by the ssh daemon if needed (command	value).
If this binary knows nothing about the user ssh authorization. It only knows
the user's name (given in the command line). It will only check if a user is
allowed to access a repository and how he can access it (read only,
read/write...).
