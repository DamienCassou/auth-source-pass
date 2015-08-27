# auth-password-store

## Summary

Integrate Emacs' auth-source with password-store. The
[auth-source](https://www.gnu.org/software/emacs/manual/html_mono/auth.html)
library is a way for Emacs to answer the old burning question “What
are my user name and password?”.
[Password-store](http://www.passwordstore.org) (or just `pass`) is a
standard unix password manager following the Unix philosophy.

The auth-password-store project is a password-store backend for
auth-source.

## Installing

Use [melpa](http://melpa.milkbox.net).

## Setup

Add the folowwing to your =init.el= file:

    (require 'auth-password-store)
    (auth-pass-enable)

## Organization

Auth-password-store follows the first approach suggested by the
Password-store project itself for
[data organization](http://www.passwordstore.org/#organization) to
find data. This means that the filename of the file containing the
password for a user on a particular host must contain the hostname.
The file itself must contain the password on the first line, as well
as a `user` field containing the username on a subsequent line. A
`port` field can be used to differentiate the authentication data for
several services with the same username on the same host.

## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2015 Damien Cassou & Nicolas Petton.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
