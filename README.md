[![Build Status](https://travis-ci.org/DamienCassou/auth-password-store.svg)](https://travis-ci.org/DamienCassou/auth-password-store)

# auth-source-pass (aka. auth-password-store)

The auth-source-pass package, formerly known as auth-password-store,
integrates Emacs' auth-source library with password-store. The
[auth-source](https://www.gnu.org/software/emacs/manual/html_mono/auth.html)
library is a way for Emacs to answer the old burning question “What
are my user name and password?”.
[Password-store](http://www.passwordstore.org) (or just `pass`) is a
standard unix password manager following the Unix philosophy.

The auth-source-pass project is a password-store backend for
auth-source.

## Installing

This package has been included in Emacs 26. If that suits you, you can
just start using it. Otherwise, install it from melpa.

Regardless of the version you choose, please report issues to
[auth-password-store's issue
tracker](https://github.com/DamienCassou/auth-password-store/issues).
Please make sure to specify which version you use.


## Usage

Add the following to your `init.el` file:

    (require 'auth-source-pass)
    (auth-source-pass-enable)

## Organization

auth-source-pass follows the first approach suggested by the
Password-store project itself for
[data organization](http://www.passwordstore.org/#organization) to
find data. This means that the filename of the file containing the
password for a user on a particular host must contain the hostname.
The file itself must contain the password on the first line, as well
as a `user` field containing the username on a subsequent line.

If you have several accounts for the same host, you can name your
files in 2 different ways:

- `user1@host.gpg` and `user2@host.gpg`, or
- `host/user1.gpg` and `host/user2.gpg`

If you use several ports (sometimes called *services*) in the same
host (e.g., web and mail), you can add a colon and the port number (or
service name) at the end of the filename: e.g., `host:443.gpg` or
`host:imap.gpg`.

## Pass in Emacs

Users of this package may also be interested in functionality provided
by other Emacs packages dealing with pass:

- [password-store](https://git.zx2c4.com/password-store/tree/contrib/emacs/password-store.el): password store (pass) support ;
- [pass](https://github.com/NicolasPetton/pass): a major mode for
  pass ;
- [helm-pass](https://github.com/jabranham/helm-pass): helm interface for pass.

## Contributing

Yes, please do! See [CONTRIBUTING][] for guidelines.

## License

See [COPYING][]. Copyright (c) 2015 Damien Cassou & Nicolas Petton.


[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
