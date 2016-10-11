Snaplet for SQLite
-------------------------

[![Build Status (master)](https://secure.travis-ci.org/nurpax/snaplet-sqlite-simple.png?branch=master)](https://travis-ci.org/nurpax/snaplet-sqlite-simple)
[![Coverage Status](http://coveralls.io/repos/nurpax/snaplet-sqlite-simple/badge.png?branch=master)](http://coveralls.io/r/nurpax/snaplet-sqlite-simple?branch=master)  

A Snap plugin (or snaplet) that makes it easy to use the SQLite database with the
[Snap web framework](http://snapframework.com/).  This plugin uses
[sqlite-simple](http://github.com/nurpax/sqlite-simple) for actually talking
to the SQLite database.

## Examples

Please see the [example project](https://github.com/nurpax/snaplet-sqlite-simple/tree/master/example)
for an introduction on how to use this within Snap.  It implements a stand-alone application with a login
screen that persists user information in a SQLite database and allows a logged in user to save comments.
Querying and listing comments demonstrates how a user of snaplet-sqlite-simple might
relate Snaplet.Auth based AuthUsers to the application's database tables.

For sqlite-simple usage examples, visit the [sqlite-simple homepage](http://github.com/nurpax/sqlite-simple).

This snaplet is based on the [snaplet-postgresql-simple](http://github.com/mightybyte/snaplet-postgresql-simple).

## Maintainers

[Janne Hellsten](https://github.com/nurpax) is the primary maintainer.  
[Sergey Bushnyak](https://github.com/sigrlami) is backup maintainer. Please get in touch with him if the primary maintainer cannot be reached.  