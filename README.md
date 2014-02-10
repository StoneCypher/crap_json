crap_json
=========

A crap JSON dumper for Erlang.  Takes some liberties.  Reasonably tested.



K what's this nonsense
----------------------

I needed a json dumper for a thing a friend and I were doing with [htstub](https://github.com/StoneCypher/htstub/).  It didn't have to be fancy.  It was export to JS only; no need to parse.  It did have to work reliably, to express JS primitives, to express nesting, and to work with international text.  I did not need the expression of Erlang exotic types like references and PIDs.

Thus (choral music) another piece o' crap was born.



The Good
--------

* Reasonably well tested, including stochastic tests from PropEr
* Tolerably documented
* Adequate



The Bad
-------

* Dump only; no parse
* Probably could be a lot faster
* Not entirely finished as of this writing



The Ugly
--------

* Probably can't be made round-trip parse stable because of collisions like how binaries are handled
* Very new; may still have lurking bugs or dumb mistakes



Polemic :neckbeard:
===================

`crap_json` is MIT licensed, because viral licenses and newspeak language modification are evil.  Free is ***only*** free when it's free for everyone.
