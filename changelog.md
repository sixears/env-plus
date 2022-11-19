1.0.7.32 2022-11-19
===================
- base0t->0.0.1.13

1.0.7.31 2022-11-19
===================
- natural->0.0.1.13

1.0.7.30 2022-11-19
===================
- has-callstack->1.0.1.16

1.0.7.29 2022-11-19
===================
- tasty-plus->1.5.2.19;exited->1.0.4.19;base0t->0.0.1.12;monaderror-io->1.2.5.16;tfmt->0.2.7.20;number->1.1.2.13;has-callstack->1.0.1.15;base0->0.0.4.10;base1->0.0.9.27;natural->0.0.1.12;more-unicode->0.0.17.11;fpath->1.3.2.28

1.0.7.28 2022-11-19
===================
- flake-build-utils->1.0.0.13

1.0.7.27 2022-11-19
===================
- base1t->0.0.5.27

1.0.7.26 2022-11-18
===================
- index->1.0.1.18

1.0.7.25 2022-11-18
===================
- index->1.0.1.17;base0t->0.0.1.10;number->1.1.2.11;base0->0.0.4.8;more-unicode->0.0.17.9;flake-build-utils->1.0.0.12

1.0.7.24 2022-11-18
===================
- tfmt->0.2.7.17

1.0.7.23 2022-11-18
===================
- has-callstack->1.0.1.13

1.0.7.22 2022-11-18
===================
- base1->0.0.9.21

1.0.7.21 2022-11-18
===================
- flake-build-utils->1.0.0.12

1.0.7.20 2022-11-18
===================
- tasty-plus->1.5.2.16

1.0.7.19 2022-11-18
===================
- exited->1.0.4.16

1.0.7.18 2022-11-18
===================
- monaderror-io->1.2.5.13

1.0.7.17 2022-11-17
===================
- tfmt->0.2.7.15

1.0.7.16 2022-11-17
===================
- has-callstack->1.0.1.12

1.0.7.15 2022-11-17
===================
- number->r1.1.2.10

1.0.7.14 2022-11-17
===================
- base0t->r0.0.1.9

1.0.7.13 2022-11-17
===================
- base0->r0.0.4.7

1.0.7.12 2022-11-17
===================
- upgrade to callPackage-based versions

1.0.7.11 2022-11-13
===================
- fix fixed-package-name typo in flake-build-utils

1.0.7.10 2022-11-04
===================
- fix package names

1.0.7.9 2022-11-03
==================
- remove redundant "output" flake-utils

1.0.7.8 2022-11-03
==================
- flake-build-utils->1.0.0.6

1.0.7.7 2022-11-03
==================
- base0t->0.0.1.3

1.0.7.6 2022-11-03
==================
- base0->0.0.4.2; base0t->0.0.1.2

1.0.7.5 2022-11-03
==================
- base0->0.0.4.1; base0t->0.0.1.1

1.0.7.4 2022-11-02
==================
- natural->0.0.1.2

1.0.7.3 2022-11-02
==================
- more-unicode -> 0.0.17.2

1.0.7.2 2022-11-02
==================
- upgrade flake-build-utils to 1.0.0.3

1.0.7.1 2022-11-01
==================
- add flake
- use ghc-8.10.7 for tfmt

1.0.7.0 2022-04-14
==================
- replace internal shell_quote with tfmt %q
- use EnvKeySet for envmods
- add tests for envmods

1.0.6.1 2022-04-06
==================
- update base1t to 0.0.4.0; remove explicit dependency on tfmt

1.0.6.0 2022-04-04
==================
- add MkEnvModFrag, mkEnvModFrag, preclearEnvMod

1.0.5.0 2022-03-30
==================
- replace withEnv with withEnvMod

1.0.4.0 2022-03-29
==================
- Add text descriptions to EnvMod
- clearEnvMod now takes a set of keys to keep.

1.0.3.0 2021-11-23
==================
- rename many *Env to *EnvMod in Types to avoid clashes with fns from Env.hs

1.0.2.0 2021-07-16
==================
- Add NFData instances of Env{,Key,Val}

1.0.1.0 2021-06-01
==================
- Add Printable instance of Env

1.0.0.0 2019-10-03
==================
- factored out from fluffy, and fpath-temp
