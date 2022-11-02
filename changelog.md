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
