
# History of flytex

## Interesting...

### [20 jan 2023] Put one ```/```!
It may be more simple than I think... For example,
```
$ tlmgr search --global --file '/caption.sty' | grep -P ':\s*$' | sed 's/://'
caption
```
seems to do do what we need.

In Haskell,
```haskell
findPackages :: [String] -> [String]
findPackages = map init . filter (isSuffixOf ":")
```
replaces
```haskell
findPackages :: String -> [String] -> [String]
findPackages fp (ln1:ln2:lns) =
  case ln2 of
    '\t':_ ->
      if isSuffixOf ('/':fp) ln2
        then (init ln1) : findPackages fp (dropWhile (isPrefixOf "\t") lns)
        else findPackages fp (ln1:lns)
    _:_ -> findPackages fp (ln2:lns)
    _ -> undefined -- this should not happen
findPackages _ _ = []
```
Other small changes are applied to make all typecheck.

In Python we have just:
```python
def find_packages(lns):
  pkgs = []
  for ln in lns:
    if ln.endswith(':'):
      pkg = ln[:-1]
      pkgs.append(pkg)
  return pkgs
```

### [13 dec 2022]

Rewritten ```flytex.py``` in a more imperative fashion.

### [3 dec 2022]

```
$ tlmgr search --global --file caption.sty
tlmgr: package repository [...]
caption:
	texmf-dist/tex/latex/caption/bicaption.sty
	texmf-dist/tex/latex/caption/caption.sty
	texmf-dist/tex/latex/caption/ltcaption.sty
	texmf-dist/tex/latex/caption/subcaption.sty
ccaption:
	texmf-dist/tex/latex/ccaption/ccaption.sty
lwarp:
	texmf-dist/tex/latex/lwarp/lwarp-caption.sty
	texmf-dist/tex/latex/lwarp/lwarp-ltcaption.sty
	texmf-dist/tex/latex/lwarp/lwarp-mcaption.sty
	texmf-dist/tex/latex/lwarp/lwarp-subcaption.sty
mcaption:
	texmf-dist/tex/latex/mcaption/mcaption.sty
```
Here, the program cannot isolate ```caption```. Another example: in
```
$ tlmgr search --global --file pgf.sty
tlmgr: package repository https://ctan.mirror.garr.it/mirrors/ctan/systems/texlive/tlnet (verified)
chessboard:
	texmf-dist/tex/latex/chessboard/chessboard-keys-pgf.sty
	texmf-dist/tex/latex/chessboard/chessboard-pgf.sty
pgf:
	texmf-dist/tex/latex/pgf/basiclayer/pgf.sty
storebox:
	texmf-dist/tex/latex/storebox/storebox-pgf.sty
```
we cannot isolate ```pgf```.

In the Haskell version:
```haskell
findPackages :: String -> [String] -> [String]
findPackages fp (ln1:ln2:lns) =
  case ln2 of
    '\t':_ ->
      if isSuffixOf ('/':fp) ln2
        then (init ln1) : findPackages fp (dropWhile (isPrefixOf "\t") lns)
        else findPackages fp (ln1:lns)
    _:_ -> findPackages fp (ln2:lns)
    _ -> undefined -- this should not happen
findPackages _ _ = []
```
instead of:
```haskell
findPackages :: String -> [String] -> [String]
findPackages fp (pkg:path:other) =
  if ('/':fp) `isSuffixOf` path
    then (init pkg) : findPackages fp other
    else findPackages fp other
findPackages _ _ = []
```
