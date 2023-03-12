# TeX Live on the fly


## Background

Initially, *flytex* was part of [this project](https://github.com/indrjo/minimal-texlive-installer.git), because it wouldn't have come to life without it and the [criticisms](https://github.com/indrjo/minimal-texlive-installer#criticisms) listed there. *flytex* aims just to be a mature and a valid replacement of *texliveonfly*, indeed.

Historically, *flytex* was first designed in Haskell. Afterwards, I decided to write it in Python too for the simple reason of portability: nearly every GNU/Linux distro ships a Python interpreter and some basic libraries. Which Python? Nowadays, Python 2 is gradually fading away: thus, you should have Python 3.

By the way, the Haskell version is the main work. New features, changes and fixes will be applied to it first, and afterwards to the other implementations.

Special thanks goes to @relikd, who dedicated some time to rewrite the Python implementation in a more *pythonic* fashion and suggested a nice trick to get all the missing packages in one go. This idea works particularly fine with Haskell, because of its laziness.


## Usage

There is no sophistication here: after you have installed *flytex* ([click](#installation)) you can use it as follows:
```
$ flytex --c COMPILER --i TEX-FILE
```
The program will try to run
```
$ COMPILER TEX-FILE
```
and install any required package that is massing from your minimal TeX Live.


## Installation

*flytex* comes here written in different languages. If you want the *flytex* written in the language *LANG*, just look for the directory ```./LANG```: if it exists, there you will find the program in a single file and its own installer.

Namely, you can install *flytex* as follows:
```
$ cd ./LANG
$ ./make.sh
```

As the installation ends, a program ```flytex``` will appear in ```~/.local/bin``` (which will be created if absent), so make sure this path is in your ```PATH```. Thus, getting rid of *flytex* is simple:
```
$ rm ~/.local/bin/flytex
```

**(Attention)** If present, read also ```./LANG/README.md```.
