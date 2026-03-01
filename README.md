## Acknowledgements
Thanks to the original author [manateelazycat](https://github.com/manateelazycat/)  for their development!

Since [corfu-english-helper](https://github.com/manateelazycat/corfu-english-helper) has been archived, I have forked it and added new features here.

## What is corfu-english-helper ?
corfu-english-helper is english writing assistant, help me complete English word.

This plugin base on fantastic completion frameworkw [Corfu](https://github.com/minad/corfu)

Only english.
<img src="./screenshot.png" width="400">

English and Code.
<img src="./screenshot-capf.png" width="400">


## Install
1. Download corfu-english-helper:

```Bash
git clone --depth=1 -b master https://github.com/ISouthRain/corfu-english-helper.git ~/.emacs.d/site-lisp/corfu-english-helper/
```

2. Add corfu-english-helper to ```load-path```:
```Elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/corfu-english-helper/")
(require 'corfu-english-helper)
```

## Usage
* ```corfu-english-helper-toggle```: toggle on english helper, write only english on the fly.
* ```corfu-english-helper-toggle-capf```: toggle on english helper, write english and code on the fly.
* ```corfu-english-helper-search```: popup english helper manually


## Customize your own dictionary.
Default english dictionary is generate from stardict KDict dictionary with below command

```Shell
python ./stardict.py stardict-kdic-ec-11w-2.4.2/kdic-ec-11w.ifo
```

You can replace with your favorite stardict dictionary's info filepath to generate your own corfu-english-helper-data.el .

# Acknowledgements
I create [company-english-helper](https://github.com/manateelazycat/company-english-helper), this package is port to corfu-mode, most code of corfu version is written by [theFool32](https://github.com/theFool32).
