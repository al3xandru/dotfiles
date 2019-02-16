# Emacs setup

My first Emacs setup was based on Spacemacs.  It worked very well as an
introductory tool with a set of defaults and packages that allowed me to
learn things at my own pace.

Over time, I've started to notice things that made me ask myself if
running Spacemacs long term is the right thing or if I'd benefit from
putting together my own settings.  Given I only need a handful of
dependencies, the sometimes lack of consistency in Spacemacs, I decided to
attempt creating my own `init.el`.

* * * * *

## Bits & Pieces

### Use `use-package` to manage dependencies 

[use-packages](https://github.com/jwiegley/use-package)

Recommended setup based on 
<https://www.reddit.com/r/emacs/comments/47aq53/best_way_to_set_up_package_dependencies_in_initel/>:

```el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (progn
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(require 'use-package)
(setq use-package-always-ensure t)
```

#### Upgrading packages using `use-package`

<https://www.reddit.com/r/emacs/comments/3m5tqx/can_usepackage_upgrade_installed_packages/>

### Disable menus and toolbars

```el
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
```

* * * * *

## Plugins

### ace-window

After triggering `ace-window`, I can perform a series of actions against windows by first choosing the action below and then the window: 
* `x`: delete
* `m`: swap
* `M`: move
* `j`: select buffer
* `n`: select the previous window
* `u`: select buffer in the other window
* `c`: split window fairly either vertically or horizontally
* `v`: split window vertically
* `b`: split window horizontally
* `o`: maximize current window
* `?`: display these commands

### avy

* in `isearch`, jump to a result using avy using binding `C-'`
* avy has commands for Org

* * * * *

### Used references

* Aaron Bedra: <http://aaronbedra.com/emacs.d/> (very well documented)
* <https://github.com/auwsmit/emacsconfig/blob/master/config.org>
* John Wiegley: <https://github.com/jwiegley/dot-emacs/blob/master/init.el> (353 stars, 61 forks)
* Sacha Chua: <https://github.com/sachac/.emacs.d> (213 stars, 43 forks)
* [technomancy/better-defaults: A small number of better defaults for Emacs](https://github.com/technomancy/better-defaults)

### References

* <https://github.com/purcell/emacs.d> (4288 stars, 2063 forks): this is too
complicated
* <https://github.com/magnars/.emacs.d> (1206 stars, 219 forks)
* <https://github.com/asimpson/dotfiles/blob/master/emacs/emacs-lite.org>: a lite version with *just* the essential modern packages
* <https://github.com/ejmr/DotEmacs/blob/master/init.el>
* A very base and simple emacs configuration <https://github.com/anonymou3/emacs.d> (*nb* doesn't have a significant number of stars or forks)
* [A Gentle Introduction to Emacs Configuration](https://blog.aaronbieber.com/2015/07/05/a-gentle-introduction-to-emacs-configuration.html)
* [What are some great .emacs.d examples? : emacs](https://www.reddit.com/r/emacs/comments/2edbau/what_are_some_great_emacsd_examples/)
* [emacs-tw/awesome-emacs: A community driven list of useful Emacs packages, libraries and others.](https://github.com/emacs-tw/awesome-emacs)
* [hlissner/doom-emacs: An Emacs configuration for the stubborn martian vimmer](https://github.com/hlissner/doom-emacs)
* [I wanna see your init file(s)! Share 'em! : emacs](https://www.reddit.com/r/emacs/comments/4kv163/i_wanna_see_your_init_files_share_em/)

### Read

* [Has Richard Stallman ever shared his .emacs file? : emacs](https://www.reddit.com/r/emacs/comments/8j98jz/has_richard_stallman_ever_shared_his_emacs_file/)


