# My Emacs setup

Creating my own Emacs setup.

* * * * *

### Bits & Pieces

* Use `use-package` to manage dependencies <https://github.com/jwiegley/use-package>
*

From
<https://www.reddit.com/r/emacs/comments/47aq53/best_way_to_set_up_package_dependencies_in_initel/>

```el
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))

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

(use-package evil
    :config
    (evil-mode 1)
    (modify-syntax-entry ?_ "w")
    )

(use-package monokai-theme)
```

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


