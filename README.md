# emacs
`~/emacs.d`

That's my emacs configuration, reborn. Thanks to
[use-package](https://github.com/jwiegley/use-package),
[system-packages](https://gitlab.com/jabranham/system-packages) and
[elpaca](https://github.com/progfolio/elpaca). If you want to scale your configuration, you
can't go too wrong reading their `README.md`s prior to copy-pasting snippets
indiscriminately.

version compatibility
---------------------

This configuration works with **emacs 29**, but one can also use it with **emacs 28** by
commenting out the `:ensure-system-package` keywords and the `symbolic expressions` that
follow. You will then need to ensure system packages like `ripgrep` and `wl-clipboard`
are installed in your system.

OS environment
--------------

This configuration is geared towards **Fedora Linux** and **Gnome** running on **Wayland**.
