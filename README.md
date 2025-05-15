# emacs.d

`git clone` this repo to your `~/.emacs.d`, `~/.config/emacs` or wherever
you put your emacs configuration, or browse about and copy/paste the parts
you like.

Thanks to
[use-package](https://github.com/jwiegley/use-package),
[system-packages](https://gitlab.com/jabranham/system-packages) and
[elpaca](https://github.com/progfolio/elpaca), it has become much easier
to manage and move the configuration across machines with minimal hassle.
If you want to scale your configuration, you can't go too wrong reading
their `README.md`s prior to copy-pasting snippets indiscriminately.

version compatibility
---------------------

This configuration was tested with **emacs 29**.

OS environment
--------------

This configuration is geared towards **Fedora Linux** and **Gnome**
running on **Wayland**. However nothing here prevents it running on other
platforms except the fact that [system-packages](https://gitlab.com/jabranham/system-packages) is set to
use the [dnf](https://dnf.readthedocs.io/en/latest/command_ref.html) package
manager. You can always fork this repo and change

```cl
system-packages-package-manager 'dnf
```

to `nil` if you want to guess

```cl
system-packages-package-manager nil
```

or any other package manager you prefer to use in your operating system.

Have fun!

Troubleshooting
---------------

I have some files that are encrypted. If you really want to run this
in your own computer, you will have to comment out the lines in
`init.el` that reference encrypted files, e.g., the ones with a `.gpg` extension.

```elisp
;; Safe
(load (concat user-emacs-directory "mixins/safe.el.gpg"))
```

Commenting in [Emacs
Lisp](https://www.gnu.org/software/emacs/manual/html_mono/elisp.html)
is done by prepending the line with `;;`. That should be enough to get
things going.

No, I won't give you my **gpg** keys. Yes, I can help you to write
your own encrypted files using **Linux** if you ask. The documentation
behind this is a bit scattered, and it took me some effort to figure
that one out.
