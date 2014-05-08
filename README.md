# emacs-pager.el

I use emacs `shell-mode` buffers almost exclusively, which usually
works great, except for when something wants to page. Things like
`less` dont really work that great in `shell-mode`, and in general I
would prefer a normal emacs buffer for this anyways.

Unfortunately, `emacsclient` doesnt really work with piped content, so
there are a few pieces required to make this work.
