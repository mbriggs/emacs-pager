# emacs-pager.el

I use emacs `shell-mode` buffers almost exclusively, which usually
works great, except for when something wants to page. Things like
`less` dont really work that great in `shell-mode`, and in general I
would prefer a normal emacs buffer for this anyways.

Unfortunately, `emacsclient` doesnt really work with piped content, so
there are a few pieces required to make this work.

### How this works

Well-behaving unix programs will use whatever is in `$PAGER` when
showing data that is too big to fit in a terminal screen. So in the
shell, we will set `$PAGER` to a script, but only if emacs is
currently running. That script will read the piped data, and write it
to a file in `/tmp`, read it into emacs via `emacsclient`, and wait
until emacs is done with it. In emacs, it will use a small mode for
ANSI coloring and keybinds.

### Installation

Make sure you are running an emacs server, so `emacsclient` will
work. This is what I currently use in my init.el


    (require 'server)
    (unless (server-running-p)
      (server-start))

Make sure that `emacsclient` is available on your path.

Put `emacs-pager` (from this repository) somewhere in your path, and
make sure it is executable. This will be the thing that accepts the
piped data, and sends it to emacs.

In your `.bashrc` or `.zshrc` file, put something like this


    if [ $INSIDE_EMACS ]; then
        export PAGER="emacs-pipe"
    elif [ -x "`which less`" ]; then
        export PAGER="`which less`"
        export LESS="-isR"
        alias lv="less"
    else
        export PAGER="/bin/more"
    fi

Put `emacs-pager.el` somewhere on your load-path, and require it. Or,
better then that, use something like el-get or quelpa to package it up
in a sane manner and make it available that way.

Somewhere in your emacs init files, add the following line
