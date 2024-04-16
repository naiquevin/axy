axy
===

`axy` is short for **A**d hoc e**X**pansion of **Y**asnippet!

It's a simple utility that allows expansion of yasnippet snippets from
anywhere in emacs. For example, to expand a bash snippet, you don't
need the `sh-mode` to be enabled as the major mode in the current
buffer.

The snippet expands in a temporary buffer, from where it can be copied
to the clipboard with a single keybinding (that will also take care of
cleaning up the temporary buffer).


Motivation
----------

The gold standard for snippet expansion for me is
[Alfred](https://www.alfredapp.com/) workflows +
[Dash](https://kapeli.com/dash) snippets, which I have been using for
quite some time. But it's specific to MacOS and requires paid versions
of both the apps. The code in this package started out as an
experiment to find out if something similar could be achieved with
yasnippet inside emacs. Turns out that it's possible!

I believe this approach has some advantages too:

1. The template syntax of yasnippet is much more powerful than that of
   dash snippets

2. It's easier to maintain a single repository of snippets, checked
   into version control


Installation & Configuration
----------------------------

The package is not yet uploaded on any of the emacs package
repositories. For now, you can clone the repo in an accessible
location and somehow load this file. Then just set a global keybinding
to invoke the `axy/find-&-expand-snippet` fn.

If you use `use-package`, you may add the following lines to your
emacs config:

``` emacs-lisp

    (use-package axy
      :ensure nil
      :requires (yasnippet dash)
      :after (yasnippet)
      :load-path /location/to/axl/repo
      :config
      (global-set-key (kbd "C-c ;") 'axy/find-&-expand-snippet))
```


Usage
-----

Assuming that the global keybinding mentioned above is set, you can
type `C-c ;` from anywhere to invoke the `axy/find-&-expand-snippet`
fn.

It will first prompt for a major mode. Upon selecting the major mode,
it will prompt you to select the snippet from a list of snippets
eligible for that mode. On selecting a snippet, a new temporary buffer
will open, initiating the expansion of the selected snippet. Once all
the placeholders are entered, you can use the `axy-mode` specific
keybinding `C-;` to copy the expanded snippet to clipboard and also
cleanup the temporary buffer.

The code also provides a minor mode named `axy-mode` which is only
meant for the temporary buffer mentioned above. There's no need to
manually enable it in any other buffers.


Demo
----

![axy](./demo.gif?raw=true)


Known issues
------------

The code depends on some of the internal functions from the yasnippet
package. So if the API of these fns changes in future releases, it
could break `axy`.

The oldest version of yasnippet that axy is tested with is
`20200604.246`.


License
-------

[MIT](LICENSE)

