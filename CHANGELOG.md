# 1.2

Improved indentation for `|>`.

# 1.1

Fixed an error when loading hack-mode from a byte-compiled file
(e.g. when installed from MELPA).

Added `hack-format`, a format command that can also act on regions.

Ensure that starting `hack-mode` cleans up any old text properties, or
buffers with incorrect highlighting stay incorrect after upgrading.

Fixed some highlighting bugs with code after comments that look like
XHP tags.

Better highlighting of obscure reserved keywords (`endforeach`) and built-in
symbols like `__FILE__`.

# 1.0

Initial release:

* Extensive syntax highlighting
* Indentation
* `hack-format` support
