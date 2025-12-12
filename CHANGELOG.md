# Next Version

No changes yet.

# 1.4

Support the `with` and `module` keywords.

Support the null-safe pipe operator `|?>`

# 1.3

Fixed infinite loops when XHP is nested in a comment

Removed highlighting for obsolete keywords `array`, `record`,
`__COMPILER_HALT_OFFSET`.

Added highlighting for keywords `nameof`, `internal`, and `readonly`.

Make keyword highlighting case-sensitive.

Don't treat `#` as a comment anymore, as that syntax has been taken
by enum classes.

# 1.2

Improved indentation for `|>`, generics, and vec literals.

Fixed an issue where indentation would use tabs.

Fixed an issue where switching to `hack-mode` would mark the buffer as
modified.

Removed highlighting for obsolete keywords `and`, `or`, `xor` and
`__halt_compiler`. Ensure that `as` and `is` are consistently
keywords, but `instanceof` is not.

Remove support for highlighting `UNSAFE` and `UNSAFE_EXPR` which were
removed from Hack in 2019.

Fixed a error when writing XHP `<` before a tag name has been typed.

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
