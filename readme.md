# formfeeder - Display ^L glyphs as horizontal lines

_Author:_ Jade Michael Thornton<br>
_Version:_ v1.1.0<br>

This minor mode displays form feed characters (which often appear as ^L) as a single
horizontal line, spanning the window.

There are a bunch of ways of attacking this problem, one of the more obscure ones is
manipulating the display table of every window displaying the buffer. Unfortunately
this approach is limited to replacing a glyph with an array of other glyphs, but
guaranteed to work on non-graphical displays. The other approach is putting an
overlay or text property over the glyph which manipulates its look. Since a face on
its own won't do the trick, this package uses font-lock to add text properties to the
page delimiter glyph. This also means that while this package is conceptually very
simple and non-invasive, it might not work on non-graphical displays. As a workaround
we also use underlining when GUI isn't available.

## Usage

This package is not available from MELPA at this time. Manually install, or use
use-package with straight.el:

       (use-package formfeeder
         :straight (:host gitlab :repo "thornjad/formfeeder" :branch "main")
         :config (global-formfeeder-mode))

Alternatively to the global minor mode, call <kbd>M-x formfeeder-mode</kbd> manually or use a
hook:

       (add-hook 'help-mode #'formfeeder-mode)

## Configuration

Use manual or hook-based activation to enable only in the exact buffers you want, or
customize the way `global-formfeeder-mode` works using `formfeeder-enable-modes`. Set
this variable to your desired modes in a list. Here's the default value as an
example:

       (setq formfeeder-enable-modes '(prog-mode text-mode help-mode eww-mode))

## License

This package is based on earlier work by Vasilij Schneidermann's form-feed package.

Copyright (c) 2019-2021 Jade Michael Thornton<br>
Copyright (c) 2014-2016 Vasilij Schneidermann

This program is free software; you may redistribute it and/or modify it under the
terms of the GNU General Public License version 3, as published by the Free Software
Foundation. This program carries no warranty whatsoever, without even the implied
warranty of merchantability or fitness for a particular purpose. See
<https://www.gnu.org/licenses/> for more details.


---
Converted from `formfeeder.el` by [_el2md_](https://gitlab.com/thornjad/el2md).
