;;; highlight.el --- Highlighting commands.
;;
;; Filename: highlight.el
;; Description: Highlighting commands.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1995-2012, Drew Adams, all rights reserved.
;; Created: Wed Oct 11 15:07:46 1995
;; Version: 21.0
;; Last-Updated: Wed Feb 29 10:36:21 2012 (-0800)
;;           By: dradams
;;     Update #: 3143
;; URL: http://www.emacswiki.org/cgi-bin/wiki/highlight.el
;; Keywords: faces, help, local
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos+', `avoid', `faces', `faces+', `fit-frame',
;;   `frame-fns', `help+20', `info', `info+', `menu-bar',
;;   `menu-bar+', `misc-cmds', `misc-fns', `naked', `second-sel',
;;   `strings', `thingatpt', `thingatpt+', `unaccent',
;;   `w32browser-dlgopen', `wid-edit', `wid-edit+', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Highlighting commands.
;;
;;    More description below.
 
;;(@> "Index")
;;
;;  Index
;;  -----
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Things Defined Here")
;;  (@> "Documentation")
;;    (@> "Library `facemenu+.el' Puts Highlight on the Menu")
;;    (@> "User Option `hlt-use-overlays-flag'")
;;    (@> "Temporary or Permanent Highlighting")
;;    (@> "Commands")
;;    (@> "User Option `hlt-act-on-any-face-flag'")
;;    (@> "Hiding and Showing Text")
;;      (@> "Hiding and Showing Text - Icicles Multi-Commands")
;;    (@> "What Gets Highlighted: Region, Buffer, New Text You Type")
;;    (@> "Interference by Font Lock")
;;    (@> "Suggested Bindings")
;;    (@> "Relation to Hi-Lock Mode")
;;    (@> "Commands That Won't Work in Emacs 20")
;;    (@> "To Do")
;;  (@> "Change log")
;;  (@> "Menus")
;;  (@> "Variables and Faces")
;;  (@> "Misc Functions - Emacs 20+")
;;  (@> "Misc Functions - Emacs 21+")
;;  (@> "Functions for Highlighting Propertized Text - Emacs 21+")
;;  (@> "General functions")
 
;;(@* "Things Defined Here")
;;
;;  Things Defined Here
;;  -------------------
;;
;;  Commands defined here:
;;
;;    `hlt-choose-default-face', `hlt-copy-props', `hlt-eraser',
;;    `hlt-eraser-mouse', `hlt-hide-default-face', `hlt-highlight',
;;    `hlt-highlight-all-prop', `hlt-highlighter',
;;    `hlt-highlighter-mouse', `hlt-highlight-property-with-value',
;;    `hlt-highlight-regexp-region', `hlt-highlight-regexp-to-end',
;;    `hlt-highlight-region', `hlt-highlight-single-quotations',
;;    `hlt-mouse-copy-props', `hlt-mouse-face-each-line',
;;    `hlt-next-highlight', `hlt-paste-props',
;;    `hlt-previous-highlight', `hlt-replace-highlight-face',
;;    `hlt-show-default-face', `hlt-toggle-act-on-any-face-flag',
;;    `hlt-toggle-link-highlighting',
;;    `hlt-toggle-property-highlighting',
;;    `hlt-toggle-use-overlays-flag', `hlt-unhighlight-all-prop',
;;    `hlt-unhighlight-region', `hlt-unhighlight-region-for-face',
;;    `hlt-yank-props'.
;;
;;  User options (variables) defined here:
;;
;;    `hlt-act-on-any-face-flag', `hlt-default-copy/yank-props',
;;    `hlt-max-region-no-warning', `hlt-use-overlays-flag'.
;;
;;  Faces defined here:
;;
;;    `hlt-property-highlight', `minibuffer-prompt' (for Emacs 20).
;;
;;  Non-interactive functions defined here:
;;
;;    `hlt-add-listifying', `hlt-add-to-invisibility-spec',
;;    `hlt-delete-highlight-overlay', `hlt-highlight-faces-in-buffer',
;;    `hlt-flat-list', `hlt-highlight-faces-in-buffer',
;;    `hlt-listify-invisibility-spec',
;;    `hlt-mouse-toggle-link-highlighting',
;;    `hlt-mouse-toggle-property-highlighting',
;;    `hlt-props-to-copy/yank', `hlt-read-props-completing',
;;    `hlt-region-or-buffer-limits', `hlt-set-intersection',
;;    `hlt-set-union', `hlt-subplist', `hlt-unhighlight-for-overlay'.
;;
;;  Internal variables defined here:
;;
;;    `hlt-copied-props', `hlt-last-face', `hlt-last-regexp',
;;    `hlt-previous-use-overlays-flag-value',
;;    `hlt-prop-highlighting-state'.
 
;;(@* "Documentation")
;;
;;  Documentation
;;  -------------
;;
;;(@* "Library `facemenu+.el' Puts Highlight on the Menu")
;;  ** Library `facemenu+.el' Puts Highlight on the Menu **
;;
;;  If you load library `facemenu+.el' after you load library
;;  `highlight.el', then the commands defined here will also be
;;  available on a Highlight submenu in the Text Properties menus.
;;
;;(@* "User Option `hlt-use-overlays-flag'")
;;  ** User Option `hlt-use-overlays-flag'
;;
;;  You can highlight text in two ways using this library, depending
;;  on the value of user option `hlt-use-overlays-flag':
;;
;;   - non-nil means to highlight using overlays
;;   - nil means to highlight using text properties
;;
;;  Overlays are independent from the text itself.  They are not
;;  picked up when you copy and paste text.  By default, highlighting
;;  uses overlays.
;;
;;  Although highlighting recognizes only nil and non-nil values for
;;  `hlt-use-overlays-flag', other actions can have different
;;  behavior, depending on the non-nil value.  If it is `only' (the
;;  default value), then only overlay highlighting is affected.  If it
;;  is any other non-nil value, then both overlay highlighting and
;;  text-property highlighting are effected.  This is the case, for
;;  instance, for unhighlighting and for navigating among highlights.
;;
;;  For example, for unhighlighting, if `hlt-use-overlays-flag' is
;;  non-nil, then overlay highlighting is removed.  If it is not
;;  `only', then text-property highlighting is removed.  A value of
;;  nil thus removes both overlays and text properties.
;;
;;  Keep this sensitivity to the value of `hlt-use-overlays-flag' in
;;  mind.  For example, if you change the value after adding some
;;  highlighting, then that highlighting might not be removed by
;;  unhighlighting, unless you change the value back again.
;;
;;  You can toggle the value of `hlt-use-overlays-flag' at any time
;;  between nil and its previous non-nil value, using command
;;  `hlt-toggle-use-overlays-flag'.
;;
;;(@* "Temporary or Permanent Highlighting")
;; ** "Temporary or Permanent Highlighting" **
;;
;;  Generally, highlighting you add is temporary: it is not saved when
;;  you write your buffer todisk.  However, Emacs has a curious and
;;  unfamiliar feature called "formatted" or "enriched" text mode,
;;  which does record highlighting permanently.  See the Emacs manual,
;;  node `Requesting Formatted Text'.
;;
;;  To save highlighting permanently, do the following:
;;
;;  1. `M-x enriched-mode', to put your file buffer in minor mode
;;     `enriched-mode'.  You will see `Enriched' in the mode line.
;;
;;  2. Choose text-property highlighting, not overlay highlighting, by
;;     setting option `hlt-use-overlays-flag' to `nil'.  To do this
;;     using Customize, choose menu item `Highlight using text
;;     properties, not overlays'.
;;
;;  3. Choose the highlight face to use:
;;     `M-x hlt-choose-default-face'.
;;
;;  4. Highlight in any way provided by library `highlight.el'.  For
;;     example, use `hlt-highlighter' (personally, I bind it to `C-x
;;     mouse-2') to drag-highlight as if using a marker pen.
;;
;;  5. Save your file.
;;
;;     Note that, although highlighting in enriched-text mode modifies
;;     the buffer, it does not appear modified (check the beginning of
;;     the mode line), so if you make no other changes then using `C-x
;;     C-s' will not save your highlighting changes.  To remedy this,
;;     just do something besides highlighting - e.g., add a space and
;;     delete it - so that `C-x C-s' will save to disk.
;;
;;  When you reopen your file later, it will automatically be in
;;  enriched mode, and your highlighting will show.  However, be aware
;;  that font-locking interferes with enriched mode, so you will
;;  probably want to use it on files where you don't use font-locking.
;;
;;(@* "Commands")
;;  ** Commands **
;;
;;  You can use any face to highlight, and you can apply a mouse face
;;  instead of a face, if you like.  A mouse face shows up only when
;;  the mouse pointer is over it.
;;
;;  The commands you will use the most often are probably
;;  `hlt-highlight', `hlt-highlighter', `hlt-next-highlight', and
;;  `hlt-previous-highlight'.  You might also often use the various
;;  commands to hide and show highlighted text.
;;
;;  You can use command `hlt-highlight' to highlight the region,
;;  highlight a regexp throughout the region, or unhighlight the
;;  region, depending on the prefix argument.  It combines most of the
;;  behavior of commands `hlt-highlight-regexp-region',
;;  `hlt-highlight-region', and `hlt-unhighlight-region'.  Command
;;  `hlt-highlight-regexp-to-end' highlights a regexp from the text
;;  cursor position to the end of the buffer.
;;
;;  Command `hlt-highlighter' lets you highlight text by simply
;;  dragging the mouse, just as you would use a highlighter (marker).
;;  You can thus highlight text the same way that you drag the mouse
;;  to define the region.
;;
;;  If you use Emacs 21 or later, you can use various commands that
;;  highlight and unhighlight text that has certain text properties
;;  with given values.  You can use them to highlight all text in the
;;  region or buffer that has a given property value.  An example is
;;  highlighting all links (text with property `mouse-face').  These
;;  commands are:
;;
;;  `hlt-highlight-all-prop' - Highlight text that has a given
;;                             property with any (non-nil) value.
;;
;;  `hlt-highlight-property-with-value' - Highlight text that has a
;;                             given property with certain values.
;;
;;  `hlt-unhighlight-all-prop' - Unhighlight highlighted propertized
;;                             text.
;;
;;  `hlt-mouse-toggle-link-highlighting' - Alternately highlight and
;;                             unhighlight links on a mouse click.
;;
;;  `hlt-toggle-link-highlighting' - Alternately highlight and
;;                             unhighlight links.
;;
;;  `hlt-mouse-toggle-property-highlighting' - Alternately highlight
;;                             and unhighlight propertized text on a
;;                             mouse click.
;;
;;  `hlt-toggle-property-highlighting' - Alternately highlight and
;;                             unhighlight propertized text.
;;
;;  As always for library `highlight.el', this "highlighting" can use
;;  property `mouse-face' instead of `face'.  You could, for example,
;;  highlight, using `mouse-face', all text that has property `foo' -
;;  or that has property `face', for that matter.
;;
;;  If you use Emacs 21 or later, you can use commands
;;  `hlt-next-highlight' and `hlt-previous-highlight' to navigate
;;  among highlights of a given face.
;;
;;  You can unhighlight the region using command
;;  `hlt-unhighlight-region' (or using `C--' with `hlt-highlight').
;;  If you use overlay highlighting, then you can use command
;;  `hlt-unhighlight-region-for-face' to unhighlight the region for an
;;  individual highlighting face - other highlighting faces remain.
;;
;;  You can replace a highlighting face in the region by another,
;;  using command `hlt-replace-highlight-face'.  With a prefix
;;  argument, property `mouse-face' is used, not property `face'.
;;
;;  Command `hlt-eraser' lets you delete highlighting by dragging the
;;  mouse.  However, its behavior is different for overlays and text
;;  properties - see the `hlt-eraser' doc string.
;;
;;(@* "Copy and Yank (Paste) Text Properties")
;;  ** Copy and Yank (Paste) Text Properties **
;;
;;  You can highlight or unhighlight text by simply copying existing
;;  highlighting (or lack of any highlighting) from anywhere in Emacs
;;  and yanking (pasting) it anywhere else.
;;
;;  Put differently, you can copy and yank a set of text properties.
;;  You can use these commands to copy and yank any text properties,
;;  not just `face' or `mouse-face'.
;;
;;  To copy the text properties at a given position, use command
;;  `hlt-copy-props'.  You can then use command `hlt-yank-props' to
;;  yank those properties to the active region anywhere.  If the set
;;  of properties that you copy is empty, then yanking means
;;  effectively removing all text properties.
;;
;;  User option `hlt-default-copy/yank-props' controls which text
;;  properties to copy and yank, by default.  The default value of the
;;  option includes only `face', which means that only property `face'
;;  is copied and pasted.  That is typically what you want, for
;;  highlighting purposes.  A value of `t' for
;;  `hlt-default-copy/yank-props' means use all properties.
;;
;;  You can further control which text properties are copied or yanked
;;  when you use the commands, by using a prefix argument.  A plain or
;;  non-negative prefix arg means copy or yank all available text
;;  properties.  A negative prefix arg (e.g. `C--') means you are
;;  prompted for which text properties to use, among those available.
;;
;;  For copying, the available properties are those among
;;  `hlt-default-copy/yank-props' that are also present at the copy
;;  position.  For yanking, the available properties are those among
;;  `hlt-default-copy/yank-props' that have previously (last) been
;;  copied.
;;
;;(@* "User Option `hlt-act-on-any-face-flag'")
;;  ** User Option `hlt-act-on-any-face-flag' **
;;
;;  Library `highlight' generally acts only on faces that it controls,
;;  that is, faces that you have explicitly asked it to use for
;;  highlighting.  It sets the text property or overlay property
;;  `hlt-highlight' on such highlighted text, so that it can recognize
;;  which faces it has responsibility for.
;;
;;  Sometimes, you might want to hide and show text other than that
;;  controlled by library `highlight'.  Similarly, you might sometimes
;;  want to navigate among faces other than those used for
;;  highlighting.  You can control this using option
;;  `hlt-act-on-any-face-flag', which you can toggle at any time using
;;  command `hlt-toggle-act-on-any-face-flag'.
;;
;;(@* "Hiding and Showing Text")
;;  ** Hiding and Showing Text **
;;
;;  You can hide and show text that you have highlighted.  You will
;;  want to read the Emacs-Lisp manual (Elisp), section Invisible
;;  Text, to understand better what this entails.  In particular, you
;;  should understand that for library `highlight.el', hiding text
;;  means adding the symbol naming the face to be hidden to both:
;;
;;  1. a text or overlay `invisible' property, making the text or
;;     overlay susceptible to being hidden by buffer-local variable
;;     `buffer-invisibility-spec', and
;;
;;  2. the buffer's `buffer-invisibility-spec', so that it in fact
;;     becomes hidden.
;;
;;  After text has been hidden this way, and unless the highlighting
;;  has been removed completely by unhighlighting the text, the
;;  `invisible' property of that text keeps the names of the faces
;;  that have been applied to that text and hidden previously, even
;;  after you show that text again.  Showing a hidden face simply
;;  removes it from the `buffer-invisibility-spec'; it does not change
;;  any `invisible' properties.
;;
;;  For example, if you hide face `foo' at some buffer position:
;;
;;  1. The `invisible' property of the text or overlay at that
;;     position is updated to include `foo'.  If there are no other
;;     faces that have been applied to this text and then hidden, the
;;     `invisible' property is just `(foo)'.
;;
;;  2. `buffer-invisibility-spec' is also updated to include `foo'.
;;     This hides all text properties and overlay properties with
;;     `invisible' property `foo', throughout the buffer.  If there
;;     are no other invisible faces in the buffer, then
;;     `buffer-invisibility-spec' has value (foo).
;;
;;  If you then show face `foo' at that same buffer position, there is
;;  no change to the `invisible' property.  `buffer-invisibility-spec'
;;  is updated, by removing `foo': if it was (foo), it becomes ().
;;
;;  There are several commands for hiding and showing highlighted
;;  text.  The basic commands for hiding and showing are
;;  `hlt-hide-default-face' and `hlt-show-default-face', which you can
;;  use to hide and show the face last used for highlighting.  With a
;;  prefix argument, you are prompted for a different face to hide; it
;;  then becomes the default face for highlighting.  You can also
;;  change the default highlighting face at any time using command
;;  `hlt-choose-default-face'.
;;
;;(@* "Hiding and Showing Text - Icicles Multi-Commands")
;;  *** Hiding and Showing Text - Icicles Multi-Commands ***
;;
;;  The other hide and show commands depend on your also using
;;  Icicles, which is a set of libraries that offer enhanced
;;  completion.  Complete information about Icicles is here:
;;  `http://www.emacswiki.org/emacs/Icicles'.  You can obtain Icicles
;;  here: `http://www.emacswiki.org/emacs/Icicles_-_Libraries'.
;;
;;  The Icicles commands defined for `highlight.el' are the following:
;;
;;  `icicle-choose-faces', `icicle-choose-invisible-faces',
;;  `icicle-choose-visible-faces', `icicle-hide-faces',
;;  `icicle-hide-only-faces', `icicle-show-faces',
;;  `icicle-show-only-faces'.
;;
;;  These are all Icicles multi-commands, which means that they each
;;  let you choose multiple completion candidates or all candidates
;;  that match your current input (a regexp).  To use them you must
;;  also use Icicles.  You can use command `icicle-hide-faces' to hide
;;  any number of visible faces.  Any text is hidden that has that
;;  face as a text property or an overlay property, depending on the
;;  value of `hlt-use-overlays-flag'.
;;
;;  Command `icicle-show-faces' is the opposite of
;;  `icicle-hide-faces': it shows invisible text that has the faces
;;  you choose.  Neither `icicle-hide-faces' nor `icicle-show-faces'
;;  has any effect on other faces, besides those you choose to hide or
;;  show, respectively; they each do only one thing, hide or show.
;;
;;  Command `icicle-hide-only-faces' hides the faces you choose, and
;;  shows all other faces, and command `icicle-show-only-faces' does
;;  the opposite.  You can thus use these commands to specify exactly
;;  what faces should be invisible and visible.  Empty input means
;;  none: If you choose no faces to hide (that is, hit `RET' with an
;;  empty minibuffer), then all faces will be made visible; if you
;;  choose no faces to show, then all will be hidden.
;;
;;  Currently, face attributes for highlighting are combined when
;;  overlays overlap, but the same is not true for text properties.
;;  For example, if you highlight a word with face `foo', and then you
;;  highlight it with face `bar', only `bar' remains as the face for
;;  that word.  With overlays, the attributes of the two faces are
;;  composed.  When you hide or show faces, this behavior difference
;;  has an effect.
;;
;;  You can hide text using the commands in this library for any of
;;  the purposes that you might use invisible text in Emacs.  This
;;  gives you an easy, interactive way to control which sections of
;;  text are seen by search and other Emacs tools.  Use the regexp
;;  highlighting commands, for instance, to highlight text
;;  syntactically, and then hide that highlighted text.  Or use
;;  `hlt-highlighter' to sweep over text that you want to hide with
;;  the mouse.
;;
;;  Hiding and showing faces also provides a "conditional text"
;;  feature similar to that available in desktop publishing
;;  applications such as Adobe Framemaker.  Publishers often use such
;;  a feature to produce different output documents from the same
;;  source document ("single sourcing").  You can use this feature
;;  similarly, if you have an application (printing is one example)
;;  that is sensitive to whether text is visible or invisible.  One
;;  caveat: Emacs faces are not saved when you save your file.
;;
;;(@* "What Gets Highlighted: Region, Buffer, New Text You Type")
;;  ** What Gets Highlighted: Region, Buffer, New Text You Type **
;;
;;  All mention of the "region" in this commentary should really say
;;  "region or buffer".  If the region is active and non-empty, then
;;  only the text in the region is targeted by the commands in this
;;  library.  This lets you easily control the scope of operations.
;;
;;  If the region is not active or it is empty, then:
;;
;;  - If `hlt-use-overlays-flag' is nil and there is no prefix arg,
;;    then the face is applied to the next characters that you type.
;;
;;  - Otherwise, the face is applied to the entire buffer (or the
;;    current restriction, if the buffer is narrowed).
;;
;;(@* "Interference by Font Lock")
;;  ** Interference by Font Lock **
;;
;;  If you use Emacs 22 or later, then you can use this library in
;;  conjunction with library `font-lock+.el'.  That will prevent
;;  font-locking from removing any highlighting face properties that
;;  you apply using the commands defined here.
;;
;;  Otherwise, when `hlt-use-overlays-flag' is nil, font-lock
;;  highlighting will interfere with the highlighting of this library.
;;  In most cases, you will be able to highlight text, but sooner or
;;  later font-lock will erase that highlighting when it refontifies
;;  the buffer.  If `hlt-use-overlays-flag' is non-nil, there is no
;;  such problem : font-lock has no effect on overlays.
;;
;;(@* "Suggested Bindings")
;;  ** Suggested Bindings **
;;
;;  This library adds menu items to the Region submenu of the Edit
;;  menu-bar menu, if you have a Region submenu.  To obtain this menu,
;;  load library `menu-bar+.el'.
;;
;;  Otherwise, library `highlight.el' makes no key bindings.  Here are
;;  some suggested bindings (`C-x C-y', `C-x mouse-2', `C-x
;;  S-mouse-2', `C-S-p', and `C-S-n', respectively):
;;
;;   (define-key ctl-x-map [(control ?y)] 'hlt-highlight)
;;   (define-key ctl-x-map [(down-mouse-2)] 'hlt-highlighter)
;;   (define-key ctl-x-map [(S-down-mouse-2)] 'hlt-eraser)
;;   (global-set-key [(shift control ?p)]  ; Emacs 21 or later
;;                   'hlt-previous-highlight)
;;   (global-set-key [(shift control ?n)]  ; Emacs 21 or later
;;                   'hlt-next-highlight)
;;
;;  You might also want to bind command `hlt-choose-default-face',
;;  which you can use to change the current default highlighting face.
;;
;;(@* "Relation to Hi-Lock Mode")
;;  ** Relation to Hi-Lock Mode **
;;
;;  The features of this library are complementary to those of the
;;  standard Emacs library `hi-lock.el', so you can use the two
;;  libraries together.
;;
;;(@* "Commands That Won't Work in Emacs 20")
;;  ** Commands That Won't Work in Emacs 20 **
;;
;;  The following commands and options work only for Emacs versions
;;  more recent than Emacs 20:
;;
;;  `hlt-act-on-any-face-flag', `hlt-hide-default-face',
;;  `hlt-highlight-property-with-value', `hlt-next-highlight',
;;  `hlt-previous-highlight', `hlt-show-default-face',
;;  `hlt-toggle-act-on-any-face-flag'.
;;
;;(@* "To Do")
;;  ** To Do **
;;
;;  1. Add commands to show and hide boolean combinations of faces.
;;
;;  2. Faces are not accumulated as text properties.
;;     Highlighting with one face completely replaces the previous
;;     highlight.  Overlays don't have this limitation.  Text
;;     properties need not have it either, but they do, for now.
;;
;;(@* "Acknowledgement")
;;  **  Acknowledgement **
;;
;;  Parts of this library are based on a library of the same name
;;  written and copyrighted by Dave Brennan, brennan@hal.com, in 1992.
;;  I haven't been able to locate that file, so my change log is the
;;  only record I have of what our relative contributions are.
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;(@* "Change log")
;;
;; 2011/12/01 dadams
;;     hlt-eraser: Fixed so it works backwards too.  Thx to Michael Heerdegen.
;;     hlt-unhighlight-region, hlt-replace-highlight-face, hlt-eraser: Use dolist, not mapcar.
;; 2011/11/04 dadams
;;     hlt-default-copy/yank-props: Allow a value of t, for all props.
;;     hlt-props-to-copy/yank: Handle t value of hlt-default-copy/yank-props.
;; 2011/10/31 dadams
;;     hlt-highlight-regexp-region: No occurrences msg if no match, not msg how to unhighlight.
;; 2011/09/13 dadams
;;     hlt-highlight-property-with-value: Corrected interactive spec for VALUES.
;; 2011/07/24 dadams
;;     Moved to icicles-cmd2.el, renamed with prefix icicle- from hlt-, and corrected them:
;;       hlt-(hide|show)(-only)-faces, hlt-choose(-(in)visible)-faces.
;;     menu-bar-edit-menu, facemenu(-mouse)-menu: Added hlt-(copy|yank)-props.
;; 2011/07/23 dadams
;;     Added: hlt-((mouse-)copy|yank|paste)-props, hlt-copied-props, hlt-subplist,
;;            hlt-default-copy/yank-props, hlt-read-props-completing, hlt-props-to-copy/yank.
;;     Added defgroup.  Updated defcustom/defface to use :group highlight.
;;     menu-bar-edit-region-menu: Added hlt-yank-props, hlt-unhighlight-region-for-face.
;;                                Removed needing region for highlighting enablement.
;; 2011/05/05 dadams
;;     icicle-delete-if(-not) -> icicle-remove-if(-not).  Former are obsolete.
;;     hlt-hide-default-face, hlt-next-highlight: Use also memq, not just eq, to test for face.
;; 2011/01/04 dadams
;;     Removed autoload cookies from non def* sexps and non-interactive functions.
;;     Added some missing autoload cookies for commands.
;; 2010/11/26 dadams
;;     Added: hlt-unhighlight-for-overlay.
;;     hlt-eraser, hlt-unhighlight-region:
;;       Use hlt-unhighlight-for-overlay, not hlt-delete-highlight-overlay.
;;     hlt-eraser: Update doc string to reflect new behavior.
;; 2009/09/24 dadams
;;     Removed hlt-no-op - use function ignore instead.
;; 2009/08/02 dadams
;;     Added: hlt(-mouse)-toggle-(link|property)-highlighting, hlt-(un)highlight-all-prop,
;;            hlt-property-highlight, hlt-prop-highlighting-state.
;; 2009/07/31 dadams
;;     Added: hlt-highlight-property-with-value, hlt-flat-list, hlt-set-intersection.
;; 2009/04/26 dadams
;;     hlt-mouse-face-each-line: Bind inhibit-field-text-motion to  t, for real eol.
;; 2008/01/17 dadams
;;     Removed soft require of icicles.el.
;; 2007/11/27 dadams
;;     hlt-highlight-regexp-region: If available, use icicle-read-string-completing.
;; 2007/08/12 dadams
;;     Moved here from menu-bar+.el: Add to Edit>Region menu.  Soft require menu-bar.el.
;; 2007/06/07 dadams
;;     Use face-name-history or icicle-face-name-history, if defined, else face-name-history.
;; 2007/06/05 dadams
;;     Added: hlt-(highlighter|eraser)-mouse.
;; 2007/06/04 dadams
;;     Added: hlt-previous-use-overlays-flag-value.
;;     hlt-use-overlays-flag: 3 values now; default is only.
;;     hlt-eraser, hlt-unhighlight-region, hlt-hide-default-face, hlt-next-highlight:
;;       Treat non-only, non-nil hlt-use-overlays-flag.
;;     hlt-toggle-use-overlays-flag: Use hlt-previous-use-overlays-flag-value.
;;     Updated doc.
;; 2007/06/03 dadams
;;     Added: hlt-toggle-use-overlays-flag.
;;     Don't even define hlt-act-on-any-face-flag for Emacs 20.
;;     Renamed no-op to hlt-no-op. Removed soft require of misc-cmds.el.
;; 2007/06/02 dadams
;;     Added: hlt-act-on-any-face-flag, hlt-add-listifying, hlt-add-to-invisibility-spec,
;;            hlt-choose(-(in)visible)-faces, hlt-(hide|show)(-default-face|-only),
;;            hlt-highlight-faces-in-buffer, hlt-set-union, hlt-toggle-act-on-any-face-flag.
;;     Renamed: highlight-use-overlays-flag to hlt-use-overlays-flag,
;;              highlight-max-region-no-warning to hlt-max-region-no-warning,
;;              highlight-last-regexp to hlt-last-regexp, highlight-last-face to hlt-last-face,
;;              highlight-face to hlt-choose-default-face,
;;              highlight-highlighter to hlt-highlighter, highlight-eraser to hlt-eraser,
;;              mouse-face-each-line to hlt-mouse-face-each-line,
;;              unhighlight-region(-for-face) to hlt-unhighlight-region(-for-face).
;;     hlt-highlighter, hlt-highlight-region, hlt-mouse-face-each-line:
;;       Also put face as hlt-highlight property.
;;     hlt-eraser: Also remove hlt-highlight property.
;;     hlt-highlight-region, hlt-unhighlight-region, hlt-replace-highlight-face,
;;       hlt-next-highlight, hlt-mouse-face-each-line, hlt-highlight-regexp-region:
;;         Made start, end, and regexp args optional too.  Default for non-interactive too.
;;     hlt-unhighlight-region-for-face: Made all args optional.  Default them.
;;     hlt-unhighlight-region: Only remove highlighting for FACE, not all faces.
;;     hlt-highlight-single-quotations: Update hlt-last-face.
;;     hlt-next-highlight:
;;       Respect hlt-act-on-any-face-flag.  Return a cons of the limits.  Added no-error-p arg.
;;     hlt-previous-highlight: Added no-error-p arg.
;;     Added soft require of Icicles.
;; 2007/04/02 dadams
;;     Renamed highlight-region-beg-end to highlight-region-or-buffer-limits.
;; 2007/03/25 dadams
;;     highlight-highlighter, highlight-eraser, highlight-region, unhighlight-region:
;;       Use font-lock-ignore property.
;;     highlight-regexp-*: Use hi-lock-regexp-history or regexp-history.
;; 2007/03/23 dadams
;;     highlight-region:
;;       If no region and no overlay, apply face to next char typed & add to facemenu menu.
;;     highlight-highlighter: Don't create overlay unless highlight-use-overlays-flag.
;;     highlight-highlighter, highlight-region, highlight-eraser:
;;       Don't bother to call font-lock-after-fontify-buffer.
;;     highlight-highlighter, highlight-region: Prepare for possible font-lock-ignore prop.
;;     highlight: Removed message.
;; 2007/03/20 dadams
;;     highlight-face: Add face as arg.  Added final message.
;; 2007/03/17 dadams
;;     Added: highlight-(next|previous)-highlight, highlight-region-beg-end,
;;            highlight-eraser.
;;     highlight-region, highlight-regexp-to-end, highlight-regexp-region:
;;       Interactively, use highlight-last-face as the face.
;;     highlight-single-quotations: Added prefix arg, meaning prompt for face.
;;     highlight-region, highlight-regexp-region, unhighlight-region(-for-face),
;;     *-replace-face, *-single-quotations: If no region, then use whole buffer.
;;     highlight-single-quotations:
;;       Use highlight-regexp-region, not highlight-regexp-to-end.  Msg if interactive.
;;     highlight-regexp-region: Ensure move past match in loop.  Face is optional now.
;;     mouse-face-each-line: Added args start, end, face, msg-p. Restrict to region.
;;     Removed: mouse-face-following-lines.
;;     highlight-region: Added msg-p arg and progress message.
;;     unhighlight-region, highlight-replace-face: Simple message, no where part.
;;     unhighlight-region: Changed order of optional args, for consistency.
;;     highlight-highlighter:
;;       Make overlay once, and move it.  Initialize end-point to start-point.
;; 2007/03/16 dadams
;;     Renamed highlight-regexp to highlight-regexp-to-end, because Emacs now uses that name.
;;     Renamed max-highlight-w-o-warning to highlight-max-region-no-warning.
;;     Added: highlight-use-overlays-flag, highlight-last-face, highlight-face,
;;            highlight-highlighter, unhighlight-region-for-face,
;;            highlight-replace-face, highlight-delete-overlay.
;;     highlight-single-quotations: Read the face name.
;;     highlight-single-quotations, highlight-region, highlight-regexp-to-end,
;;     highlight-regexp-region: Set highlight-last-face.
;;     unhighlight-region, highlight-region, mouse-face-following-lines,
;;     mouse-face-each-line: Respect highlight-use-overlays-flag.
;;     unhighlight-region, mouse-face-*: Added optional face arg.
;;     highlight-max-region-no-warning: defvar -> defcustom.
;;     highlight-regexp-region: Use mouse-p when call highlight-region.
;; 2006/03/31 dadams
;;     No longer use display-in-minibuffer.
;; 2005/12/18 dadams
;;     Use minibuffer-prompt face.  Removed require of def-face-const.el.
;;     highlight-single-quotations: defsubst -> defun.
;; 2004/10/13 dadams
;;     Updated for Emacs 21: highlight-region: Bind
;;       inhibit-modification-hooks to non-nil to prevent Emacs 21
;;       font-lock from refontifying (removing highlighting)
;; 2004/10/12 dadams
;;     highlight-region: Use font-lock-after-fontify-buffer instead of
;;       lazy-lock-after-fontify-buffer.
;; 2004/03/16 dadams
;;     highlight-region: Prevent put-text-property from removing highlighting
;; 1996/04/26  dadams
;;     Put escaped newlines on long-line strings.
;; 1996/04/25  dadams
;;     1. Added highlight-single-quotations.
;;     2. highlight-regexp, highlight-regexp-region: Added new optional arg NTH.
;; 1996/04/25  dadams
;;     Added mouse-face-following-lines.
;; 1996/04/04  dadams
;;     1. highlight: Removed RAW-PREFIX, DISPLAY-MSGS args.  Made PREFIX optional.
;;        Set current-prefix-arg to nil so called fns don't use it as mouse-p.
;;     2. highlight-regexp, highlight-regexp-region: Added MOUSE-P arg.
;; 1996/02/27  dadams
;;     Added mouse-face-each-line.
;; 1996/02/26  dadams
;;     unhighlight-region: Added new arg MOUSE-P.
;; 1996/02/12  dadams
;;     highlight-region: Added optional arg MOUSE-P.
;; 1996/02/06  dadams
;;     Put variable-interactive property on appropriate user option vars.
;; 1996/02/01  dadams
;;     highlight: Just call subfunctions interactively.
;;     highlight-region, highlight-regexp, highlight-regexp-region: Use
;;       read-face-name
;; 1996/01/08  dadams
;;     highlight-regexp, highlight-regexp-region: message -> display-in-minibuffer.
;; 1995/11/09  dadams
;;     highlight-region: FACE arg is optional.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'frame-fns nil t) ;; (no error if not found): flash-ding
(when (< emacs-major-version 21) (require 'faces+ nil t)) ;; (no error if not found):
                                                          ;; read-face-name
(require 'menu-bar+ nil t) ;; (no error if not found): menu-bar-edit-region-menu
;; (require 'icicles nil t)   ;; (no error if not found): icicle-define-command,
                              ;; icicle-face-name-history, icicle-make-face-candidate, 
                              ;; icicle-read-string-completing, icicle-remove-if,
                              ;; icicle-remove-if-not. 

;; Quiet the byte-compiler for Emacs 20
(defvar hi-lock-mode)
(defvar hlt-act-on-any-face-flag)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "Menus")

;;; Menu-Bar `Edit' Menu ---------------------------------------------

(define-key-after menu-bar-edit-menu [hlt-copy-props]
  '(menu-item "Copy Text Properties" hlt-copy-props
    :help "Copy text properties at point, for subsequent pasting") 'paste)
(define-key-after menu-bar-edit-menu [hlt-yank-props]
    '(menu-item "Paste Text Properties" hlt-yank-props
      :help "Paste previously copied text properties to text in region"
      :enable (and mark-active (not buffer-read-only)))
    'hlt-copy-props)

;;; Menu-Bar `Edit' > `Region' Menu ----------------------------------

(when (boundp 'menu-bar-edit-region-menu) ; Defined in `menu-bar+.el'.
  (define-key menu-bar-edit-region-menu [separator-highlight] '("--"))
  (define-key menu-bar-edit-region-menu [hlt-yank-props]
    '(menu-item "Paste Text Properties" hlt-yank-props
      :help "Paste previously copied text properties to text in region"
      :enable (and mark-active (not buffer-read-only))))
  (define-key menu-bar-edit-region-menu [hlt-unhighlight-region-for-face]
    '(menu-item "Unhighlight for Face..." hlt-unhighlight-region-for-face
      :help "Remove highlighting for a given face in the region"))
  (define-key menu-bar-edit-region-menu [hlt-unhighlight-region]
    '(menu-item "Unhighlight" hlt-unhighlight-region
      :help "Remove highlighting (faces) in the region"))
  (define-key menu-bar-edit-region-menu [hlt-highlight-regexp-region]
    '(menu-item "Highlight Regexp..." hlt-highlight-regexp-region
      :help "Highlight parts of region that match a regexp"))
  (define-key menu-bar-edit-region-menu [hlt-highlight-region]
    '(menu-item "Highlight" hlt-highlight-region
      :help "Highlight all text in the region")))

;;; Facemenu `Text Properties' Menu ----------------------------------
(when (boundp 'facemenu-mouse-menu)
  (easy-menu-add-item facemenu-mouse-menu ()
                      ["Paste Text Properties" hlt-yank-props
                                               (and mark-active (not buffer-read-only))] 'dp)
  (easy-menu-add-item facemenu-mouse-menu ()
                      ["Copy Text Properties" hlt-copy-props t] 'dp))
(easy-menu-add-item facemenu-menu ()
                    ["Paste Text Properties" hlt-yank-props
                                             (and mark-active (not buffer-read-only))] 'dp)
(easy-menu-add-item facemenu-menu () ["Copy Text Properties" hlt-copy-props t] 'dp)

 
;;(@* "Variables and Faces")

;;; Variables and Faces ----------------------------------------------

(defgroup highlight nil
  "Highlighting."
  :prefix "hlt-" :group 'editing :group 'convenience :group 'wp :group 'faces
  :link `(url-link :tag "Send Bug Report"
          ,(concat "mailto:" "drew.adams" "@" "oracle" ".com?subject=\
highlight.el bug: \
&body=Describe bug here, starting with `emacs -q'.  \
Don't forget to mention your Emacs and library versions."))
  :link '(url-link :tag "Download" "http://www.emacswiki.org/highlight.el")
  :link '(url-link :tag "Description" "http://www.emacswiki.org/HighLight")
  :link '(emacs-commentary-link :tag "Commentary" "highlight"))

;; This is defined in `faces.el', Emacs 22.  This definition is adapted to Emacs 20.
(unless (facep 'minibuffer-prompt)
  (defface minibuffer-prompt '((((background dark)) (:foreground "cyan"))
                               (t (:foreground "dark blue")))
    "*Face for minibuffer prompts."
    :group 'basic-faces))

(when (fboundp 'next-single-char-property-change) ; Don't bother, for Emacs 20.
  (defface hlt-property-highlight '((((background dark)) (:background "Navy"))
                                    (t (:background "Wheat")))
    "*Face used to highlight all links."
    :group 'highlight :group 'faces)
  (defcustom hlt-act-on-any-face-flag nil
    "*Non-nil means highlight actions apply to all text with a face.
nil means that they apply only to text that has been highlighted.
Consult the doc for particular actions, to see if they are affected by
this option."
    :type 'boolean :group 'highlight)

  (defvar hlt-prop-highlighting-state '(nil . nil)
    "Cons representing the state of property highlighting.
The car indicates whether property highlighting is on (nil means off).
The cdr is the position of the last mouse click that changed state, as
a marker."))

;;;###autoload
(defcustom hlt-max-region-no-warning 100000
  "*Maximum size (chars) of region to highlight without confirmation.
This is used only for highlighting of a regexp, which can be slow."
  :type 'integer :group 'highlight)

;;;###autoload
(defcustom hlt-use-overlays-flag 'only
  "*Non-nil means use overlays to highlight; nil means use text properties.
This value also affects some actions, such as unhighlighting, for text
that is highlighted.  If the value is `only' (the default value), then
those actions only affect overlay highlighting.  Otherwise, they
affect both kinds of highlighting."
  :type '(choice
          (const :tag "Highlight using text properties, not overlays" nil)
          (const :tag "Highlight using overlays, not text properties" only)
          (sexp  :tag
           "Highlight using overlays, but act also on highlight text properties" t))
  :group 'highlight)

;;;###autoload
(defcustom hlt-default-copy/yank-props '(face)
  "*Properties that `hlt-copy-props' and `hlt-yank-props' use by default.
You can use a prefix argument with those commands to override the
default behavior.
Either a list of properties (symbols) or `t', meaning all properties."
  :type '(choice
          (const :tag "All properties" t)
          (repeat (symbol :tag "Property")))
  :group 'highlight)

(defvar hlt-last-regexp nil "The last regexp highlighted.")
(defvar hlt-last-face 'highlight "The last face used by highlight commands.")
(defvar hlt-previous-use-overlays-flag-value nil "Previous value of `hlt-use-overlays-flag'.")

(defvar hlt-copied-props ()
  "Plist of text properties last copied using `hlt-copy-props'.")
 
;;(@* "Misc Functions - Emacs 20+")

;;; Misc Functions - Emacs 20+ ---------------------------------------

;;;###autoload
(defun hlt-choose-default-face (face)
  "Choose a face for highlighting."
  (interactive (list (read-face-name "Use highlighting face: ")))
  (setq hlt-last-face  face)
  (when (interactive-p) (message "Highlighting will now use face `%s'" face)))

;;;###autoload
(defun hlt-highlighter (start-event)    ; Suggested binding: `C-x mouse-2'.
  "Highlight the text you drag the mouse over.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose a different face."
  (interactive "e")
  (save-excursion
    (run-hooks 'mouse-leave-buffer-hook) ; Let temporary modes like isearch turn off.
    (let* ((original-window  (selected-window))
           (echo-keystrokes  0)
           (start-posn       (event-start start-event))
           (start-point      (posn-point start-posn))
           (end-point        start-point)
           (start-window     (posn-window start-posn)))
      (let ((read-only                          buffer-read-only)
            (modified-p                         (buffer-modified-p))
            (inhibit-modification-hooks         t)
            (overlay                            (and hlt-use-overlays-flag
                                                     (make-overlay start-point start-point)))
            ;; Otherwise, `put-text-property' calls this, which would remove highlight.
            (font-lock-fontify-region-function  'ignore)
            event)
        (setq buffer-read-only  nil)
        (track-mouse
          (while (progn (setq event  (read-event))
                        (or (mouse-movement-p event)
                            (memq (car-safe event) '(switch-frame select-window))))
            (unless (memq (car-safe event) '(switch-frame select-window))
              (setq end-point  (posn-point (event-end event))))
            (cond (hlt-use-overlays-flag
                   (setq overlay  (move-overlay overlay start-point end-point))
                   (overlay-put overlay 'face          hlt-last-face)
                   (overlay-put overlay 'hlt-highlight hlt-last-face))
                  (t
                   (put-text-property start-point end-point 'face             hlt-last-face)
                   (put-text-property start-point end-point 'hlt-highlight    hlt-last-face)
                   (put-text-property start-point end-point 'font-lock-ignore t)
                   ))))
        (setq buffer-read-only  read-only)
        (set-buffer-modified-p modified-p)))))

;;;###autoload
(defun hlt-eraser (start-event)         ; Suggested binding: `C-x S-mouse-2'.
  "Erase highlights that you click or drag the mouse over.
If `hlt-use-overlays-flag' is non-nil, then remove overlay
highlighting for the last face that was used for highlighting.  (You
can use command `hlt-choose-default-face' first to choose a different
face.)  If `hlt-use-overlays-flag' is not `only', then remove
text-property highlighting for *ALL* faces (not just highlighting
faces).  This means, in particular, that a value of nil erases both
overlays for the last face and text properties for all faces."
  (interactive "e")
  (save-excursion
    (run-hooks 'mouse-leave-buffer-hook) ; Let temporary modes like isearch turn off.
    (let* ((original-window  (selected-window))
           (echo-keystrokes  0)
           (start-posn       (event-start start-event))
           (start            (posn-point start-posn))
           (end              start)
           (start-window     (posn-window start-posn)))
      (let ((read-only                          buffer-read-only)
            (modified-p                         (buffer-modified-p))
            (inhibit-modification-hooks         t)
            ;; Otherwise, `put-text-property' calls this, which removes highlight.
            (font-lock-fontify-region-function  'ignore)
            event)
        (setq buffer-read-only  nil)
        (track-mouse
          (while (progn (setq event  (read-event))
                        (or (mouse-movement-p event)
                            (memq (car-safe event) '(switch-frame select-window))))
            (unless (memq (car-safe event) '(switch-frame select-window))
              (let ((posn-point  (posn-point (event-end event))))
                (setq end    (max end posn-point)
                      start  (min start posn-point))))
            (when hlt-use-overlays-flag ; Erase overlay properties
              (dolist (ov  (overlays-in start end))
                (hlt-unhighlight-for-overlay ov start end hlt-last-face)))
            (unless (eq 'only hlt-use-overlays-flag) ; Erase text properties
              (remove-text-properties
               start end '(face nil hlt-highlight nil font-lock-ignore nil)))))
        (setq buffer-read-only  read-only)
        (set-buffer-modified-p modified-p)))))

;;;###autoload
(defun hlt-highlighter-mouse ()
  "Same as `hlt-highlighter', but for binding to a menu item."
  (interactive)
  (message "Drag mouse to highlight text") (sleep-for 1)
  (hlt-highlighter (read-event)))

;;;###autoload
(defun hlt-eraser-mouse ()
  "Same as `hlt-eraser', but for binding to a menu item."
  (interactive)
  (message "Drag mouse over to erase highlighting") (sleep-for 1)
  (hlt-eraser (read-event)))

;;;###autoload
(defun hlt-highlight (&optional prefix) ; Suggested binding: `C-x C-y'.
  "Highlight region, regexp (PREFIX +), or unhighlight region (PREFIX -).
PREFIX arg non-negative means `hlt-highlight-regexp-region'
PREFIX arg negative means `hlt-unhighlight-region'
PREFIX arg nil means `hlt-highlight-region'.
If the region is not active or it is empty, then use the whole buffer.
The face used is the last face that was used for highlighting.
You can use command `hlt-choose-default-face' to choose a different face."
  (interactive "P")
  (setq current-prefix-arg  nil)         ; No mouse-p.
  (if prefix
      (if (natnump (prefix-numeric-value prefix))
          (call-interactively 'hlt-highlight-regexp-region)
        (save-excursion (call-interactively 'hlt-unhighlight-region)))
    (call-interactively 'hlt-highlight-region)))

;;;###autoload
(defun hlt-highlight-region (&optional start end face msg-p mouse-p)
  "Highlight either the region/buffer or new input that you type.
Use the region if active, or the buffer otherwise.

If *all* of the following are true, the apply the last-used face as a
text property to the next and subsequent chars that you type, and add
that face to a facemenu menu (`Text Properties' or one of its
submenus):
 * You call this command interactively.
 * You use no prefix arg.
 * Option `prop-use-overlays-flag' is nil
 * The last property used for highlighting was `face'.

Otherwise, the behavior respects `hlt-use-overlays-flag' and depends
on the optional arguments, as follows:

 Optional args START and END are the limits of the area to act on.
  They default to the region limits.  If the region is not active or
  it is empty, then use the whole buffer.

 Optional 3rd arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)

 Optional 4th arg MSG-P non-nil means to display a progress message.
  Interactively, MSG-P is t.

Optional 5th arg MOUSE-P non-nil means use property `mouse-face', not
 `face'.  Interactively, MOUSE-P is provided by the prefix arg."
  (interactive `(,@(hlt-region-or-buffer-limits) nil t ,current-prefix-arg))
  (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                            (setq start  (car start-end)
                                  end    (cadr start-end))))
  (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
  (when (and msg-p (or mark-active mouse-p)) (message "Highlighting..."))
  (let ((read-only                           buffer-read-only)
        (modified-p                          (buffer-modified-p))
        (inhibit-modification-hooks          t)
        ;; Otherwise, `put-text-property' calls this, which removes highlight.
        (font-lock-fontify-region-function  'ignore)
        overlay)
    (setq buffer-read-only  nil)
    (cond (hlt-use-overlays-flag
           (setq overlay  (make-overlay start end))
           (overlay-put overlay (if mouse-p 'mouse-face 'face) face)
           (overlay-put overlay 'hlt-highlight                 face))
          (mouse-p (put-text-property start end 'mouse-face face))
          ((interactive-p)
           (message "Text you type now will have face `%s'." face)
           (facemenu-add-new-face face)
           ;; It is `facemenu-add-face' that either uses region or next insert.
           (facemenu-add-face face (and mark-active start) (and mark-active end))
           (when (and mark-active start end (/= start end))
             (put-text-property start end 'hlt-highlight    face)
             (put-text-property start end 'font-lock-ignore t)))
          (t (put-text-property start end 'face             face)
             (put-text-property start end 'hlt-highlight    face)
             (put-text-property start end 'font-lock-ignore t)))
    (setq buffer-read-only  read-only)
    (set-buffer-modified-p modified-p))
  (let ((remove-msg  (substitute-command-keys
                      "`\\[negative-argument] \\[hlt-highlight]' to remove highlighting.")))
    (when (and msg-p (or mark-active mouse-p))
      (message "Highlighting... done. %s" remove-msg))))

;;;###autoload
(defun hlt-highlight-regexp-region (&optional start end regexp face msg-p mouse-p nth)
  "Highlight regular expression REGEXP in region/buffer.
Use the region if active, or the buffer otherwise.
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional 4th arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional 5th arg MSG-P:
  t means to treat this as an interactive call when deciding to
    display all messages.
  non-nil & non-t means to display only error and warning messages.
Optional 6th arg MOUSE-P non-nil means to use `mouse-face' property,
  not `face'.  Interactively, this is provided by the prefix arg.
Optional 7th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)"
  (interactive
   `(,@(hlt-region-or-buffer-limits) 
     ,(if (fboundp 'icicle-read-string-completing)
          (icicle-read-string-completing "Regexp to highlight: "
                                         hlt-last-regexp
                                         (lambda (c) (string-match "regexp" (symbol-name c)))
                                         (if (and (boundp 'hi-lock-mode) hi-lock-mode)
                                             'hi-lock-regexp-history
                                           'regexp-history))
            (read-string "Regexp to highlight: "
                         nil (if (and (boundp 'hi-lock-mode) hi-lock-mode)
                                 'hi-lock-regexp-history
                               'regexp-history)
                         hlt-last-regexp))
       nil t ,current-prefix-arg)) ; interactive-p means to display all msgs.
  (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                            (setq start  (car start-end)
                                  end    (cadr start-end))))
  (unless regexp (setq regexp  hlt-last-regexp))
  (unless (stringp regexp)      ; Else re-search-forward gets an error
    (error "HIGHLIGHT-REGEXP-REGION: REGEXP arg is not a string: `%S'" regexp))
  (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
  (let ((reg-size  (abs (- end start))))
    (when (and msg-p
               (> reg-size hlt-max-region-no-warning)
               (not (progn
                      (and (fboundp 'flash-ding) ; In `frame-fns.el'
                           (flash-ding 'no-terminate-macros (selected-frame)))
                      (y-or-n-p (substitute-command-keys
                                 (format "Lots of highlighting slows \
things down.  Do you really want to highlight up to %d chars?  "
                                         reg-size))))))
      (error "OK, highlighting was cancelled")))
  (when (eq t msg-p) (message (concat "Highlighting occurrences of `" regexp "'...")))
  (let ((hits-p  nil))
    (save-excursion
      (goto-char start)
      (while (and (< start end) (not (eobp)) (re-search-forward regexp end t)
                  (setq hits-p  t))
        (condition-case nil
            (progn (forward-char 1) (setq start  (1+ (point))))
          (end-of-buffer (setq start  end)))
        (hlt-highlight-region (match-beginning (or nth 0))
                              (match-end (or nth 0)) face nil mouse-p)))
    (when (eq t msg-p)
      (if hits-p
          (message "Highlighting occurrences of `%s' done.  %s" regexp
                   (substitute-command-keys
                    "`\\[negative-argument] \\[hlt-highlight]' to remove highlighting."))
        (message "No occurrences of `%s'" regexp))))
  (setq hlt-last-regexp  regexp))

;;;###autoload
(defun hlt-highlight-regexp-to-end (regexp &optional face msg-p mouse-p nth)
  "Highlight text after cursor that matches REGEXP.
The behavior respects `hlt-use-overlays-flag' and depends on the
optional arguments, as follows:

 Optional 2nd arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different
  face.)

 Optional 3rd arg MSG-P non-nil means to display a progress message.
  Interactively, MSG-P is t.

 Optional 4th arg MOUSE-P non-nil means use property `mouse-face', not
 `face'.  Interactively, MOUSE-P is provided by the prefix arg.

 Optional 5th arg NTH determines which regexp subgroup is highlighted.
  If nil or 0, the entire regexp is highlighted.  Otherwise, the NTH
  regexp subgroup (\"\\\\(...\\\\)\" expression) is highlighted.
  (NTH is not available interactively.)"
  (interactive
   (list (read-string "Regexp to highlight after cursor: " nil
                      (if (and (boundp 'hi-lock-mode) hi-lock-mode)
                          'hi-lock-regexp-history
                        'regexp-history)
                      hlt-last-regexp)
         nil t current-prefix-arg))
  (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
  (let ((remove-msg
         (and msg-p
              (substitute-command-keys
               "`\\[negative-argument] \\[hlt-highlight]' to remove highlighting."))))
    (when msg-p
      (message "Highlighting occurrences of `%s' after cursor..." regexp))
    (hlt-highlight-regexp-region (point) (point-max) regexp face
                                 (and msg-p 'error-msgs-only) mouse-p nth)
    (when msg-p
      (message "Highlighting occurrences of `%s' done.  %s" regexp remove-msg)))
  (setq hlt-last-regexp  regexp))

;;;###autoload
(defun hlt-unhighlight-region (&optional start end face msg-p mouse-p)
  "Remove all highlighting in region or buffer.
Use the region if active, or the buffer otherwise.
The arguments are the same as those for `hlt-highlight-region'.

If `hlt-use-overlays-flag' is non-nil, then remove overlay highlighting.
If `hlt-use-overlays-flag' is not `only', then remove text-property
highlighting.  This means, in particular, that a value of nil removes
both overlays and text properties."
  (interactive `(,@(hlt-region-or-buffer-limits) nil t ,current-prefix-arg))
  (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                            (setq start  (car start-end)
                                  end    (cadr start-end))))
  (when msg-p (message "Removing highlighting..."))
  (let ((read-only-p  buffer-read-only)
        (modified-p   (buffer-modified-p)))
    (setq buffer-read-only  nil)
    (when hlt-use-overlays-flag         ; Unhighlight overlay properties.
      (dolist (ov  (overlays-in start end))
        (hlt-unhighlight-for-overlay ov start end face)))
    (unless (eq 'only hlt-use-overlays-flag) ; Unhighlight text properties.
      (let ((beg  start)
            hi-face)
        (while (< beg end)
          (when (setq hi-face  (get-text-property beg 'hlt-highlight))
            (when (or (null face) (eq hi-face face))
              ;; $$$ Really, we should remove only the part of the `face'
              ;;     property that belongs to Highlight, and set the value to be
              ;;     the same as it is, but without hlt-last-face.
              (remove-text-properties
               beg (1+ beg) (if mouse-p
                                '(mouse-face nil hlt-highlight nil font-lock-ignore nil)
                              '(face nil hlt-highlight nil font-lock-ignore nil)))))
          (setq beg  (1+ beg)))))
    (setq buffer-read-only  read-only-p)
    (set-buffer-modified-p modified-p))
  (when msg-p (message "Removing highlighting... done.")))

;;;###autoload
(defun hlt-unhighlight-region-for-face (&optional face start end mouse-p)
  "Remove any highlighting in the region that uses FACE.
Same as `hlt-unhighlight-region', but removes only highlighting
that uses FACE.  Interactively, you are prompted for the face.

This works only for overlay highlighting, not text-property
highlighting.

Note: When text in the region has been highlighted using more than one
face, unhighlighting for one of those faces can mean that adjacent
highlighting outside the region appears to change.  That outside text
still has the same multiple-overlay face highlighting, but the overlay
stacking order is not the same as it was.

Optional arg FACE is the face to use.
  Interactively, this is the last face that was used for highlighting.
  (You can use command `hlt-choose-default-face' to choose a different face.)
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional arg MOUSE-P non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSE-P is provided by the prefix arg."
  (interactive `(,(read-face-name "Remove highlight overlays that use face: ")
                  ,@(hlt-region-or-buffer-limits) ,current-prefix-arg))
  (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
  (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                            (setq start  (car start-end)
                                  end    (cadr start-end))))
  (hlt-unhighlight-region start end face (interactive-p) mouse-p))

;; No longer used - use `hlt-unhighlight-for-overlay' instead.
(defun hlt-delete-highlight-overlay (overlay &optional face)
  "Delete OVERLAY if it was created by highlighting (library `highlight').
Optional arg FACE is a face symbol.  If non-nil, then delete only
overlays with that FACE."
  (let ((highlight-face  (overlay-get overlay 'hlt-highlight)))
    (when (and highlight-face (or (not face) (eq face highlight-face)))
      (delete-overlay overlay))))

;;; (defun hlt-unhighlight-for-overlay (overlay start end &optional face)
;;;   "Remove OVERLAY highlighting from START to END.
;;; Acts only on an OVERLAY that was created by library `highlight'.
;;; If OVERLAY extends beyond the region from START to END, then replace
;;; it with two overlays: one that ends at START and the other that starts
;;; at END.  Otherwise, delete OVERLAY.
;;; Optional arg FACE is a face symbol.  If non-nil, then delete only
;;; overlays with that FACE."
;;;   (let ((oface   (overlay-get overlay 'hlt-highlight))
;;;         (ostart  (overlay-start overlay))
;;;         (oend    (overlay-end   overlay)))
;;;     (when (and oface (or (not face) (eq face oface)))
;;;       (delete-overlay overlay)
;;;       (when (< ostart start) (hlt-highlight-region ostart start face))
;;;       (when (> oend end) (hlt-highlight-region end oend face)))))

;; This version has an implementation similar to `remove-overlays' in Emacs 22+.
(defun hlt-unhighlight-for-overlay (overlay start end &optional face)
  "Remove OVERLAY highlighting from START to END.
Acts only on an OVERLAY that was created by library `highlight'.
OVERLAY might be moved or split or both.

Optional arg FACE is a face symbol.  If non-nil, then remove only
highlighting with that FACE."
  ;; (overlay-recenter end)                ; Speed up loops over overlays.
  (when (< end start) (setq start (prog1 end (setq end start))))
  (let ((oface   (overlay-get overlay 'hlt-highlight))
        (ostart  (overlay-start overlay))
        (oend    (overlay-end   overlay)))
    (when (and oface (or (not face) (eq face oface)))
      ;; Either push OVERLAY outside region or split it to exclude region
      ;; or delete it (if it is entirely contained in region).
      (if (< ostart start)
          (if (<= oend end)
              (move-overlay overlay ostart start)
            (move-overlay (copy-overlay overlay) ostart start)
            (move-overlay overlay end oend))
        (if (> oend end)
            (move-overlay overlay end oend)
          (delete-overlay overlay))))))

(unless (fboundp 'copy-overlay)         ; Defined in Emacs 22+.
  (defun copy-overlay (o)
    "Return a copy of overlay O."
    (let ((o1 (if (overlay-buffer o)
                  (make-overlay (overlay-start o) (overlay-end o)
                                ;; FIXME: there's no easy way to find the
                                ;; insertion-type of the two markers.
                                (overlay-buffer o))
                (let ((o1 (make-overlay (point-min) (point-min))))
                  (delete-overlay o1)
                  o1)))
          (props (overlay-properties o)))
      (while props
        (overlay-put o1 (pop props) (pop props)))
      o1)))

;;;###autoload
(defun hlt-replace-highlight-face (old-face new-face &optional start end msg-p mouse-p)
  "Replace OLD-FACE by NEW-FACE in overlay highlighting in the region.
This command applies only to overlay highlighting created by library
`highlight.el'.

Update the last-used highlighting face.

With a prefix argument, replace OLD-FACE as the `mouse-face' property,
 not the `face' property.

Other arguments:
 Optional args START and END are the limits of the area to act on.
  They default to the region limits.  If the region is not active or
  it is empty, then use the whole buffer.
 Optional arg MSG-P non-nil means display a progress message.
 Optional arg MOUSE-P non-nil means use `mouse-face' property, not
  `face'.  Interactively, MOUSE-P is provided by the prefix arg."
  (interactive `(,(read-face-name "Replace face in region highlights. Old face: ")
                 ,(read-face-name "New face: ")
                 ,@(hlt-region-or-buffer-limits) t ,current-prefix-arg))
  (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                            (setq start  (car start-end)
                                  end    (cadr start-end))))
  (when msg-p (message "Replacing highlighting face `%s'..." old-face))
  (let ((read-only-p  buffer-read-only)
        (modified-p   (buffer-modified-p)))
    (setq buffer-read-only  nil)
    (dolist (ov  (overlays-in start end))
      (when (eq old-face (overlay-get ov (if mouse-p 'mouse-face 'face)))
        (overlay-put ov (if mouse-p 'mouse-face 'face) new-face)
        (overlay-put ov 'hlt-highlight                 new-face)))
    (setq buffer-read-only  read-only-p)
    (set-buffer-modified-p modified-p))
  (setq hlt-last-face  new-face)
  (when msg-p (message "Replacing overlay highlighting face `%s'... done." old-face)))

;;;###autoload
(defun hlt-highlight-single-quotations (&optional face)
  "Highlight single-quoted text in the region.
This means, for example, commands and keys between `'s: `foobar'.
If the region is not active or it is empty, then use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a different face."
  (interactive "P")
  (if face
      (setq face  (read-face-name "Use highlighting face: ") hlt-last-face face)
    (setq face  hlt-last-face))
  (apply #'hlt-highlight-regexp-region
         (append (hlt-region-or-buffer-limits)
                 (list "`\\([^']+\\)'" face (and (interactive-p) t) nil 1))))

;;;###autoload
(defun hlt-mouse-face-each-line (&optional start end face msg-p)
  "Put `mouse-face' on each line of buffer in region.
If the region is active and not empty, then limit mouse-face
highlighting to the region.  Otherwise, use the whole buffer.
With a prefix argument, prompt for the highlighting face to use.
Otherwise, use the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a different face.
Optional args START and END are the limits of the area to act on.
  They default to the region limits.
Optional arg MSG-P non-nil means display a progress message."
  (interactive `(,@(hlt-region-or-buffer-limits) ,current-prefix-arg t))
  (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                            (setq start  (car start-end)
                                  end    (cadr start-end))))
  (if face
      (setq face  (read-face-name "Use highlighting face: ") hlt-last-face face)
    (setq face  hlt-last-face))
  (when msg-p (message "Putting mouse face `%s' on each line..." face))
  (let ((buffer-read-only           nil)
        (inhibit-field-text-motion  t)  ; Just to be sure, for `end-of-line'.
        overlay)
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (not (eobp))          
          (cond (hlt-use-overlays-flag
                 (setq overlay
                       (make-overlay (point) (setq start  (progn (end-of-line) (point)))))
                 (overlay-put overlay 'mouse-face    face)
                 (overlay-put overlay 'hlt-highlight face))
                (t
                 (put-text-property (point) (progn (end-of-line) (point)) 'mouse-face face)
                 (put-text-property start end 'hlt-highlight face)))
          (forward-line 1)))))
  (when msg-p (message "Putting mouse face `%s' on each line... done." face)))

;;;###autoload
(defun hlt-toggle-use-overlays-flag ()
  "Toggle `hlt-use-overlays-flag'.
If the current value is non-nil, it is set to nil.
If the current value is nil, it is set to the last non-nil value."
  (interactive)
  (let ((before-toggle  hlt-use-overlays-flag))
    (if hlt-use-overlays-flag
        (setq hlt-use-overlays-flag  nil)
      (setq hlt-use-overlays-flag  hlt-previous-use-overlays-flag-value))
    (setq hlt-previous-use-overlays-flag-value  before-toggle))
  (message
   (cond ((eq hlt-use-overlays-flag 'only)
          "Highlight actions now use only overlay properties, not text properties")
         (hlt-use-overlays-flag
          "Highlighting with overlays now, but actions affect also text properties")
         (t "Highlight actions now use only text properties, not overlay properties"))))


;;; Copying and yanking text properties

;;;###autoload
(defalias 'hlt-paste-props 'hlt-yank-props)
;;;###autoload
(defun hlt-yank-props (start end &optional arg msgp)
  "Yank (paste) copied text properties over the active region.
Interactively, do nothing if there is no nonempty active region.
By default, yank only the copied properties defined by
 `hlt-default-copy/yank-props'.
With a plain or non-negative prefix arg, yank all copied properties.
With a negative prefix arg, you are prompted for the copied properties
 to yank.  To finish entering properties, hit `RET RET' (i.e., twice).

NOTE: If the list of copied text properties is empty, then yanking
      REMOVES ALL PROPERTIES from the text in the region.  This
      provides an easy way to UNpropertize text."
  (interactive "r\nP\np")
  ;; Do nothing if no active region.
  (unless (or (and transient-mark-mode mark-active (not (eq (mark) (point))))
              (not msgp))
    (error "No region to paste properties to"))
  (let ((read-only                           buffer-read-only)
        (modified-p                          (buffer-modified-p))
        (inhibit-modification-hooks          t)
        ;; Otherwise, `put-text-property' calls this, which removes highlight.
        (font-lock-fontify-region-function   'ignore)
        (props-to-yank                       (hlt-props-to-copy/yank hlt-copied-props arg)))
    (undo-boundary)
    (setq buffer-read-only  nil)
    (set-text-properties start end props-to-yank)
    ;; Set/reset props `hlt-highlight' and `font-lock-ignore', if `face' is one of the props.
    ;; (The Emacs 20 code here is fudged: it just uses `member' instead of `plist-member'.)
    (cond ((fboundp 'plist-member)
           (put-text-property
            start end 'hlt-highlight    (and (plist-member props-to-yank 'face)  t))
           (put-text-property
            start end 'font-lock-ignore (and (plist-member props-to-yank 'face)  t)))
          (t                            ; Emacs 20 - no `plist-member'.
           (put-text-property
            start end 'hlt-highlight    (and (member 'face props-to-yank)  t))
           (put-text-property
            start end 'font-lock-ignore (and (member 'face props-to-yank)  t))))
    (setq buffer-read-only  read-only)
    (set-buffer-modified-p modified-p)
    (when msgp
      (if props-to-yank
          (message "Yanked propert%s `%s'" (if (car (cddr props-to-yank)) "ies" "y")
                   (let ((pprops  ()))
                     (while props-to-yank
                       (push (pop props-to-yank) pprops)
                       (pop props-to-yank))
                     (mapconcat #'symbol-name (nreverse pprops) "', `")))
        (message "ALL PROPERTIES REMOVED (yanked empty list of properties)")))))

;;;###autoload
(defun hlt-mouse-copy-props (&optional event arg msgp)
  "Same as `hlt-copy-props', but copy at mouse pointer, not at point."
  (interactive "e\nP\np")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (save-excursion (goto-char (posn-point (event-end event)))
                    (hlt-copy-props (point) arg msgp))))

;; For testing
;; (global-set-key [C-S-down-mouse-2] 'hlt-mouse-copy-props)
;; (global-set-key [C-S-mouse-2]      'ignore)

;;;###autoload
(defun hlt-copy-props (&optional position arg msgp)
  "Copy text properties at point for use by `hlt-yank-props'.
Properties are copied to `hlt-copied-props'.
By default, copy the properties defined by
 `hlt-default-copy/yank-props'.
With a plain or non-negative prefix arg, copy all properties.
With a negative prefix arg, you are prompted for the properties to
 copy.  To finish entering properties, hit `RET RET' (i.e., twice)."
  (interactive "d\nP\np")
  (unless position  (setq position  (point)))
  (let ((props-to-copy  (hlt-props-to-copy/yank (text-properties-at position) arg)))
    (setq hlt-copied-props  props-to-copy)
    (when msgp
      (if props-to-copy
          (message "Copied propert%s `%s'" (if (car (cddr props-to-copy)) "ies" "y")
                   (let ((pprops  ()))
                     (while props-to-copy
                       (push (pop props-to-copy) pprops)
                       (pop props-to-copy))
                     (mapconcat #'symbol-name (nreverse pprops) "', `")))
        (message "Emptied copied properties list - yanking will REMOVE ALL")))))

(defun hlt-props-to-copy/yank (avail-props arg)
  "Return a plist of properties to copy or yank.
AVAIL-PROPS is a plist of available properties.
ARG is from a raw prefix argument.
 If nil, then use the properties from AVAIL-PROPS that are also
  among those specified by `hlt-default-copy/yank-props'.
 If a plain or non-negative prefix arg, then use all properties in
  AVAIL-PROPS.
 If a negative prefix arg, then prompt for the properties
  to use, using completion against the candidates in AVAIL-PROPS."
  (cond ((and arg (natnump (prefix-numeric-value arg)))
         (copy-sequence avail-props))   ; Copy/yank all props available.
        (arg                            ; Prompt for props, from among those available.
         (let ((props-avail  avail-props)
               (props-alist  ()))
           (while props-avail
             (push (cons (symbol-name (pop props-avail)) (pop props-avail)) props-alist))
           (if (not (cdr props-alist))
               avail-props
             (hlt-subplist (hlt-read-props-completing props-alist) avail-props))))
        (t                              ; Copy/yank the available default props.
         (if (eq t hlt-default-copy/yank-props)
             avail-props
           (hlt-subplist hlt-default-copy/yank-props avail-props)))))

(defun hlt-subplist (properties available)
  "Return a plist with entries from plist AVAILABLE for PROPERTIES.
PROPERTIES is a list of properties without their values."
  (let ((plist     ())
        (prop+val  nil))
    (dolist (prop  properties)
      (when (setq prop+val  (if (fboundp 'plist-member)
                                (plist-member available prop)
                              (member prop available))) ; Emacs 20 fudge.
        (push prop plist)
        (push (cadr prop+val) plist)))
    (nreverse plist)))

(defun hlt-read-props-completing (props)
  "Read text properties from among those in PROPS.
PROPS is an alist whose cars are text property names (strings)."
  (let ((prompt1        "Property (RET for each, empty input to finish): ")
        (prompt2        "Property: ")
        (props-to-copy  ())
        prop)
    (setq prop   (completing-read prompt1 props nil t)
          props  (delete (assoc prop props) props))
    (unless (string= "" prop)
      (push (intern prop) props-to-copy)
      (while (and props (not (string= "" prop)))
        (setq prop   (completing-read prompt2 props nil t)
              props  (delete (assoc prop props) props))
        (unless (string= "" prop) (push (intern prop) props-to-copy)))
      (nreverse props-to-copy))))
 
;;(@* "Misc Functions - Emacs 21+")

;;; Misc Functions - Emacs 21+ ---------------------------------------

(when (fboundp 'next-single-char-property-change) ; Don't bother, for Emacs 20.
  (defun hlt-show-default-face (face)
    "Show FACE, by default, the default highlighting face.
With a prefix argument, prompt for the highlighting face to show.
Otherwise, show the last face used for highlighting.
 You can also use command `hlt-choose-default-face' to choose a different face."
    (interactive (list (if current-prefix-arg
                           (read-face-name "Show highlighting face: ")
                         hlt-last-face)))
    (hlt-listify-invisibility-spec)
    (remove-from-invisibility-spec face))

  (defun hlt-listify-invisibility-spec ()
    "Convert `buffer-invisibility-spec' to list form.
If it is already a list, do nothing.
If it is t, set it to a list of all `invisible' spec values in the buffer.
That is, for each character in the buffer that has property `invisible',
the invisibility criteria specified by that value are accumulated."
    (unless (listp buffer-invisibility-spec)
      (setq buffer-invisibility-spec  nil)
      (let ((start  (point-min))
            (end    (point-max))
            spec)
        (dolist (ov  (overlays-in start end))
          (when (setq spec  (overlay-get ov 'invisible))
            (unless (listp spec) (setq spec  (list spec)))
            (setq buffer-invisibility-spec
                  (hlt-set-union spec buffer-invisibility-spec))))
        (while (< start end)
          (when (setq spec  (get-text-property start 'invisible))
            (unless (listp spec) (setq spec  (list spec)))
            (setq buffer-invisibility-spec
                  (hlt-set-union spec buffer-invisibility-spec)))
          (setq start  (1+ start)))))
    buffer-invisibility-spec)

  ;; From `cl-seq.el', function `union', without keyword treatment.
  (defun hlt-set-union (list1 list2)
    "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or
LIST2.  This is a non-destructive function; it copies the data if
necessary."
    (cond ((null list1) list2)
          ((null list2) list1)
          ((equal list1 list2) list1)
          (t
           (or (>= (length list1) (length list2))
               (setq list1  (prog1 list2 (setq list2  list1)))) ; Swap them.
           (while list2
             (unless (member (car list2) list1)
               (setq list1  (cons (car list2) list1)))
             (setq list2  (cdr list2)))
           list1)))

  ;; From `cl-seq.el', function `intersection', without keyword treatment.
  (defun hlt-set-intersection (list1 list2)
    "Set intersection of lists LIST1 and LIST2.
This is a non-destructive operation: it copies the data if necessary."
    (and list1 list2
         (if (equal list1 list2)
             list1
           (let ((result  ()))
             (unless (>= (length list1) (length list2))
               (setq list1  (prog1 list2 (setq list2  list1)))) ; Swap them.
             (while list2
               (when (member (car list2) list1)
                 (setq result  (cons (car list2) result)))
               (setq list2  (cdr list2)))
             result))))

  (defun hlt-hide-default-face (&optional start end face)
    "Hide the last face used for highlighting.
With a prefix argument, prompt for the highlighting face to hide,
 instead.  You can also use command `hlt-choose-default-face' to
 choose a different face.

If `hlt-act-on-any-face-flag' is non-nil, then the face to be hidden
can be any face you choose.  Otherwise, it must be a face that has
been used for highlighting.

Hiding a face at some location means two things:
1) setting its `invisible' property there, making it susceptible to
   being hidden by `buffer-invisibility-spec', and
2) adding it to `buffer-invisibility-spec', so that it is hidden.

This command hides all text with the specified face that has the
`invisible' property, throughout the entire buffer.  However, it only
adds the `invisible' property to text with an overlay or text
property, depending on `hlt-use-overlays-flag', and it only does so
within the region, if the region is active.

Non-interactively:
FACE is the face to hide. It defaults to the last highlighting face.
START and END are the limits of the area to act on. They default to
  the region limits."
    (interactive `(,@(hlt-region-or-buffer-limits)
                   ,(if current-prefix-arg
                        (read-face-name "Hide highlighting face: ")
                        hlt-last-face)))
    (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                              (setq start  (car start-end)
                                    end    (cadr start-end))))
    (hlt-listify-invisibility-spec)
    ;; Add FACE to `invisible' property throughout START...END,
    ;; whenever it is used as a highlighting face.
    (save-excursion
      (save-window-excursion
        (goto-char start)
        (let ((zone-beg  start)
              zone-end zone)
          (while (and zone-beg (< zone-beg end))
            (setq zone      (hlt-next-highlight zone-beg end face nil nil 'no-error-msg)
                  zone-beg  (car zone)
                  zone-end  (cdr zone))
            ;; Add FACE to `invisible' property from `zone-beg' to `zone-end'.
            (when hlt-use-overlays-flag
              (let ((overlays  (overlays-at zone-beg)))
                (while overlays
                  (when (and (or hlt-act-on-any-face-flag
                                 (eq face (overlay-get (car overlays) 'hlt-highlight)))
                             (eq face (overlay-get (car overlays) 'face)))
                    (overlay-put (car overlays) 'invisible
                                 (hlt-add-listifying
                                  (overlay-get (car overlays) 'invisible)
                                  face)))
                  (when overlays (setq overlays  (cdr overlays))))))
            (when (and (not (eq hlt-use-overlays-flag 'only))
                       (or hlt-act-on-any-face-flag
                           (eq face (get-text-property (point) 'hlt-highlight)))
                       ;; $$$$$$ (eq face (get-text-property (point) 'face)))
                       (let ((pt-faces  (get-text-property (point) 'face)))
                         (if (consp pt-faces) (memq face pt-faces) (eq face pt-faces))))
              (put-text-property zone-beg zone-end 'invisible
                                 (hlt-add-listifying
                                  (get-text-property zone-beg 'invisible)
                                  face)))
            (hlt-add-to-invisibility-spec face))))))

  ;; Same as `add-to-invisibility-spec', except it doesn't allow duplicates.
  (defun hlt-add-to-invisibility-spec (element)
    "Add ELEMENT to `buffer-invisibility-spec'.
See documentation for `buffer-invisibility-spec' for the kind of elements
that can be added."
    (when (eq buffer-invisibility-spec t) (setq buffer-invisibility-spec  (list t)))
    (add-to-list 'buffer-invisibility-spec element))

  (defun hlt-add-listifying (orig-val val-to-add)
    "Add VAL-TO-ADD to list ORIG-VAL, listifying ORIG-VAL first if needed."
    (unless (listp orig-val) (setq orig-val  (list orig-val)))    
    (add-to-list 'orig-val val-to-add)
    orig-val)

  ;; Suggested binding: `C-S-n'.
  (defun hlt-next-highlight (&optional start end face mouse-p backward-p no-error-p)
    "Go to the next highlight in FACE.
Interactively, FACE is the last face used for highlighting, but
you can use command `hlt-choose-default-face' to choose a different face.

If `hlt-act-on-any-face-flag' is non-nil, then the target face can be
any face you choose.  Otherwise, it must be a face that has been used
for highlighting.

If `hlt-use-overlays-flag' is non-nil, then overlay highlighting is
targeted.  If `hlt-use-overlays-flag' is not `only', then
text-property highlighting is targeted.  This means, in particular,
that a value of nil targets both overlays and text properties.

If the region is active and not empty, then limit movement to the
region.  Otherwise, use the whole buffer.
When called non-interactively:

 - non-nil argument NO-ERROR-P means do not raise an error if no
   highlight with FACE is found, and leave point at END.

 - Return a cons of the limits of the text starting at point that has
   property `hlt-highlight' of value FACE: (BEGIN-FACE . END-FACE), where
   BEGIN-FACE is point and END-FACE is the first position just after
   value FACE ends."
    (interactive `(,@(hlt-region-or-buffer-limits) nil ,current-prefix-arg))
    (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                              (setq start  (car start-end)
                                    end    (cadr start-end))))
    (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
    (when backward-p (setq end  (prog1 start (setq start  end))))
    (let ((face-found  nil)
          (orig-point  (point))
          (beg         start))
      (while (and (not (if backward-p (bobp) (eobp)))
                  (not (eq face face-found))
                  (not (= beg end)))
        (save-restriction
          (narrow-to-region beg end)
          (setq beg  (if backward-p
                         (goto-char (previous-single-char-property-change
                                     (point) (if mouse-p 'mouse-face 'face)
                                     nil (point-min)))
                       (goto-char (next-single-char-property-change
                                   (point) (if mouse-p 'mouse-face 'face)
                                   nil (point-max))))))
        (when hlt-use-overlays-flag
          (let ((overlays  (overlays-at (point))))
            (while overlays
              (when (and (or hlt-act-on-any-face-flag
                             (eq face (overlay-get (car overlays) 'hlt-highlight)))
                         (eq face (overlay-get (car overlays) 'face)))
                (setq face-found  face
                      overlays    ()))
              (when overlays (setq overlays  (cdr overlays))))))
        (when (and (not face-found)
                   (not (eq hlt-use-overlays-flag 'only))
                   (or hlt-act-on-any-face-flag
                       (eq face (get-text-property (point) 'hlt-highlight)))
                   ;; $$$$$$ (eq face (get-text-property (point) 'face)))
                   (let ((pt-faces  (get-text-property (point) 'face)))
                     (if (consp pt-faces) (memq face pt-faces) (eq face pt-faces))))
          (setq face-found  face)))
      (unless (or (and (eq face face-found) (not (eq (point) orig-point))) no-error-p)
        (goto-char orig-point)
        (error "No %s highlight with face `%s'" (if backward-p "previous" "next") face)))
    (unless (interactive-p)
      (cons (point)
            (next-single-char-property-change (point) (if mouse-p 'mouse-face 'face)
                                              nil (if backward-p start end)))))

  ;; Suggested binding: `C-S-p'.
  (defun hlt-previous-highlight (&optional start end face mouse-p no-error-p)
    "Go to the previous highlight in the last face used for highlighting.
This is the same as `hlt-previous-highlight', except movement is backward."
    (interactive `(,@(hlt-region-or-buffer-limits) nil ,current-prefix-arg))
    (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                              (setq start  (car start-end)
                                    end    (cadr start-end))))
    (hlt-next-highlight start end face mouse-p t no-error-p))

  (defun hlt-highlight-faces-in-buffer (start end)
    "List of highlighting faces in current buffer between START and END.
This includes faces used in overlays and as text properties.
Only highlighting faces are included, that is, faces associated with a
`hlt-highlight' property."
    (save-excursion
      (save-window-excursion
        (let ((faces  ())
              (beg  start)
              face)
          (setq end  (min end (point-max)))
          (goto-char beg)
          (while (< beg end)
            (save-restriction
              (narrow-to-region beg end)
              (setq beg  (goto-char (next-single-char-property-change (point) 'face
                                                                      nil (point-max)))))
            (when (setq face  (get-text-property (point) 'hlt-highlight))
              (add-to-list 'faces face))
            (let ((overlays  (overlays-at (point))))
              (while overlays
                (when (and (overlay-get (car overlays) 'hlt-highlight)
                           (setq face  (overlay-get (car overlays) 'face)))
                  (add-to-list 'faces face)
                  (setq overlays  ()))
                (when overlays (setq overlays  (cdr overlays))))))
          faces))))

  (defun hlt-toggle-act-on-any-face-flag ()
    "Toggle `hlt-act-on-any-face-flag'."
    (interactive)
    (setq hlt-act-on-any-face-flag  (not hlt-act-on-any-face-flag))
    (message (if hlt-act-on-any-face-flag
                 "Highlight actions now apply to any face, not just a highlighting face"
               "Highlight actions now apply only to a highlighting face")))
  )
 
;;(@* "Functions for Highlighting Propertized Text - Emacs 21+")

;;; Functions for Highlighting Propertized Text - Emacs 21+ ----------

(when (fboundp 'next-single-char-property-change) ; Don't bother, for Emacs 20.
  (defun hlt-highlight-property-with-value (prop &optional values start end face
                                            type msg-p mouse-p)
    "Highlight text in region with property PROP of a value in VALUES.
Non-nil VALUES means do this only where PROP has a value in VALUES.
Interactively, you are prompted for PROP and VALUES.  For VALUES you
  can enter either a list or a single, non-list value.  A list is
  always interpreted as a list of values, not as a single list value.
  Using `RET' with no input means highlight for any non-nil value.

Optional args START and END are the limits of the area to act on.
  They default to the region limits (buffer, if no active region).
Optional 5th arg FACE is the face to use for highlighting.
  Interactively, this is the last face that was used for highlighting.
  (Use command `hlt-choose-default-face' to choose a different face.)
Optional 6th arg TYPE is `overlay', `text', or nil, and specifies the
  type of character property - nil means to look for both overlay and
  text properties.  Interactively, TYPE is derived from
  `hlt-use-overlays-flag'.
Optional 7th arg MSG-P non-nil means to display a progress message.
Optional 8th arg MOUSE-P non-nil means use the `mouse-face' property,
  not the `face' property, for highlighting.  Interactively, MOUSE-P
  is provided by the prefix arg."
    (interactive
     `(,(intern (read-string "Property to highlight: " nil 'highlight-property-history))
       ,(let* ((strg  (read-string "Property value: "))
               (vals  (if (string= "" strg)
                          ()
                        (car (read-from-string strg)))))
              (unless (listp vals) (setq vals  (list vals)))
              vals)
       ,@(hlt-region-or-buffer-limits)
       nil
       ,(if hlt-use-overlays-flag
            (if (eq hlt-use-overlays-flag 'only) 'overlay nil)
            'text)
       t
       ,current-prefix-arg))
    (unless (and start end) (let ((start-end  (hlt-region-or-buffer-limits)))
                              (setq start  (car start-end)
                                    end    (cadr start-end))))
    (if face (setq hlt-last-face  face) (setq face  hlt-last-face))
    (when (and msg-p (or mark-active mouse-p)) (message "Highlighting..."))
    (let ((zone-end  nil))
      (unless (and start end)
        (setq start  (point-min)
              end    (point-max)))
      (condition-case highlight-property-with-value
          (save-excursion
            (while (and (< start end)
                        (let* ((charval  (and (or (not type) (eq type 'overlay))
                                              (get-char-property start prop)))
                               (textval  (and (or (not type) (eq type 'text))
                                              (get-text-property start prop)))
                               (currval  (hlt-flat-list charval textval)))
                          (if values
                              (not (hlt-set-intersection values currval))
                            (not currval))))
              (setq start  (next-single-char-property-change start prop nil end)))
            (while (and start (< start end))
              (setq zone-end  (or (next-single-char-property-change start prop nil end) end))
              (hlt-highlight-region start zone-end face nil mouse-p)
              (setq start  zone-end)
              (while (and (< start end)
                          (let* ((charval  (and (or (not type) (eq type 'overlay))
                                                (get-char-property start prop)))
                                 (textval  (and (or (not type) (eq type 'text))
                                                (get-text-property start prop)))
                                 (currval  (hlt-flat-list charval textval)))
                            (if values
                                (not (hlt-set-intersection values currval))
                              (not currval))))
                (setq start  (next-single-char-property-change start prop nil end)))))
        (quit (hlt-unhighlight-region start end face))
        (error (hlt-unhighlight-region start end face)
               (error (error-message-string highlight-property-with-value)))))
    (let ((remove-msg  (substitute-command-keys
                        "`\\[negative-argument] \\[hlt-highlight]' to remove highlighting.")))
      (when (and msg-p (or mark-active mouse-p))
        (message "Highlighting... done. %s" remove-msg))))

  (defun hlt-flat-list (val1 val2)
    "Return a flat list with all values in VAL1 and VAL2."
    (let ((result  ()))
      (unless (listp val1) (setq val1  (list val1)))
      (unless (listp val2) (setq val2  (list val2)))
      (while val1 (add-to-list 'result (pop val1)))
      (while val2 (add-to-list 'result (pop val2)))
      result))

  (defun hlt-mouse-toggle-link-highlighting ()
    "Alternately highlight and unhighlight links on a mouse click.
Do nothing if the click is at a different location from the last one. 
This calls `hlt-toggle-link-highlighting' to do the toggling.
Links in the entire buffer are affected, even if the region is active.
This is intended to be used on `post-command-hook'."
    (when (and (string-match "mouse" (format "%S" (event-basic-type last-command-event)))
               (memq 'click (event-modifiers last-command-event)))
      (let* ((estart  (event-start last-command-event))
             (pos     (copy-marker (posn-point estart))))
        (when (integer-or-marker-p pos)
          (save-excursion
            (with-current-buffer (window-buffer (posn-window estart))
              (when (condition-case nil
                        (get-char-property (min pos (point-max)) 'mouse-face)
                      (error nil))
                (hlt-toggle-link-highlighting nil nil pos))))))))

  ;; Use it like this:
  ;; (add-hook 'post-command-hook 'hlt-mouse-toggle-link-highlighting)

  (defun hlt-toggle-link-highlighting (&optional start end pos)
    "Alternately highlight and unhighlight links.
A link is considered to be any text with property `mouse-face'.
Calls `hlt-toggle-property-highlighting', passing the args."
    (interactive `(,@(hlt-region-or-buffer-limits)))
    (hlt-toggle-property-highlighting 'mouse-face start end 'hlt-property-highlight
                                      (interactive-p) nil pos))

  (defun hlt-mouse-toggle-property-highlighting (prop &optional face msg-p mouse-p)
    "Alternately highlight and unhighlight text on a mouse click.
Do nothing if the click is at a different location from the last one. 
Call `hlt-toggle-link-highlighting', passing the args.
Propertized text in the entire buffer is (un)highlighted, even if the
region is active.
This is intended to be used on `post-command-hook'."
    (when (and (string-match "mouse" (format "%S" (event-basic-type last-command-event)))
               (memq 'click (event-modifiers last-command-event)))
      (let* ((estart  (event-start last-command-event))
             (pos     (copy-marker (posn-point estart))))
        (when (integer-or-marker-p pos)
          (save-excursion
            (with-current-buffer (window-buffer (posn-window estart))
              (when (condition-case nil
                        (get-char-property (min pos (point-max)) prop)
                      (error nil))
                (hlt-toggle-property-highlighting prop nil nil
                                                  face (interactive-p) mouse-p pos))))))))

  ;; Use it like this:
  ;; (add-hook 'post-command-hook
  ;;           (lambda () (hlt-mouse-toggle-property-highlighting myprop myface)

  (defun hlt-toggle-property-highlighting (prop &optional start end face
                                           msg-p mouse-p pos)
    "Alternately highlight/unhighlight all text that has property PROP.
Highlighting is done using overlays.
Optional arg POS is a buffer position.  If it is the same as the
  position recorded in `hlt-prop-highlighting-state', then do not
  toggle.  In any case, update `hlt-prop-highlighting-state' with POS.
Other args are the same as for `hlt-highlight-property-with-value'."
    (interactive
     `(,(intern (read-string "Property to highlight: " nil 'highlight-property-history))
       ,@(hlt-region-or-buffer-limits) 
       nil  t  ,current-prefix-arg))
    (when (or (not pos) (equal pos (cdr hlt-prop-highlighting-state)))
      (cond ((car hlt-prop-highlighting-state)
             (hlt-unhighlight-all-prop prop start end face (interactive-p) mouse-p)
             (setcar hlt-prop-highlighting-state  nil))
            (t
             (hlt-highlight-all-prop prop start end face (interactive-p) mouse-p)
             (setcar hlt-prop-highlighting-state t))))
    (when pos (setcdr hlt-prop-highlighting-state  pos)))

  (defun hlt-highlight-all-prop (prop &optional start end face msg-p mouse-p)
    "Highlight all text that has a non-nil property PROP using FACE.
Highlight using overlays.
Args are the same as for `hlt-highlight-property-with-value'."
    (interactive `(,@(hlt-region-or-buffer-limits)))
    (hlt-highlight-property-with-value
     prop () start end face 'overlay (interactive-p) mouse-p))

  (defun hlt-unhighlight-all-prop (prop &optional start end face msg-p mouse-p)
    "Unhighlight all text highlighted with face `hlt-property-highlight'.
Args are the same as for `hlt-highlight-property-with-value'."
    (interactive `(,@(hlt-region-or-buffer-limits)))
    (let ((hlt-use-overlays-flag  'only))
      (hlt-unhighlight-region-for-face face start end mouse-p)))

  )
 
;;(@* "General functions")

;;; General functions

;; This is the same as `region-or-buffer-limits' in `misc-fns.el'.
(defun hlt-region-or-buffer-limits ()
  "Return the start and end of the region as a list, smallest first.
If the region is empty or not active, then bob and eob are used."
  (if (or (not mark-active) (null (mark)) (= (point) (mark)))
      (list (point-min) (point-max))
    (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; highlight.el ends here

