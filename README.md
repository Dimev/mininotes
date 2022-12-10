# MiniNotes
A tiny terminal and gui notetaking app

Mainly serves as an example on how to make text editing work on both terminal and gui, in a decent way.

Feel free to use this as reference for whatever you want

Current goals:
 - render to the terminal - V
 - make grapheme cluster navigation work - V
 - insert extra whitespace to match the previous line's whitespace that was before the cursor V
 - allow loading and saving files ~ Saving is kinda meh as it will open a file if it doesn't exist, but wont properly make the path to it
 - make terminal cursor position and text layout work (including scrolling) V
 - render to a gui (via minifb)
 - make gui terminal position and text layout work ~ layout works, can't do input on it yet 
 - basic undo/redo/copy/paste ~ Undo-Redo almost works nicely, selection as well
 - status bar V
 - pageup/pagedown scroll
 - skip words navigation
 - maybe bidir

# Licence
WTFPL

Does not apply on fira-code.ttf, which is instead under the open font licence 
