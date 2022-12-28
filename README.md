# Mininotes
A small terminal text editor

## Features
 - grapheme navigation
 - line numbers
 - status bar
 - loading and saving
 - mouse to cursor position
 - undo/redo
 - selection
 - copy/paste
 - pageup/pagedown scroll
 - skip words navigation

## Usage
Usage: [OPTIONS] <FILE-PATH>

File path: path to the file to edit.

Options
 - -r --relative-line-numbers: enables relative line numbers
 - -d --disable-mouse-interaction: disables mouse clicking and dragging to move the cursor
 - -t --tab-width: how wide a tab is displayed.
 - -h --help: list these options

## Keybinds
 - arrow keys: move cursor
 - Ctrl+arrow keys: move cursor, and skip past words (the first whitespace after a non-whitespace character)
 - Shift+arrow keys: move cursor, and add selection
 
 - Ctrl+c: copy selection
 - Ctrl+v: paste selection
 - Ctrl+x: cut selection
 
 - Alt+c: copy selection to system clipboard
 - Alt+v: paste selection from system clipboard
 - Alt+x: cut selection to system clipboard
 
 - Ctrl+z: undo change
 - Ctrl+y: redo change
 - Alt+d: discard changes since last save
 
 - Ctrl+s: save changes to file
 
 - Alt+q: quit (does not ask for confirmation)

## Platforms
Should work on most desktop platforms and terminals

Tested on linux, windows and termux (android)

## Licence
MIT, see LICENCE for details
