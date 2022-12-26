# Mininotes
A small terminal text editor

Mainly serves as an example on how to make text editing work on both terminal and gui, in a decent way.

Feel free to use this as reference for whatever you want.

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
 - Alt+q: discard changes since last save
 
 - Ctrl+s: save changes to file
 
 - Alt+q: quit (does not ask for confirmation)

## Platforms
Should work on most desktop platforms and terminals

Tested on linux, windows and termux (android)

## Jank
The code is not without jank.
I've put everything in a single file, which can make code navigation slightly harder.

There's also quite a lot of different integer types being used.
crossterm uses u16 for sizes, I use usize for the text editor part, usize for communicating between crossterm and the editor, and u32 for UI.

While most of the methods should work in any context, I haven't implemented every feature, and some might miss some handy arguments if you want to use it,
due to the code mostly being written for mininotes itself.

bringing in the entirety of clap (and clap_derive) for only a few command line args may also have been over the top. 

I might come back to fix some of these shortcomings, but I'm not sure, as I'd like to make a bigger, better text editor with what I learned here.

## Licence
WTFPL
