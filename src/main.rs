// terminal deps
use crossterm::{
    cursor::{self, CursorShape},
    event::{
        poll, read, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent,
        KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
    },
    execute, queue, style,
    style::Color,
    terminal,
};

use std::collections::VecDeque;
use std::{
    io::{stdout, Write},
    path::PathBuf,
};

// editor deps
use ropey::{Rope, RopeSlice};
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete};
use unicode_width::UnicodeWidthChar;

use std::ops::Range;

// clipboard, only include on desktop platforms so it can compile on termux
#[cfg(any(
    target_os = "windows",
    target_os = "macos",
    target_os = "linux",
    target_os = "freebsd",
    target_os = "dragonfly",
    target_os = "openbsd",
    target_os = "netbsd"
))]
use arboard::Clipboard;

#[cfg(not(any(
    target_os = "windows",
    target_os = "macos",
    target_os = "linux",
    target_os = "freebsd",
    target_os = "dragonfly",
    target_os = "openbsd",
    target_os = "netbsd"
)))]
#[allow(non_snake_case)]
mod Clipboard {
    pub fn new() -> Result<DummyClip, ()> {
        Err(())
    }

    pub struct DummyClip(String);

    impl DummyClip {
        pub fn get_text(&mut self) -> Result<String, ()> {
            Ok(self.0.clone())
        }

        pub fn set_text(&mut self, string: String) {
            self.0 = string;
        }
    }
}

// arg parsing
use clap::Parser;

// unicode tools =============================================

const TERM_TAB_WIDTH: usize = 0;

// test strings
// Ｈｅｌｌｏ, ｗｏｒｌｄ!
// اربك تكست هو اول موقع يسمح لزواره الكرام بتحويل الكتابة العربي الى كتابة مفهومة من قبل اغلب برامج التصميم
// 👩‍🔬👩🔬
// 🇷🇸🇮🇴

/// return the start byte index of the unicode grapheme if the cursor is advanced by amount
/// returns the length of the rope if it's too far
pub fn move_grapheme(amount: isize, mut byte_cursor: usize, text: RopeSlice) -> usize {
    // make a cursor
    let mut cursor = GraphemeCursor::new(byte_cursor, text.len_bytes(), true);

    // make the context
    let (mut chunk, mut chunk_idx, _, _) = text.chunk_at_byte(byte_cursor);

    // loop for as long as needed
    for _ in 0..amount.abs() {
        loop {
            match if amount > 0 {
                cursor.next_boundary(chunk, chunk_idx)
            } else {
                cursor.prev_boundary(chunk, chunk_idx)
            } {
                // nothing, stop
                Ok(None) => {
                    return if amount > 0 { text.len_bytes() } else { 0 };
                }

                // found a border, move the cursor there
                Ok(Some(n)) => {
                    byte_cursor = n;
                    break;
                }

                // need more context
                Err(GraphemeIncomplete::NextChunk) => {
                    // get the next chunk
                    let (next_chunk, next_chunk_idx, _, _) =
                        text.chunk_at_byte(chunk_idx + chunk.len());
                    chunk = next_chunk;
                    chunk_idx = next_chunk_idx;
                }

                // need more context
                Err(GraphemeIncomplete::PrevChunk) => {
                    // get the previous chunk
                    let (prev_chunk, prev_chunk_idx, _, _) = text.chunk_at_byte(chunk_idx - 1);
                    chunk = prev_chunk;
                    chunk_idx = prev_chunk_idx;
                }

                // provide context
                Err(GraphemeIncomplete::PreContext(n)) => {
                    // get the context
                    let ctx = text.chunk_at_byte(n - 1).0;
                    cursor.provide_context(ctx, n - ctx.len());
                }

                _ => unreachable!(),
            }
        }
    }

    // and return the found byte position
    byte_cursor
}

/// with of a char iterator in unicode characters
/// this helps when working with terminal UI layout
pub fn string_width<I: IntoIterator<Item = char>>(iterator: I, tab_width: usize) -> usize {
    iterator
        .into_iter()
        .map(|x| {
            if x == '\t' {
                tab_width
            } else {
                x.width_cjk().unwrap_or(0)
            }
        })
        .sum()
}

/// newline, as recognized by ropey, so either one of line feed, carriage return, vertical tab, form feed, next line, line separator, paragraph separator
// TODO: char iter?
pub fn is_newline(c: char) -> bool {
    [
        '\n', '\r', '\u{000B}', '\u{000C}', '\u{0085}', '\u{2028}', '\u{2029}',
    ]
    .contains(&c)
}

// general layout ===========================================

pub trait LineLayout {
    type Iter<'a>: Iterator<Item = GraphemePosition>;

    /// Layout a line
    fn layout_line<'a>(&self, line: RopeSlice<'a>) -> Self::Iter<'a>;
}

/// grapheme information
pub struct GraphemePosition {
    /// where it starts
    pub start_column: usize,

    /// where it ends
    pub end_column: usize,

    /// and where the cursor is in the string, in bytes
    pub cursor: usize,
}

// terminal layout ===========================================

/// Layout settings, which there are none of
// TODO: bidir?
pub struct TermLineLayoutSettings {
    tab_width: usize,
}

impl TermLineLayoutSettings {
    pub fn new(tab_width: usize) -> Self {
        Self { tab_width }
    }
}

impl LineLayout for TermLineLayoutSettings {
    type Iter<'a> = TermLineLayout<'a>;

    fn layout_line<'a>(&self, line: RopeSlice<'a>) -> TermLineLayout<'a> {
        TermLineLayout::new(line, self.tab_width)
    }
}

/// iterator state
pub struct TermLineLayout<'a> {
    /// line
    line: RopeSlice<'a>,

    /// where the cursor currently is
    cursor: usize,

    /// current column
    column: usize,

    /// how wide a tab is
    tab_width: usize,
}

impl<'a> TermLineLayout<'a> {
    pub fn new(line: RopeSlice<'a>, tab_width: usize) -> Self {
        Self {
            line,
            tab_width,
            cursor: 0,
            column: 0,
        }
    }
}

impl<'a> Iterator for TermLineLayout<'a> {
    type Item = GraphemePosition;

    fn next(&mut self) -> Option<GraphemePosition> {
        // stop if we are at the end
        if self.cursor == self.line.len_bytes() {
            None
        } else {
            // advance one char
            let next_cursor = move_grapheme(1, self.cursor, self.line);

            // get the slice
            let rope_slice = self.line.byte_slice(self.cursor..next_cursor);

            // figure out the width
            // newlines are control characters, so they will have 0 width
            let grapheme_width = string_width(rope_slice.chars(), self.tab_width);

            // make the found grapheme
            let grapheme = GraphemePosition {
                start_column: self.column,
                end_column: self.column + grapheme_width,
                cursor: self.cursor,
            };

            // move the cursor
            self.cursor = next_cursor;

            // update where we are
            self.column += grapheme_width;

            // and return
            Some(grapheme)
        }
    }
}

// editor ====================================================

/// undo/redo action
#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub enum EditorAction {
    /// delete a string at the character index
    Delete(usize, String),

    /// insert a string at the character index
    Insert(usize, String),
}

/// multiline text editor
pub struct TextEditor<L: LineLayout> {
    /// text
    text: Rope,

    /// cursor position, in bytes
    cursor: usize,

    /// cursor target column
    target_column: usize,

    /// scolled lines
    scroll_lines: usize,

    /// scrolled columns
    scroll_columns: usize,

    /// line layout settings
    layout_settings: L,

    /// selection anchor, aka where it starts
    /// the end range is the cursor
    selection_anchor: Option<usize>,

    /// undo/redo buffer
    history: VecDeque<EditorAction>,

    /// where the last save was in the history, if any
    save_anchor: Option<usize>,

    /// where we are right now in the history
    current_history: usize,

    /// max undo/redo buffer size
    history_size: usize,

    /// width of a tab
    tab_width: usize,
}

impl<L: LineLayout> TextEditor<L> {
    /// create a new editor from the given string and layout settings
    /// newly loaded means it's unsaved
    pub fn new(content: &str, layout_settings: L, tab_width: usize, newly_loaded: bool) -> Self {
        Self {
            text: Rope::from_str(content),
            cursor: 0,
            target_column: 0,
            scroll_lines: 0,
            scroll_columns: 0,
            layout_settings,
            selection_anchor: None,
            history: VecDeque::new(),
            save_anchor: if newly_loaded { None } else { Some(0) },
            current_history: 0,
            history_size: 16384,
            tab_width,
        }
    }

    /// convert the contents of the editor to a string
    pub fn to_string(&self) -> String {
        self.text.to_string()
    }

    /// set the save point in the history to now
    /// this means that the current content should be saved to a file
    pub fn set_saved(&mut self) {
        self.save_anchor = Some(self.current_history);
    }

    /// get if the contents have been changed since last save
    pub fn has_changed_since_save(&self) -> bool {
        self.save_anchor != Some(self.current_history)
    }

    /// discards all changes since the save
    pub fn discard_changes(&mut self) {
        // undo until we reach the save point
        while self
            .save_anchor
            .map(|x| self.current_history > x)
            .unwrap_or(false)
        {
            self.undo();
        }

        // redo until we reach the save point
        while self
            .save_anchor
            .map(|x| self.current_history < x)
            .unwrap_or(false)
        {
            self.redo();
        }
    }

    /// undo a change
    pub fn undo(&mut self) {
        // can only undo if we have history
        if self.current_history > 0 {
            // move backward in the history
            self.current_history -= 1;

            // get the current change
            let change = self.history[self.current_history].clone();

            // and undo the change
            match change {
                EditorAction::Delete(cursor, string) => {
                    // insert the string again
                    self.insert_string(cursor, &string, false, true, true);
                }
                EditorAction::Insert(cursor, string) => {
                    // remove the range
                    self.remove_range(cursor, cursor + string.len(), false, true, true);
                }
            }
        }
    }

    /// redo a change
    pub fn redo(&mut self) {
        // can only redo if we're not at the end of history
        if self.history.len() > self.current_history {
            // get the change
            let change = self.history[self.current_history].clone();

            // move forward
            self.current_history += 1;

            // and redo the change
            match change {
                EditorAction::Insert(cursor, string) => {
                    // insert the text
                    self.insert_string(cursor, &string, false, true, true);
                }
                EditorAction::Delete(cursor, string) => {
                    // delete the range
                    self.remove_range(cursor, cursor + string.len(), false, true, true);
                }
            }
        }
    }

    /// insert a new, single change
    pub fn do_change(&mut self, change: EditorAction) {
        // purge history after this point
        while self.history.len() > self.current_history {
            self.history.pop_back();
        }

        // insert it
        self.history.push_back(change);
        self.current_history += 1;

        // purge end of the queue if it's too long
        // make sure that the save anchor and current point in history remain in the history however
        while self.current_history > 0
            && self.save_anchor.map(|x| x > 0).unwrap_or(true)
            && self.history.len() > self.history_size
        {
            // move these back
            self.current_history -= 1;
            self.save_anchor = self.save_anchor.map(|x| x - 1);

            // and pop the front off
            self.history.pop_front();
        }
    }

    /// clear the selection, unselect text, don't edit it
    pub fn clear_selection(&mut self) {
        self.selection_anchor = None;
    }

    /// get the current selection, if any
    pub fn get_selection(&self) -> Option<String> {
        // get the selection range, convert it to char indices, and get it from the text
        self.get_selection_range()
            .map(|range| self.text.byte_to_char(range.start)..self.text.byte_to_char(range.end))
            .map(|range| self.text.slice(range).to_string())
    }

    /// remove the current selection, and return it, if any
    pub fn cut_selection(&mut self) -> Option<String> {
        // get the selection range, if any
        let range = self.get_selection_range()?;

        // get the text inside it, in the proper character range
        let string = self
            .text
            .slice(self.text.byte_to_char(range.start)..self.text.byte_to_char(range.end))
            .to_string();

        // and remove it
        self.remove_range(range.start, range.end, true, true, true);

        // remove the selection
        self.clear_selection();

        // and return
        Some(string)
    }

    /// get the selection range
    pub fn get_selection_range(&self) -> Option<Range<usize>> {
        self.selection_anchor.map(|x| {
            if x > self.cursor {
                self.cursor..x
            } else {
                x..self.cursor
            }
        })
    }

    /// add to the selection, based on the current cursor position
    pub fn add_selection(&mut self) {
        // if it's empty, start, otherwise simply retain it
        if self.selection_anchor.is_none() {
            self.selection_anchor = Some(self.cursor)
        }
    }

    /// get the character currently under the cursor
    pub fn get_character_under_cursor(&self) -> char {
        self.text.char(self.text.byte_to_char(self.cursor))
    }

    /// get the character before the curosr, if any
    pub fn get_character_in_front_of_cursor(&self) -> Option<char> {
        self.text
            .get_char(self.text.byte_to_char(self.cursor).checked_sub(1)?)
    }

    /// move cursor horizontally, and save the cursor column if needed
    pub fn move_cursor_horizontal(
        &mut self,
        amount: isize,
        add_selection: bool,
        save_column: bool,
    ) {
        // add to the selection if needed, do this first so it makes most sense
        if add_selection {
            self.add_selection();
        } else {
            // move the cursor to the end with the correct position
            self.cursor = if amount > 0 {
                self.cursor
                    .max(self.selection_anchor.unwrap_or(self.cursor))
            } else {
                self.cursor
                    .min(self.selection_anchor.unwrap_or(self.cursor))
            };

            // then clear the selection
            self.clear_selection();
        }

        // just move it
        self.cursor = move_grapheme(amount, self.cursor, self.text.slice(..));

        // and recalculate the target column
        if save_column {
            self.target_column = self.get_cursor_column();
        }
    }

    /// move the cursor horizontally past a word, and save the column if needed
    pub fn move_cursor_horizontal_words(
        &mut self,
        amount: isize,
        add_selection: bool,
        save_column: bool,
    ) {
        // forward or backward
        if amount > 0 {
            // skip past any whitespaces
            while self.cursor < self.text.len_bytes()
                && self.get_character_under_cursor().is_whitespace()
            {
                self.move_cursor_horizontal(1, add_selection, save_column);
            }

            // skip past any normal characters
            while self.cursor < self.text.len_bytes()
                && !self.get_character_under_cursor().is_whitespace()
            {
                self.move_cursor_horizontal(1, add_selection, save_column);
            }
        } else {
            // skip past any whitespaces
            while self.cursor > 0
                && self
                    .get_character_in_front_of_cursor()
                    .unwrap()
                    .is_whitespace()
            {
                self.move_cursor_horizontal(-1, add_selection, save_column);
            }

            // skip past any normal characters
            while self.cursor > 0
                && !self
                    .get_character_in_front_of_cursor()
                    .unwrap()
                    .is_whitespace()
            {
                self.move_cursor_horizontal(-1, add_selection, save_column);
            }
        }
    }

    /// move cursor vertically, and save the column if needed
    pub fn move_cursor_vertical(&mut self, amount: isize, add_selection: bool, save_column: bool) {
        // add to the selection
        if add_selection {
            self.add_selection();
        } else {
            // move the cursor to the end with the correct position
            self.cursor = if amount > 0 {
                self.cursor
                    .max(self.selection_anchor.unwrap_or(self.cursor))
            } else {
                self.cursor
                    .min(self.selection_anchor.unwrap_or(self.cursor))
            };

            // then clear this
            self.clear_selection();
        }

        // find where the next line is
        let next_line = (self.text.byte_to_line(self.cursor) as isize + amount)
            .max(0)
            .min(self.text.len_lines().saturating_sub(1) as isize) as usize;

        // find where it starts and move there
        self.cursor = self.text.line_to_byte(next_line);

        // move horizontally until we are at the target
        self.move_cursor_to_column(self.target_column, add_selection, save_column);
    }

    /// insert a character
    pub fn insert_character_at_cursor(&mut self, character: char) {
        // first, remove selection
        self.cut_selection();

        // get the string to insert
        let mut buffer = [0 as u8; 4];
        let string = character.encode_utf8(&mut buffer);

        // insert the string
        self.insert_string(self.cursor, string, true, true, true);
    }

    /// insert a string
    pub fn insert_string_at_cursor(&mut self, string: &str) {
        // first, remove selection
        self.cut_selection();

        // insert the string
        self.insert_string(self.cursor, string, true, true, true);
    }

    /// insert a tab
    pub fn insert_tab_at_cursor(&mut self) {
        // short for this
        self.insert_character_at_cursor('\t');
    }

    /// insert a newline
    /// also inserts all spaces preceeding the current line, up to the cursor position
    pub fn insert_newline_at_cursor(&mut self) {
        // first, remove selection
        self.cut_selection();

        // get the line we are currently on
        let line_num = self.text.byte_to_line(self.cursor);

        // get the current line
        let line = self.text.line(line_num);

        // where the cursor is in chars
        let line_char_start = self.text.line_to_char(line_num);

        // where the cursor is right now
        let line_char_pos = self.text.byte_to_char(self.cursor);

        // figure out the whitespaces preceeding that line, up to
        let pred_whitespace = line
            .chars()
            .take_while(|x| x.is_whitespace() && !is_newline(*x))
            .take(line_char_pos - line_char_start);

        // chain that with a newline
        let string = "\n".chars().chain(pred_whitespace).collect::<String>();

        // insert the string, with the added whitespace
        self.insert_string(self.cursor, &string, true, true, true);
    }

    /// remove a character at the cursor, or the selection, if any
    /// before means remove it before the cursor, wich is what would be expected if a character was removed with backspace
    pub fn remove_character_or_selection_at_cursor(&mut self, before: bool) {
        // remove the selection, if any
        if self.cut_selection().is_some() {
            // no need to do anything else
            return;
        }

        // get where the "end" of the character range is
        let end_range = move_grapheme(
            if before { -1 } else { 1 },
            self.cursor,
            self.text.slice(..),
        );

        // get the byte start and end range
        let (start, end) = if before {
            (end_range, self.cursor)
        } else {
            (self.cursor, end_range)
        };

        // remove it
        self.remove_range(start, end, true, true, true);
    }

    /// insert a string of characters, at the start byte indicated
    /// assumes that start points at a correct grapheme boundary
    /// record indicates whether or not to record this action in the history
    pub fn insert_string(
        &mut self,
        start: usize,
        string: &str,
        record: bool,
        store_cursor: bool,
        move_cursor_after: bool,
    ) {
        // clear this
        self.clear_selection();

        // get the start position in chars
        let start_char = self.text.byte_to_char(start);

        // insert the text
        self.text.insert(start_char, string);

        // insert the change, if it was anything
        if record && string.len() > 0 {
            self.do_change(EditorAction::Insert(start, string.to_string()));
        }

        // fix cursor
        if self.cursor > start {
            // after the range, so move it over the right amount of bytes to compensate for the added range
            self.cursor += string.len();
        }

        // move the cursor to after the inserted text
        if move_cursor_after {
            // move it after the inserted item
            self.cursor = start + string.len();
        }

        // and recalculate the target column
        if store_cursor {
            self.target_column = self.get_cursor_column();
        }
    }

    /// remove a range of characters, in bytes, start..end
    /// assumes that the range is in correct graphemes
    /// record indicates whether or not to record this action in the history
    pub fn remove_range(
        &mut self,
        start: usize,
        end: usize,
        record: bool,
        store_cursor: bool,
        move_cursor_after: bool,
    ) {
        // clear this
        self.clear_selection();

        // find the start and end range in chars
        let start_char = self.text.byte_to_char(start);
        let end_char = self.text.byte_to_char(end);

        // find the slice we are going to remove
        let string = self.text.slice(start_char..end_char).to_string();

        // remove the slice
        self.text.remove(start_char..end_char);

        // insert the change, if it was anything
        if record && string.len() > 0 {
            self.do_change(EditorAction::Delete(start, string));
        }

        // fix cursor
        if self.cursor >= start && self.cursor < end {
            // inside the range, so move it to the start of the range
            self.cursor = start;
        } else if self.cursor > start {
            // after the end, so move it over the right amount of bytes to compensate the removed range
            self.cursor -= end - start;
        }

        // move cursor to the deleted text
        if move_cursor_after {
            self.cursor = start;
        }

        // and recalculate the target column
        if store_cursor {
            self.target_column = self.get_cursor_column();
        }
    }

    /// move the cursor to a specific column, assuming a terminal program
    pub fn move_cursor_to_column(&mut self, column: usize, add_selection: bool, save_column: bool) {
        // add to the selection if needed
        if add_selection {
            self.add_selection();
        } else {
            self.clear_selection();
        }

        // move the cursor to the start of the line
        self.move_cursor_to_start_of_line(add_selection, save_column);

        // get the current line
        let line = self.text.line(self.text.byte_to_line(self.cursor));

        // layout the line, and find the right grapheme that contains our column, or at least the one closest to it
        let cursor_pos = self
            .layout_settings
            .layout_line(line)
            .take_while(|x| x.start_column <= column)
            .last()
            .map(|x| self.cursor + x.cursor);

        // need to do this here, otherwise the borrowchecker can't see we don't need to borrow self in line when doing move_cursor
        if let Some(cursor) = cursor_pos {
            self.cursor = cursor;
        } else {
            // otherwise, move it to the end of the line, as that would be expected
            self.move_cursor_to_end_of_line(add_selection, save_column);
        }
    }

    /// move the cursor to the start of the line
    pub fn move_cursor_to_start_of_line(&mut self, add_selection: bool, save_column: bool) {
        // add to the selection if needed
        if add_selection {
            self.add_selection();
        } else {
            self.clear_selection();
        }

        self.cursor = self.text.line_to_byte(self.text.byte_to_line(self.cursor));
        if save_column {
            self.target_column = 0;
        }
    }

    /// move the cursor to the end of the line
    pub fn move_cursor_to_end_of_line(&mut self, add_selection: bool, save_column: bool) {
        // add to the selection
        if add_selection {
            self.add_selection();
        } else {
            self.clear_selection();
        }

        // do nothing if we are at the end of the file
        if self.cursor != self.text.len_bytes() {
            // move it to the start of the next line
            let line = self.text.byte_to_line(self.cursor);

            // find where the next line starts
            let next_line_start = self
                .text
                .line_to_byte((line + 1).min(self.text.len_lines()));

            // move it to the next one
            self.cursor = next_line_start;

            // move it back, if we're not at the last line
            if line + 1 < self.text.len_lines() {
                self.move_cursor_horizontal(-1, add_selection, save_column);
            }
        }
    }

    /// get the current position of the column the cursor is on, assuming a terminal program
    pub fn get_cursor_column(&self) -> usize {
        // get the line number
        let line_num = self.text.byte_to_line(self.cursor);

        // get the line
        let line = self.text.line(line_num);

        // and the start of the line
        let line_start = self.text.line_to_byte(line_num);

        // find the column
        // not using find directly here, as that may not properly find the end of the string
        self.layout_settings
            .layout_line(line)
            .take_while(|x| x.cursor + line_start < self.cursor)
            .last()
            .map(|x| x.end_column)
            .unwrap_or(0)
    }

    /// get the row and column
    /// this is equivalent to getting the cursor position assuming a terminal text layout
    pub fn get_row_and_column(&self) -> (usize, usize) {
        // get the line
        let line_num = self.text.byte_to_line(self.cursor);

        // get the real line
        let line = self.text.line(line_num);

        // get where it starts
        let line_start = self.text.line_to_char(line_num);

        // get where we are, in chars
        let line_pos = self.text.byte_to_char(self.cursor) - line_start;

        // and loop over the line until we are at the right width
        let column = line
            .chars()
            .take(line_pos)
            .map(|x| x.width_cjk().unwrap_or(0))
            .sum();

        (line_num, column)
    }

    /// get the cursor pos, assuming a terminal program
    /// this is the true cursor position, not adjusted by scrolling
    pub fn get_cursor_pos(&self) -> (usize, usize) {
        // get the line and line number
        let line = self.text.byte_to_line(self.cursor);

        // and the column
        let column = self.get_cursor_column();

        (column, line)
    }

    /// gets the cursor position adjusted by scrolling
    pub fn get_relative_cursor_pos(&self) -> Option<(usize, usize)> {
        let (x, y) = self.get_cursor_pos();

        // only return if it's in frame
        Some((
            x.checked_sub(self.scroll_columns)?,
            y.checked_sub(self.scroll_lines)?,
        ))
    }

    /// set the cursor pos
    pub fn set_cursor_pos(&mut self, x: usize, y: usize, add_selection: bool) {
        // find the line
        let line = y.min(self.text.len_lines());

        // find where the line start
        let line_start = self.text.line_to_byte(line);

        // move cursor to the right line
        self.cursor = line_start;

        // and move the cursor to the right column
        self.move_cursor_to_column(x, add_selection, true);
    }

    /// set the cursor pos, relative to scrolling
    pub fn set_relative_cursor_pos(&mut self, x: usize, y: usize, add_selection: bool) {
        // set with the absolute coords
        self.set_cursor_pos(
            x + self.scroll_columns,
            y + self.scroll_lines,
            add_selection,
        );
    }

    /// set the right scrolling values so the text stays in frame
    /// margins are how many characters before and after on that axis should remain visible
    pub fn set_scroll(
        &mut self,
        width: usize,
        height: usize,
        width_margin: usize,
        height_margin: usize,
    ) {
        // get the cursor position
        let (x, y) = self.get_cursor_pos();

        self.scroll_lines = self
            .scroll_lines
            .max(y.saturating_sub(height.saturating_sub(height_margin.min(height / 2) + 1)))
            .min(y.saturating_sub(height_margin.min(height / 2)));

        self.scroll_columns = self
            .scroll_columns
            .max(x.saturating_sub(width.saturating_sub(width_margin.min(width / 2) + 1)))
            .min(x.saturating_sub(width_margin.min(width / 2)));
    }

    /// get the number of lines
    pub fn len_lines(&self) -> usize {
        self.text.len_lines()
    }

    /// get the top row that is visible
    pub fn get_first_visible_line(&self) -> usize {
        self.scroll_lines
    }

    /// get the current line the cursor is on
    pub fn get_current_line(&self) -> usize {
        self.text.byte_to_line(self.cursor)
    }
}

// line numbers ==========================================

/// line numbers to show
#[derive(Copy, Clone)]
pub struct LineNumbers {
    /// start of the range
    start: usize,

    /// total number of lines
    total: usize,

    /// current line
    current: usize,

    /// whether it's relative
    relative: bool,
}

impl LineNumbers {
    pub fn new(start: usize, total: usize, current: usize, relative: bool) -> Self {
        Self {
            start,
            total,
            current,
            relative,
        }
    }

    /// get how wide this should be
    pub fn width(self, height: usize) -> usize {
        self.width_number(height) + 2
    }

    /// how wide the number should be
    pub fn width_number(self, height: usize) -> usize {
        if self.relative {
            // get the max difference
            let max_diff = height;

            // get the one with the most digits, aka highest
            let max = max_diff.max(self.total);

            // figure out the number of digits
            (max as f64).log10() as usize + 1
        } else {
            // number of digits
            (self.total as f64).log10() as usize + 1
        }
    }
}

// terminal ==============================================

/// character type
#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct Char {
    /// character
    c: char,

    /// color
    color: Highlight,
}

impl Char {
    pub fn new_text(c: char, selected: bool) -> Self {
        Self {
            c,
            color: if selected {
                Highlight::Selection
            } else {
                Highlight::Text
            },
        }
    }

    pub fn new(c: char, color: Highlight) -> Self {
        Self { c, color }
    }
}

/// buffer type
type ColoredString = Vec<Char>;

/// type of highlight
#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub enum Highlight {
    /// basic text
    Text,

    /// selected text
    Selection,

    /// the left gutter, or the line numbers for this editor
    Gutter,

    /// status bar
    Status,
}

impl Highlight {
    /// returns the foreground color
    pub fn get_color_foreground_crossterm(self) -> Color {
        match self {
            Self::Text => Color::Reset,
            Self::Selection => Color::Black,
            Self::Gutter => Color::Yellow,
            Self::Status => Color::Black,
        }
    }

    /// returns the background color
    pub fn get_color_background_crossterm(self) -> Color {
        match self {
            Self::Text => Color::Reset,
            Self::Selection => Color::Blue,
            Self::Gutter => Color::Reset,
            Self::Status => Color::Grey,
        }
    }
}

// ui actions ===========================================

/// Enum representing an action in the UI
/// This can be used for both in and out, as they are similar enough
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum UiEvent {
    /// clicked a position
    Clicked(usize, usize),

    /// dragged from and to a position
    Dragged(usize, usize, usize, usize),

    /// inserted a character
    Insert(char),

    /// insert newline
    InsertNewline,

    /// backspace
    Backspace,

    /// delete
    Delete,

    /// undo
    Undo,

    /// redo
    Redo,

    /// copy
    Copy,

    /// paste
    Paste,

    /// cut
    Cut,

    /// system clipboard copy
    SysCopy,

    /// system clipboard paste
    SysPaste,

    /// system clipboard cut
    SysCut,

    /// discard changes
    Discard,

    /// save changes
    Save,

    /// quit
    Quit,
}

// ui layout/terminal drawing ===========================

// TODO: figure out a better way to do this
// a good way of doing it:
// provide a layout struct
// this struct keeps track of remaining area
// it also manages all rendering/interaction
// similar to tui, and handles some things better hopefully

/// Layout align
#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub enum Align {
    /// Stick to the bottom.
    /// Takes maximum size left and right
    Bottom,

    /// Stick to the top.
    /// Takes maximum size left and right
    Top,

    /// Stick to the left.
    /// Takes maximum size top and bottom
    Left,

    /// Stick to the right.
    /// Takes maximum size top and bottom
    Right,
}

/// Layout restrictions
pub enum Restriction {
    /// grow as far out as possible
    Grow,

    /// become as small as possible
    Shrink,
}

/// layout item
pub struct LayoutItem<'a, R: DrawResult, I, O: OutputResult> {
    /// x position, left to right
    x: u32,

    /// y position, top to bottom
    y: u32,

    /// width of the element
    width: u32,

    /// height of the element
    height: u32,

    /// widget
    widget: &'a dyn Widget<R, I, O>,
}

/// UI layout widget
/// if this needs to be expanded more, layers of UI layout can be added, where each layer is stacked on top of eachother
/// if UI layout is then done in stages (once for interaction, once for visuals) it would be possible to add those hovering panels with ease (assuming no interaction needed)
pub struct Layout<'a, R: DrawResult, I, O: OutputResult> {
    /// all items
    items: Vec<LayoutItem<'a, R, I, O>>,

    /// currently leftover space
    /// as a rectangle
    space: (u32, u32, u32, u32),
}

impl<'a, R: DrawResult, I, O: OutputResult> Layout<'a, R, I, O> {
    /// create a new layout
    pub fn new(width: u32, height: u32) -> Self {
        Self {
            space: (0, 0, width, height),
            items: Vec::new(),
        }
    }

    /// handle interactions
    pub fn interact(self, interactions: &I) -> O {
        // simply pass the interaction over all widgets, and combine their result
        // because `O` is a monoid, folds are trivial
        self.items
            .iter()
            .map(|x| x.widget.interact(interactions, x.x, x.y, x.width, x.height))
            .fold(O::empty(), O::combine)
    }

    /// draw the UI
    pub fn draw(self) -> R {
        // start with any empty area that is left over
        let mut result = R::empty(self.space.2 - self.space.0, self.space.3 - self.space.1);

        // figure out the current start positions
        let (mut x, mut y, mut width, mut height) = (
            self.space.0,
            self.space.1,
            self.space.2 - self.space.0,
            self.space.3 - self.space.1,
        );

        // next, go over all widgets backwards
        for item in self.items.iter().rev() {
            // calculate the new sizes
            let (new_x, new_y, new_width, new_height) = (
                // the lowest position is the new origin
                x.min(item.x),
                y.min(item.x),

                // if one of these changed, the width/height for that axis also changed
                if item.x != x {
                    item.width + width
                } else {
                    width
                },
                if item.y != y {
                    item.height + height
                } else {
                    height
                },
            );

            // figure out how to combine it with the current state
            // because we can guarantee we only expand to either one of the sides, this can be done somewhat easily
            if item.x > x {
                // expand to the right
                result = result.combine_horizontal(
                    item.widget.draw(item.width, item.height),
                    new_width,
                    new_height,
                );
            } else if item.x < x {
                // expand to the left
                result = item
                    .widget
                    .draw(item.width, item.height)
                    .combine_horizontal(result, new_width, new_height);
            } else if item.y > y {
                // expand to the bottom
                result = result.combine_vertical(
                    item.widget.draw(item.width, item.height),
                    new_width,
                    new_height,
                );
            } else if item.y < y {
                // expand to the top
                result = item
                    .widget
                    .draw(item.width, item.height)
                    .combine_vertical(result, new_width, new_height);
            }

            // set the new sizes
            x = new_x;
            y = new_y;
            width = new_width;
            height = new_height;
        }

        // return the result
        result
    }

    /// add an item to the UI layout
    /// This is intended to work via the builder pattern, so it consumes self, and returns a modified version
    pub fn add_item(
        mut self,
        item: &'a dyn Widget<R, I, O>,
        align: Align,
        restriction: Restriction,
    ) -> Self {
        // figure out the orientation and in what direction to certainly expand
        let expand_horizontal = match align {
            Align::Top | Align::Bottom => true,
            Align::Left | Align::Right => false,
        };

        // current width and height
        let width = self.space.2 - self.space.0;
        let height = self.space.3 - self.space.1;

        // figure out the size this will take up
        let (min_width, min_height) = item.minimum_size(width, height);
        let (max_width, max_height) = item.maximum_size(width, height);

        // actually scale the widget
        let (widget_width, widget_height) = if expand_horizontal {
            (
                width,
                match restriction {
                    Restriction::Grow => max_height.min(height),
                    Restriction::Shrink => min_height.min(height),
                },
            )
        } else {
            (
                match restriction {
                    Restriction::Grow => max_width.min(width),
                    Restriction::Shrink => min_width.min(width),
                },
                height,
            )
        };

        // figure out the top left corner
        let (widget_x, widget_y) = match align {
            Align::Top | Align::Left => (self.space.0, self.space.1),
            Align::Bottom => (self.space.0, self.space.3 - widget_height),
            Align::Right => (self.space.2 - widget_width, self.space.1),
        };

        // resize the available space
        // if the widget is positioned at the available space corner, it needs to move over by the widget size
        // otherwise, the other end of the corner needs to move to the widget corner, as it is now filled
        if widget_x == self.space.0 {
            self.space.0 = widget_x + widget_width
        } else {
            self.space.2 = widget_x
        }
        if widget_y == self.space.1 {
            self.space.1 = widget_y + widget_height
        } else {
            self.space.3 = widget_y
        }

        // add the widget
        self.items.push(LayoutItem {
            x: widget_x,
            y: widget_y,
            width: widget_width,
            height: widget_height,
            widget: item,
        });

        // return
        self
    }
}

/// UI widget
pub trait Widget<R: DrawResult, I, O: OutputResult>: Drawable<R> + Interactive<I, O> {
    /// Returns the preferred minimum width and height, frrom the available width and height
    fn minimum_size(&self, width: u32, height: u32) -> (u32, u32);

    /// returns the preferred maximum width and height, from the available width and height
    fn maximum_size(&self, width: u32, height: u32) -> (u32, u32);
}

/// draw a widget
/// `R` is the result a draw produces
pub trait Drawable<R: DrawResult> {
    /// draw
    fn draw(&self, width: u32, height: u32) -> R;
}

/// handle input for a widget
/// `I` is the input type, `O` is the result of an interaction
pub trait Interactive<I, O: OutputResult> {
    /// react to events
    fn interact(&self, input: &I, x: u32, y: u32, width: u32, height: u32) -> O;
}

/// Trait representing a result of a draw
/// These need to be combined after layout to produce a full draw result, so it needs to implement those functions
pub trait DrawResult {
    /// Produce a result for an empty area
    fn empty(width: u32, height: u32) -> Self;

    /// combine horizontally, self is left, other is right
    fn combine_horizontal(self, other: Self, width: u32, height: u32) -> Self;

    /// combine vertically, self is top, other is bottom
    fn combine_vertical(self, other: Self, width: u32, height: u32) -> Self;
}

/// Trait representing a result of an interaction
/// Multiple interactions may happen for a widget, so they need to be combined
pub trait OutputResult {
    /// Produce a result for an empty interaction
    fn empty() -> Self;

    /// combine the results
    fn combine(self, other: Self) -> Self;
}

// TODO: trait to convert element into a widget

/// pane split left and right
#[derive(Copy, Clone)]
pub struct VerticalPane<'a, L: TerminalBuffer, R: TerminalBuffer> {
    left: &'a L,
    right: &'a R,
    mode: SplitMode,
}

/// pane split top and bottom
#[derive(Copy, Clone)]
pub struct HorizontalPane<'a, L: TerminalBuffer, R: TerminalBuffer> {
    pub top: &'a L,
    pub bottom: &'a R,
    pub mode: SplitMode,
}

/// terminal rendering trait
pub trait TerminalBuffer: Sized {
    /// Get the buffer of this size
    /// This has to guarantee the buffer is of this exact size
    fn get_buffer(&self, width: usize, height: usize) -> ColoredString;

    /// preferred width, from a given height
    fn preferred_width(&self, height: usize) -> Option<usize>;

    /// preferred height, from a given width
    fn preferred_height(&self, width: usize) -> Option<usize>;

    /// Compose a buffer to the other side
    fn left_of<'a, T: TerminalBuffer>(
        &'a self,
        other: &'a T,
        mode: SplitMode,
    ) -> VerticalPane<'a, Self, T> {
        VerticalPane {
            left: self,
            right: other,
            mode,
        }
    }

    /// Compose a buffer to the other side
    fn right_of<'a, T: TerminalBuffer>(
        &'a self,
        other: &'a T,
        mode: SplitMode,
    ) -> VerticalPane<'a, T, Self> {
        VerticalPane {
            left: other,
            right: self,
            mode: mode.reverse(),
        }
    }

    /// Compose a buffer to the other side
    fn top_of<'a, T: TerminalBuffer>(
        &'a self,
        other: &'a T,
        mode: SplitMode,
    ) -> HorizontalPane<'a, Self, T> {
        HorizontalPane {
            top: self,
            bottom: other,
            mode,
        }
    }

    /// Compose a buffer to the other side
    fn bottom_of<'a, T: TerminalBuffer>(
        &'a self,
        other: &'a T,
        mode: SplitMode,
    ) -> HorizontalPane<'a, T, Self> {
        HorizontalPane {
            top: other,
            bottom: self,
            mode: mode.reverse(),
        }
    }
}

/// mode on how to treat split layout
#[derive(Copy, Clone)]
pub enum SplitMode {
    /// Uses the preferred size of the left panel, or split halfway through the middle otherwise
    PreferLeft,

    /// Uses the preferred size of the right panel, or split halfway through the middle otherwise
    PreferRight,

    /// Show based on a split ratio, 0-1
    Ratio(f32),

    /// Guarantee the left side is at most this wide, when possible
    ExactLeft(usize),

    /// Guarantee the right side is at most this wide, when possible
    ExactRight(usize),
}

impl SplitMode {
    /// reverses the split mode, so you can swap the arguments
    pub fn reverse(self) -> Self {
        match self {
            Self::Ratio(x) => Self::Ratio(1.0 - x),
            Self::ExactLeft(x) => Self::ExactRight(x),
            Self::ExactRight(x) => Self::ExactLeft(x),
            Self::PreferLeft => Self::PreferRight,
            Self::PreferRight => Self::PreferLeft,
        }
    }

    /// returns the calculated split value
    pub fn get_split(
        self,
        total: usize,
        preferrred_left: Option<usize>,
        preferred_right: Option<usize>,
    ) -> usize {
        match self {
            SplitMode::Ratio(x) => ((total as f32 * x) as usize).min(total),
            SplitMode::ExactLeft(x) => x.min(total),
            SplitMode::ExactRight(x) => total.saturating_sub(x),
            SplitMode::PreferLeft => preferrred_left.unwrap_or(total / 2).min(total),
            SplitMode::PreferRight => preferred_right
                .map(|x| total.saturating_sub(x))
                .unwrap_or(total / 2)
                .min(total),
        }
    }
}

// TODO: mutable splits, as well as the ability to get the cursor position, run functions etc

impl<'a, L: TerminalBuffer, R: TerminalBuffer> TerminalBuffer for HorizontalPane<'a, L, R> {
    fn get_buffer(&self, width: usize, height: usize) -> ColoredString {
        // figure out the correct split
        let split = self.mode.get_split(
            height,
            self.top.preferred_height(width),
            self.bottom.preferred_height(width),
        );

        // get the buffers of the right size
        let top = self.top.get_buffer(width, split);
        let bottom = self.bottom.get_buffer(width, height - split);

        // and just stitch them together
        top.iter().chain(bottom.iter()).copied().collect()
    }

    fn preferred_width(&self, height: usize) -> Option<usize> {
        match self.mode {
            SplitMode::PreferLeft => self.top.preferred_width(height),
            SplitMode::PreferRight => self.bottom.preferred_width(height),
            _ => None,
        }
    }

    fn preferred_height(&self, width: usize) -> Option<usize> {
        match self.mode {
            // prefer the sum of both heights
            SplitMode::PreferLeft | SplitMode::PreferRight => self
                .top
                .preferred_height(width)
                .map(|x| self.bottom.preferred_height(width).map(|y| x.max(y)))
                .flatten(),
            _ => None,
        }
    }
}

impl<'a, L: TerminalBuffer, R: TerminalBuffer> TerminalBuffer for VerticalPane<'a, L, R> {
    fn get_buffer(&self, width: usize, height: usize) -> ColoredString {
        // figure out the correct split
        let split = self.mode.get_split(
            width,
            self.left.preferred_width(height),
            self.right.preferred_width(height),
        );

        // get the buffers
        let left = self.left.get_buffer(split, height);
        let right = self.right.get_buffer(width - split, height);

        let mut left_chars = left.iter();
        let mut right_chars = right.iter();

        // current column
        let mut column = 0;

        // string to generate
        let mut buffer = ColoredString::with_capacity(left.len() + right.len());

        // and add the lines together
        while let Some(character) = if column < split {
            left_chars.next()
        } else {
            right_chars.next()
        } {
            // add to the column
            column += string_width(std::iter::once(character.c), TERM_TAB_WIDTH);

            // wrap if it's on the next line
            if column >= width {
                column -= width;
            }

            // push it
            buffer.push(*character);
        }

        buffer
    }

    fn preferred_height(&self, width: usize) -> Option<usize> {
        match self.mode {
            SplitMode::PreferLeft => self.left.preferred_height(width),
            SplitMode::PreferRight => self.right.preferred_height(width),
            _ => None,
        }
    }

    fn preferred_width(&self, height: usize) -> Option<usize> {
        match self.mode {
            // prefer the sum of both widths
            SplitMode::PreferLeft | SplitMode::PreferRight => self
                .left
                .preferred_width(height)
                .map(|x| self.right.preferred_width(height).map(|y| x.max(y)))
                .flatten(),
            _ => None,
        }
    }
}

pub struct TextLine<'a> {
    string: &'a str,
}

impl<'a> TextLine<'a> {
    pub fn new(string: &'a str) -> Self {
        Self { string }
    }
}

impl<'a> TerminalBuffer for TextLine<'a> {
    fn get_buffer(&self, width: usize, height: usize) -> ColoredString {
        // simply add extra padding
        self.string
            .chars()
            .chain(std::iter::repeat(' '))
            .scan(0, |acc, x| {
                *acc += string_width(std::iter::once(x), TERM_TAB_WIDTH);
                if *acc <= width * height {
                    Some(x)
                } else {
                    None
                }
            })
            .map(|c| Char::new(c, Highlight::Status))
            .collect()
    }

    fn preferred_width(&self, _: usize) -> Option<usize> {
        // prefer to fit enough on here
        Some(string_width(self.string.chars(), TERM_TAB_WIDTH))
    }

    fn preferred_height(&self, _: usize) -> Option<usize> {
        // prefer to fit enough on here
        Some(1)
    }
}

impl TerminalBuffer for LineNumbers {
    fn get_buffer(&self, width: usize, height: usize) -> ColoredString {
        // buffer to add to
        let mut buffer = ColoredString::with_capacity(width * height);

        // figure out the max number padding
        let padding = self.width_number(height);

        // figure out the number of spaces to pad at the front
        let space_padding = width.saturating_sub(padding + 1).max(1);

        // lines to show, start and end
        let start = self.start + 1;
        let end = (self.start + 1 + height).min(self.total + 1);

        // add the line numbers
        for line in start..end {
            // current column
            let mut column = 0;

            // push starting space
            while column < width.min(space_padding) {
                buffer.push(Char::new(' ', Highlight::Gutter));
                column += 1;
            }

            // get the start number
            let mut base = (10 as usize).pow(padding.saturating_sub(1) as u32);

            // get the line number
            let line_number = if self.relative && line != self.current {
                // difference if we are not on the same line
                line.abs_diff(self.current)
            } else {
                line
            };

            // go as long as it's > 0, calculate the number to show
            while base > 0 && column < width {
                // see if we need to show a number
                if line_number / base > 0 {
                    // something, get the digit
                    let digit = (line_number / base) % 10;

                    // get it as char
                    let character = char::from_digit(digit as u32, 10).unwrap();

                    // push it
                    buffer.push(Char::new(character, Highlight::Gutter));
                } else {
                    // nothing, show a space
                    buffer.push(Char::new(' ', Highlight::Gutter));
                }

                // next base
                base /= 10;

                // next column
                column += 1;
            }

            // push remaining space, if any can fit
            if column < width {
                buffer.push(Char::new(' ', Highlight::Gutter));
            }
        }

        // and the rest
        buffer.extend(
            // generate the ~, over and over
            std::iter::repeat((0..width).map(|x| {
                if x == padding {
                    Char::new('~', Highlight::Gutter)
                } else {
                    Char::new(' ', Highlight::Gutter)
                }
            }))
            // for the remaining lines
            .take(height.saturating_sub(end - start))
            .flatten(),
        );

        // return
        buffer
    }

    // wide enough to fit our line numbers
    fn preferred_width(&self, height: usize) -> Option<usize> {
        Some(self.width(height))
    }

    // don't prefer
    fn preferred_height(&self, _: usize) -> Option<usize> {
        None
    }
}

impl TerminalBuffer for &TextEditor<TermLineLayoutSettings> {
    /// get the currently visible buffer, as a list of lines
    /// this is assuming a terminal editor, and should not be used when doing a gui editor
    fn get_buffer(&self, width: usize, height: usize) -> ColoredString {
        // all found text lines
        let mut buffer = ColoredString::with_capacity(width * height);

        // selection range, empty if none exists
        let selection_range = self.get_selection_range().unwrap_or(0..0);

        // go over all lines in the buffer
        for line_num in self.scroll_lines..self.scroll_lines + height {
            // current column
            let mut column = 0;

            // current cursor in the column
            let mut cursor = 0;

            // get the line
            if let Some(line) = self.text.get_line(line_num) {
                // where in the buffer the line is
                let line_start = self.text.line_to_byte(line_num);

                // go over all columns until we are either out of bounds or end of the line
                while cursor < line.len_bytes() && column < self.scroll_columns + width {
                    // get the next cursor pos
                    let next_cursor = move_grapheme(1, cursor, line);

                    // get the grapheme
                    let grapheme = line.byte_slice(cursor..next_cursor);

                    // stop if it's a newline
                    if grapheme.chars().any(|x| is_newline(x)) {
                        // if there's still room on the line, mark this as selected, if that's the case
                        if column >= self.scroll_columns
                            && column + 1 <= self.scroll_columns + width
                            && selection_range.contains(&(cursor + line_start))
                        {
                            // add to the buffer
                            buffer.push(Char::new(' ', Highlight::Selection));

                            // move over a column
                            column += 1;
                        }

                        // and stop, so the rest of the line is empty
                        break;
                    }

                    // get the grapheme width
                    let grapheme_width = string_width(grapheme.chars(), self.tab_width);

                    // if it doesn't fit in the buffer, pad spaces
                    if column < self.scroll_columns && column + grapheme_width > self.scroll_columns
                    {
                        buffer.extend(
                            std::iter::repeat(Char::new_text(' ', false))
                                .take(column + grapheme_width - self.scroll_columns),
                        );

                    // if we exceed the line, pad spaces instead
                    } else if column + grapheme_width > self.scroll_columns + width {
                        buffer.extend(
                            std::iter::repeat(Char::new_text(' ', false))
                                .take(self.scroll_columns + width - column),
                        );

                    // otherwise, add spaces if it's a tab
                    } else if column >= self.scroll_columns
                        && column + grapheme_width <= self.scroll_columns + width
                        && grapheme.chars().eq(std::iter::once('\t'))
                    {
                        buffer.extend(std::iter::repeat(' ').take(self.tab_width).map(|x| {
                            Char::new_text(x, selection_range.contains(&(cursor + line_start)))
                        }));

                    // otherwise, add characters
                    } else if column >= self.scroll_columns
                        && column + grapheme_width <= self.scroll_columns + width
                    {
                        buffer.extend(grapheme.chars().map(|x| {
                            Char::new_text(x, selection_range.contains(&(cursor + line_start)))
                        }));
                    }

                    // update
                    cursor = next_cursor;
                    column += grapheme_width;
                }
            }

            // final padding
            buffer.extend(std::iter::repeat(Char::new_text(' ', false)).take(
                (width + self.scroll_columns).saturating_sub(column.max(self.scroll_columns)),
            ));
        }

        buffer
    }

    // don't prefer
    fn preferred_width(&self, _: usize) -> Option<usize> {
        None
    }

    fn preferred_height(&self, _: usize) -> Option<usize> {
        None
    }
}

/// setup the terminal
pub fn setup_terminal() {
    // set panic hook
    std::panic::set_hook(Box::new(|info| {
        // clean up the terminal
        cleanup_terminal("Panic!");

        // pring panic info, if any
        if let Some(msg) = info.payload().downcast_ref::<&str>() {
            println!("Cause: {:?}", msg);
        }

        if let Some(loc) = info.location() {
            println!("Location: {}", loc);
        }
    }));

    // set raw mode
    terminal::enable_raw_mode().unwrap();

    execute!(
        stdout(),
        // so it can be restored
        cursor::SavePosition,
        // so it won't clutter other activities
        terminal::EnterAlternateScreen,
        // allow mouse usage
        EnableMouseCapture,
        // change cursor to a bar, as that's more clear
        cursor::SetCursorShape(CursorShape::Line),
    )
    .unwrap();
}

/// clean up the terminal
pub fn cleanup_terminal(message: &str) {
    execute!(
        stdout(),
        // go back to the normal screen
        terminal::LeaveAlternateScreen,
        // disable mouse
        DisableMouseCapture,
        // restore old cursor position
        cursor::RestorePosition,
        // restore cursor style
        cursor::SetCursorShape(CursorShape::Block),
        // restore visibility
        cursor::Show,
    )
    .unwrap();

    // leave raw mode
    terminal::disable_raw_mode().unwrap();

    println!("{}", message);
}

/// render the editor to a buffer
pub fn render_editor_to_buffer(
    editor: &TextEditor<TermLineLayoutSettings>,
    width: usize,
    height: usize,
    filename: &str,
    relative_line_numbers: bool,
) -> (ColoredString, Option<(usize, usize)>) {
    let lines = LineNumbers::new(
        editor.get_first_visible_line(),
        editor.len_lines(),
        editor.get_current_line() + 1,
        relative_line_numbers,
    );

    let status_line = format!(
        " {}{} {}:{}",
        filename,
        if editor.has_changed_since_save() {
            "*"
        } else {
            ""
        },
        editor.get_row_and_column().0 + 1,
        editor.get_row_and_column().1 + 1,
    );

    // cursor position
    let cursor_pos = editor
        .get_relative_cursor_pos()
        .map(|(x, y)| (x + lines.width(height), y));

    // and simply perform layout
    (
        lines
            .left_of(&editor, SplitMode::PreferLeft)
            .top_of(&TextLine::new(&status_line), SplitMode::PreferRight)
            .get_buffer(width, height),
        cursor_pos,
    )
}

/// render to the terminal, with differencing to not redraw the whole screen
pub fn render(
    width: usize,
    cursor_position: Option<(usize, usize)>,
    buffer: &[Char],
    previous_buffer: &[Char],
) {
    // current position in the buffer
    let mut x = 0;
    let mut y = 0;

    // position of the other item in the buffer
    let mut prev_x = 0;
    let mut prev_y = 0;

    // previous buffer chars
    let mut prev_chars = previous_buffer.iter().peekable();

    // whether to force move to the position
    let mut force_move = true;

    // previous colors
    let mut prev_fg = Color::Reset;
    let mut prev_bg = Color::Reset;

    // force reset the colors, ensure they are known
    queue!(
        stdout(),
        style::SetForegroundColor(Color::Reset),
        style::SetBackgroundColor(Color::Reset)
    )
    .unwrap();

    // and draw, char per char to ensure the correct cursor position
    for c in buffer.iter() {
        // don't draw if the positions are the same, and the character is as well
        if x != prev_x || y != prev_y || Some(&c) != prev_chars.peek() {
            // only need to force move if the previous character was not ascii, or we are on a new line
            if force_move {
                queue!(stdout(), cursor::MoveTo(x as u16, y as u16)).unwrap();
            }

            // get the new colors
            let fg = c.color.get_color_foreground_crossterm();
            let bg = c.color.get_color_background_crossterm();

            // change the color, if needed
            if fg != prev_fg {
                queue!(stdout(), style::SetForegroundColor(fg)).unwrap();
                prev_fg = fg;
            }

            if bg != prev_bg {
                queue!(stdout(), style::SetBackgroundColor(bg)).unwrap();
                prev_bg = bg;
            }

            // print as normal
            queue!(stdout(), style::Print(c.c)).unwrap();

            // we moved, so this can be false because our position is known
            // however, it may have changed if our character is not an ascii char (which has a known width)
            force_move = !c.c.is_ascii() || c.c.is_ascii_control();
        } else {
            // we skipped something, so our position is now unknown
            force_move = true;
        }

        // calculate the new cursor position
        x += string_width(std::iter::once(c.c), TERM_TAB_WIDTH);

        // if it's off to the side, reset
        if x >= width {
            y += 1;
            x = 0;

            // moved to a new line, so do this to be sure
            force_move = true;
        }

        // next up, see if we need to update the previous character
        while prev_x < x || prev_y < y {
            if let Some(c) = prev_chars.next() {
                // update the position
                prev_x += string_width(std::iter::once(c.c), TERM_TAB_WIDTH);

                // and wrap
                if prev_x >= width {
                    prev_y += 1;
                    prev_x = 0;
                }
            } else {
                // no characters, stop
                break;
            }
        }
    }

    // set cursor
    if let Some((x, y)) = cursor_position {
        queue!(stdout(), cursor::Show, cursor::MoveTo(x as u16, y as u16),).unwrap();
    } else {
        queue!(stdout(), cursor::Hide).unwrap();
    };

    // save changes
    stdout().flush().unwrap();
}

fn terminal_main(
    file_content: String,
    newly_loaded: bool,
    save_path: PathBuf,
    relative_line_numbers: bool,
    tab_width: usize,
) {
    // set up the terminal
    setup_terminal();

    // state
    let (mut width, mut height) = terminal::size().unwrap();

    // editor
    let mut editor = TextEditor::new(
        &file_content,
        TermLineLayoutSettings::new(tab_width),
        tab_width,
        newly_loaded,
    );

    // clipboard
    let mut clip = String::new();

    // system clipboard
    let mut system_clip = Clipboard::new().ok();

    // draw beforehand
    let (mut current_buffer, cursor_position) = render_editor_to_buffer(
        &editor,
        width as usize,
        height as usize,
        &save_path.to_string_lossy(),
        relative_line_numbers,
    );

    // and render to the terminal
    render(width as usize, cursor_position, &current_buffer, &[]);

    // event loop
    loop {
        if poll(std::time::Duration::from_millis(100)).unwrap() {
            // input
            match read().unwrap() {
                Event::Mouse(MouseEvent {
                    row,
                    column,
                    kind: MouseEventKind::Down(MouseButton::Left),
                    ..
                }) => {
                    // mouse move
                    editor.set_relative_cursor_pos(column as usize, row as usize, false);

                    // fix scrolling before rendering
                    editor.set_scroll(width as usize, height as usize, 6, 6);

                    // render
                    let (next_buffer, cursor_position) = render_editor_to_buffer(
                        &editor,
                        width as usize,
                        height as usize,
                        &save_path.to_string_lossy(),
                        relative_line_numbers,
                    );

                    // and render to the terminal
                    render(
                        width as usize,
                        cursor_position,
                        &next_buffer,
                        &current_buffer,
                    );

                    current_buffer = next_buffer;
                }
                Event::Key(KeyEvent {
                    code, modifiers, ..
                }) => {
                    // quit if needed
                    if code == KeyCode::Char('q') && modifiers == KeyModifiers::ALT {
                        break;
                    }

                    // saving
                    if code == KeyCode::Char('s') && modifiers == KeyModifiers::CONTROL {
                        let string = editor.to_string();

                        // ensure all folders exist, then make the file
                        if std::fs::create_dir_all(save_path.as_path().parent().unwrap()).is_ok()
                            && std::fs::write(save_path.as_path(), string).is_ok()
                        {
                            // indicate we saved, if succeeded
                            editor.set_saved();
                        }
                    }
                    // discard
                    else if code == KeyCode::Char('d') && modifiers == KeyModifiers::ALT {
                        editor.discard_changes();
                    }
                    // undo/redo
                    else if code == KeyCode::Char('z') && modifiers == KeyModifiers::CONTROL {
                        editor.undo();
                    } else if code == KeyCode::Char('y') && modifiers == KeyModifiers::CONTROL {
                        editor.redo();
                    }
                    // copy/paste/cut
                    else if code == KeyCode::Char('c') && modifiers == KeyModifiers::CONTROL {
                        // copy, if any
                        if let Some(x) = editor.get_selection() {
                            clip = x;
                        }
                    } else if code == KeyCode::Char('v') && modifiers == KeyModifiers::CONTROL {
                        // paste, if the clipboard is not empty
                        if clip.len() > 0 {
                            editor.insert_string_at_cursor(&clip);
                        }
                    } else if code == KeyCode::Char('x') && modifiers == KeyModifiers::CONTROL {
                        // cut, if the selection is any
                        if let Some(x) = editor.cut_selection() {
                            clip = x;
                        }
                    }
                    // system clipboard
                    else if code == KeyCode::Char('c') && modifiers == KeyModifiers::ALT {
                        // copy, if any
                        if let Some(x) = editor.get_selection() {
                            system_clip.as_mut().map(|y| y.set_text(x));
                        }
                    } else if code == KeyCode::Char('v') && modifiers == KeyModifiers::ALT {
                        // paste, if the clipboard is not empty
                        if let Some(x) = system_clip.as_mut() {
                            if let Ok(y) = x.get_text() {
                                if y.len() > 0 {
                                    editor.insert_string_at_cursor(&y);
                                }
                            }
                        }
                    } else if code == KeyCode::Char('x') && modifiers == KeyModifiers::ALT {
                        // cut, if the selection is any
                        if let Some(x) = editor.cut_selection() {
                            system_clip.as_mut().map(|y| y.set_text(x));
                        }
                    }
                    // move cursor
                    else if code == KeyCode::Up {
                        editor.move_cursor_vertical(-1, modifiers == KeyModifiers::SHIFT, false);
                    } else if code == KeyCode::Down {
                        editor.move_cursor_vertical(1, modifiers == KeyModifiers::SHIFT, false);
                    } else if code == KeyCode::Left && modifiers.contains(KeyModifiers::CONTROL) {
                        editor.move_cursor_horizontal_words(
                            -1,
                            modifiers.contains(KeyModifiers::SHIFT),
                            true,
                        );
                    } else if code == KeyCode::Right && modifiers.contains(KeyModifiers::CONTROL) {
                        editor.move_cursor_horizontal_words(
                            1,
                            modifiers.contains(KeyModifiers::SHIFT),
                            true,
                        );
                    } else if code == KeyCode::Left {
                        editor.move_cursor_horizontal(-1, modifiers == KeyModifiers::SHIFT, true);
                    } else if code == KeyCode::Right {
                        editor.move_cursor_horizontal(1, modifiers == KeyModifiers::SHIFT, true);
                    } else if code == KeyCode::Home {
                        editor.move_cursor_to_start_of_line(modifiers == KeyModifiers::SHIFT, true);
                    } else if code == KeyCode::End {
                        editor.move_cursor_to_end_of_line(modifiers == KeyModifiers::SHIFT, true);
                    }
                    // insert text
                    else if let KeyCode::Char(c) = code {
                        editor.insert_character_at_cursor(c);
                    } else if code == KeyCode::Enter {
                        editor.insert_newline_at_cursor();
                    } else if code == KeyCode::Tab {
                        editor.insert_tab_at_cursor();
                    }
                    // remove text
                    else if code == KeyCode::Backspace {
                        editor.remove_character_or_selection_at_cursor(true);
                    } else if code == KeyCode::Delete {
                        editor.remove_character_or_selection_at_cursor(false);
                    }

                    // fix scrolling before rendering
                    // TODO: fix in render step
                    editor.set_scroll(width as usize, height as usize, 6, 6);

                    // render
                    let (next_buffer, cursor_position) = render_editor_to_buffer(
                        &editor,
                        width as usize,
                        height as usize,
                        &save_path.to_string_lossy(),
                        relative_line_numbers,
                    );

                    // and render to the terminal
                    render(
                        width as usize,
                        cursor_position,
                        &next_buffer,
                        &current_buffer,
                    );

                    current_buffer = next_buffer;
                }

                Event::Resize(..) => {
                    width = terminal::size().unwrap().0;
                    height = terminal::size().unwrap().1;

                    // fix cursor pos
                    editor.set_scroll(width as usize, height as usize, 6, 6);

                    // render
                    let (next_buffer, cursor_position) = render_editor_to_buffer(
                        &editor,
                        width as usize,
                        height as usize,
                        &save_path.to_string_lossy(),
                        relative_line_numbers,
                    );

                    // and render to the terminal
                    render(width as usize, cursor_position, &next_buffer, &[]);

                    current_buffer = next_buffer;
                }
                _ => (),
            }
        } else {
            // do nothing!
            // at least here, if you are running an lsp or something else, you might want to check that
        }
    }

    // cleanup terminal
    cleanup_terminal("Done");
}

// arg parsing ======================================================

#[derive(Parser)]
struct Args {
    #[arg()]
    /// file to edit
    file_path: PathBuf,

    /// tab width
    #[arg(long, short, default_value_t = 4)]
    tab_width: usize,

    /// whether to use relative line numbers
    #[arg(long, short)]
    relative_line_numbers: bool,
}

// run ==============================================================

fn main() {
    // parse arguments
    let args = Args::parse();

    // get the file
    let (file_content, newly_loaded) = match std::fs::read_to_string(&args.file_path) {
        Ok(x) => (x, false),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => (String::new(), true),
        Err(e) => {
            println!("Failed to open file: {:?}", e);
            return;
        }
    };

    terminal_main(
        file_content,
        newly_loaded,
        args.file_path,
        args.relative_line_numbers,
        args.tab_width,
    );
}
