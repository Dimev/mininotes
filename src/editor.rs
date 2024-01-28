//! text editor

use crate::unicode::{is_newline, move_grapheme};
use ropey::{Rope, RopeSlice};
use std::collections::VecDeque;
use std::ops::Range;
use unicode_width::UnicodeWidthChar;

/// line layout, in order to lay out a single line
pub trait LineLayout {
    type Iter<'a>: Iterator<Item = GraphemePosition>;

    /// Layout a line
    fn layout_line<'a>(&self, line: RopeSlice<'a>) -> Self::Iter<'a>;
}

/// grapheme information, after layout
pub struct GraphemePosition {
    /// where it starts
    pub start_column: usize,

    /// where it ends
    pub end_column: usize,

    /// and where the cursor is in the string, in bytes
    pub cursor: usize,
}

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

    /// get the entire text
    pub fn get_text(&self) -> RopeSlice {
        self.text.slice(..)
    }

    /// get the number of lines scrolled
    pub fn get_lines_scrolled(&self) -> usize {
        self.scroll_lines
    }

    /// get the number of columns scrolled
    pub fn get_columns_scrolled(&self) -> usize {
        self.scroll_columns
    }

    /// get the tab width
    pub fn get_tab_width(&self) -> usize {
        self.tab_width
    }

    /// convert the contents of the editor to a string
    pub fn to_string(&self) -> String {
        self.text.to_string()
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
        let mut buffer = [0_u8; 4];
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
        if record && !string.is_empty() {
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
        if record && !string.is_empty() {
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
        // manage selection
        if add_selection {
            self.add_selection();
        } else {
            self.clear_selection();
        }

        // find the line
        let line = y.min(self.text.len_lines());

        // if it's not the last line, actually move it
        if line != self.text.len_lines() {
            // find where the line start
            let line_start = self.text.line_to_byte(line);

            // layout the line, and find the right grapheme that contains our column, or at least the one closest to it
            let cursor_pos = self
                .layout_settings
                .layout_line(self.text.line(line))
                .take_while(|y| y.start_column <= x)
                .last()
                .map(|x| line_start + x.cursor);

            // need to do this here, otherwise the borrowchecker can't see we don't need to borrow self in line when doing move_cursor
            if let Some(cursor) = cursor_pos {
                self.cursor = cursor;
            } else {
                // otherwise, move it to the end of the line, as that would be expected
                self.move_cursor_to_end_of_line(add_selection, true);
            }
        } else {
            // else, move to the end of the text
            self.cursor = self.text.len_bytes();
        }
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

    /// scroll the visible window by some amount, and ensure that visually the cursor remains on the same line
    pub fn scroll_vertically(&mut self, amount: isize) {
        // scroll the window
        self.scroll_lines = self.scroll_lines.saturating_add_signed(amount);

        // move the cursor
        self.move_cursor_vertical(amount, false, false);
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
}
