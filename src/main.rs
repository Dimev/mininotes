// terminal deps
use crossterm::{
    cursor,
    event::{
        poll, read, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent,
        KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
    },
    execute, queue, style,
    style::Color,
    terminal,
};
use swash::FontRef;

use std::ffi::OsString;
use std::io::{stdout, Write};
use std::ops::Range;

// editor deps
use ropey::{Rope, RopeSlice};
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete, UnicodeSegmentation};
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

// gui deps
use minifb::{Window, WindowOptions};

// arg parsing
use clap::Parser;

// unicode tools =============================================

// Ｈｅｌｌｏ, ｗｏｒｌｄ!
// اربك تكست هو اول موقع يسمح لزواره الكرام بتحويل الكتابة العربي الى كتابة مفهومة من قبل اغلب برامج التصميم
// 👩‍🔬👩🔬
// 🇷🇸🇮🇴

/// return the start byte index of the unicode grapheme if the cursor is advanced by amount
/// returns the length of the rope if it's too far
fn move_grapheme(amount: isize, mut byte_cursor: usize, text: RopeSlice) -> usize {
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

// TODO: force the rope to be 1 wide if it's not fully newlines
// this fixes terminal layout
fn string_width<I: IntoIterator<Item = char>>(iterator: I) -> usize {
    iterator
        .into_iter()
        .map(|x| x.width_cjk().unwrap_or(0))
        .sum()
}

// TODO: all newlines
fn is_newline(c: char) -> bool {
    c == '\n'
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
// TODO: bidir
pub struct TermLineLayoutSettings {}

impl LineLayout for TermLineLayoutSettings {
    type Iter<'a> = TermLineLayout<'a>;

    fn layout_line<'a>(&self, line: RopeSlice<'a>) -> TermLineLayout<'a> {
        TermLineLayout::new(line)
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
}

impl<'a> TermLineLayout<'a> {
    pub fn new(line: RopeSlice<'a>) -> Self {
        Self {
            line,
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
            let grapheme_width = string_width(rope_slice.chars());

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

    /// selection, byte range
    selection: Option<Range<usize>>,
}

impl<L: LineLayout> TextEditor<L> {
    pub fn new(content: &str, layout_settings: L) -> Self {
        Self {
            text: Rope::from_str(content),
            cursor: 0,
            target_column: 0,
            scroll_lines: 0,
            scroll_columns: 0,
            layout_settings,
            selection: None,
        }
    }

    /// move cursor horizontally, and save the cursor column if needed
    pub fn move_cursor_horizontal(&mut self, amount: isize, save_column: bool) {
        // just move it
        self.cursor = move_grapheme(amount, self.cursor, self.text.slice(..));

        // and recalculate the target column
        if save_column {
            self.target_column = self.get_cursor_column();
        }
    }

    /// move cursor vertically, and save the column if needed
    pub fn move_cursor_vertical(&mut self, amount: isize, save_column: bool) {
        // find where the next line is
        let next_line = (self.text.byte_to_line(self.cursor) as isize + amount)
            .max(0)
            .min(self.text.len_lines().saturating_sub(1) as isize) as usize;

        // find where it starts and move there
        self.cursor = self.text.line_to_byte(next_line);

        // move horizontally until we are at the target
        self.move_cursor_to_column(self.target_column, save_column);
    }

    /// insert a character
    pub fn insert_character(&mut self, character: char) {
        // insert
        self.text
            .insert_char(self.text.byte_to_char(self.cursor), character);

        // move the cursor over
        self.move_cursor_horizontal(1, true);
    }

    /// insert a string
    pub fn insert_string(&mut self, string: &str) {
        // insert
        self.text
            .insert(self.text.byte_to_char(self.cursor), string);

        // move the cursor over
        self.move_cursor_horizontal(string.graphemes(true).count() as isize, true);
    }

    /// insert a newline
    /// also inserts all spaces preceeding the current line, up to the cursor position
    pub fn insert_newline(&mut self) {
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
            .take(line_char_pos - line_char_start)
            .collect::<String>();

        // insert a newline
        self.insert_character('\n');

        // append the extra whitespaces
        self.insert_string(&pred_whitespace);
    }

    /// remove a character at the cursor.
    /// before means remove it before the cursor, same as backspace
    pub fn remove_character(&mut self, before: bool) {
        if before {
            // end of the character to remove
            let end = self.text.byte_to_char(self.cursor);

            // move back
            self.move_cursor_horizontal(-1, true);

            // we are now at the start
            let start = self.text.byte_to_char(self.cursor);

            // remove it
            self.text.remove(start..end);
        } else {
            // start of the character to remove
            let start_byte = self.cursor;
            let start = self.text.byte_to_char(start_byte);

            // move forward
            self.move_cursor_horizontal(1, true);

            // we are now at the end
            let end = self.text.byte_to_char(self.cursor);

            // restore position
            self.cursor = start_byte;

            // remove the next character
            self.text.remove(start..end);
        }
    }

    /// move the cursor to a specific column, assuming a terminal program
    pub fn move_cursor_to_column(&mut self, column: usize, save_column: bool) {
        // move the cursor to the start of the line
        self.move_cursor_to_start_of_line(save_column);

        // get the current line
        let line = self.text.line(self.text.byte_to_line(self.cursor));

        // layout the line, and find the right grapheme that contains our column, or at least the one closest to it
        let cursor_pos = self
            .layout_settings
            .layout_line(line)
            .find(|x| (x.start_column..x.end_column).contains(&column))
            .map(|x| self.cursor + x.cursor);

        // need to do this here, otherwise the borrowchecker can't see we don't need to borrow self in line when doing move_cursor
        if let Some(cursor) = cursor_pos {
            self.cursor = cursor;
        } else {
            // otherwise, move it to the end of the line, as that would be expected
            self.move_cursor_to_end_of_line(save_column);
        }
    }

    /// move the cursor to the start of the line
    pub fn move_cursor_to_start_of_line(&mut self, save_column: bool) {
        self.cursor = self.text.line_to_byte(self.text.byte_to_line(self.cursor));
        if save_column {
            self.target_column = 0;
        }
    }

    /// move the cursor to the end of the line
    pub fn move_cursor_to_end_of_line(&mut self, save_column: bool) {
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
                self.move_cursor_horizontal(-1, save_column);
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
    pub fn set_cursor_pos(&mut self, x: usize, y: usize) {
        // find the line
        let line = y.min(self.text.len_lines());

        // find where the line start
        let line_start = self.text.line_to_byte(line);

        // move cursor to the right line
        self.cursor = line_start;

        // and move the cursor to the right column
        self.move_cursor_to_column(x, true);
    }

    /// set the cursor pos, relative to scrolling
    pub fn set_relative_cursor_pos(&mut self, x: usize, y: usize) {
        // set with the absolute coords
        self.set_cursor_pos(x + self.scroll_columns, y + self.scroll_lines);
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
}

// line numbers ==========================================

// TODO: relative line numbers
/// line numbers to show
#[derive(Copy, Clone)]
pub struct LineNumbers {
    /// start of the range
    start: usize,

    /// total number of lines
    total: usize,
}

impl LineNumbers {
    /// get how wide this should be
    fn width(self) -> usize {
        format!(" {} ", self.total).len()
    }

    /// how wide the number should be
    fn width_number(self) -> usize {
        format!("{}", self.total).len()
    }
}

// terminal ==============================================

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
    fn get_color_foreground_crossterm(self) -> Color {
        match self {
            Self::Text => Color::Reset,
            Self::Selection => Color::White,
            Self::Gutter => Color::Yellow,
            Self::Status => Color::Black,
        }
    }

    /// returns the background color
    fn get_color_background_crossterm(self) -> Color {
        match self {
            Self::Text => Color::Reset,
            Self::Selection => Color::Grey,
            Self::Gutter => Color::Reset,
            Self::Status => Color::White,
        }
    }
}

// TODO: syntax highlighting here

// ui layout/terminal drawing ===========================
// TODO: move this around a bit

// TODO: better layout functionality
// this should also work when using the gui
// having a way to capture events here would also be nice
// besides get_buffer, there should also be handle input, get glyphs, etc
// as well as size hints

/// terminal rendering trait
trait TerminalBuffer: Sized {
    /// Get the buffer of this size
    /// This has to guarantee the buffer is of this exact size
    fn get_buffer(&self, width: usize, height: usize) -> String;

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
enum SplitMode {
    // TODO: inferred left and right
    // this uses the preferred with and height, if any, otherwise split by half
    // TODO: left or right only, to (not) show things when not needed
    /// Show based on a split ratio, 0-1
    Ratio(f32),

    /// Guarantee the left side is at most this wide, when possible
    ExactLeft(usize),

    /// Guarantee the right side is at most this wide, when possible
    ExactRight(usize),
}

impl SplitMode {
    fn reverse(self) -> Self {
        match self {
            Self::Ratio(x) => Self::Ratio(1.0 - x),
            Self::ExactLeft(x) => Self::ExactRight(x),
            Self::ExactRight(x) => Self::ExactLeft(x),
        }
    }
}

// TODO: mutable splits, as well as the ability to get the cursor position, run functions etc

/// pane split top and bottom
#[derive(Copy, Clone)]
struct HorizontalPane<'a, L: TerminalBuffer, R: TerminalBuffer> {
    top: &'a L,
    bottom: &'a R,
    mode: SplitMode,
}

impl<'a, L: TerminalBuffer, R: TerminalBuffer> TerminalBuffer for HorizontalPane<'a, L, R> {
    fn get_buffer(&self, width: usize, height: usize) -> String {
        // figure out the correct split
        let split = match self.mode {
            SplitMode::Ratio(x) => (height as f32 * x) as usize,
            SplitMode::ExactLeft(x) => x.min(height),
            SplitMode::ExactRight(x) => height.saturating_sub(x),
        };

        // get the buffers of the right size
        let top = self.top.get_buffer(width, split);
        let bottom = self.bottom.get_buffer(width, height - split);

        // and just stitch them together
        top.chars().chain(bottom.chars()).collect()
    }
}

/// pane split left and right
#[derive(Copy, Clone)]
struct VerticalPane<'a, L: TerminalBuffer, R: TerminalBuffer> {
    left: &'a L,
    right: &'a R,
    mode: SplitMode,
}

impl<'a, L: TerminalBuffer, R: TerminalBuffer> TerminalBuffer for VerticalPane<'a, L, R> {
    fn get_buffer(&self, width: usize, height: usize) -> String {
        // figure out the correct split
        let split = match self.mode {
            SplitMode::Ratio(x) => (width as f32 * x) as usize,
            SplitMode::ExactLeft(x) => x.min(width),
            SplitMode::ExactRight(x) => width.saturating_sub(x),
        };

        // get the buffers
        let left = self.left.get_buffer(split, height);
        let right = self.right.get_buffer(width - split, height);

        let mut left_graphemes = left.graphemes(true);
        let mut right_graphemes = right.graphemes(true);

        // current column
        let mut column = 0;

        // string to generate
        let mut buffer = String::with_capacity(left.len() + right.len());

        // and add the lines together
        while let Some(grapheme) = if column < split {
            left_graphemes.next()
        } else {
            right_graphemes.next()
        } {
            // add to the column
            column += grapheme.width_cjk();

            // wrap if it's on the next line
            if column >= width {
                column -= width;
            }

            // push it
            buffer.push_str(grapheme);
        }

        buffer
    }
}

struct TextLine<'a> {
    string: &'a str,
}

impl<'a> TextLine<'a> {
    fn new(string: &'a str) -> Self {
        Self { string }
    }
}

impl<'a> TerminalBuffer for TextLine<'a> {
    fn get_buffer(&self, width: usize, height: usize) -> String {
        // simply add extra padding
        // TODO: wrapping?
        self.string
            .chars()
            .chain(std::iter::repeat(' '))
            .scan(0, |acc, x| {
                *acc += x.width_cjk().unwrap_or(0);
                if *acc <= width * height {
                    Some(x)
                } else {
                    None
                }
            })
            .collect()
    }
}

impl TerminalBuffer for LineNumbers {
    fn get_buffer(&self, width: usize, height: usize) -> String {
        // TODO: width/height guarantees
        assert!(width >= self.width(), "TODO: uphold width guarantee");

        // get the total width of the number
        let number_padding = self.width_number();

        // iterator over the numbered lines
        let numbered =
            (self.start + 1..self.total + 1).map(|x| format!(" {:>1$} ", x, number_padding));

        // iterator over the rest of the lines
        let rest = std::iter::repeat(format!(" {:>1$} ", "~", number_padding));

        // collect
        numbered.chain(rest).take(height).collect()
    }
}

impl TerminalBuffer for &TextEditor<TermLineLayoutSettings> {
    /// get the currently visible buffer, as a list of lines
    /// this is assuming a terminal editor, and should not be used when doing a gui editor
    fn get_buffer(&self, width: usize, height: usize) -> String {
        // all found text lines
        let mut buffer = String::with_capacity(width * height);

        // go over all lines in the buffer
        for line_num in self.scroll_lines..self.scroll_lines + height {
            // current column
            let mut column = 0;

            // current cursor in the column
            let mut cursor = 0;

            // get the line
            if let Some(line) = self.text.get_line(line_num) {
                // go over all columns until we are either out of bounds or end of the line
                while cursor < line.len_bytes() && column < self.scroll_columns + width {
                    // get the next cursor pos
                    let next_cursor = move_grapheme(1, cursor, line);

                    // get the grapheme
                    let grapheme = line.byte_slice(cursor..next_cursor);

                    // stop if it's a newline
                    if grapheme.chars().any(|x| is_newline(x)) {
                        break;
                    }

                    // get the grapheme width
                    let grapheme_width = string_width(grapheme.chars());

                    // if it doesn't fit in the buffer, pad spaces
                    if column < self.scroll_columns && column + grapheme_width > self.scroll_columns
                    {
                        buffer.extend(
                            std::iter::repeat(' ')
                                .take(column + grapheme_width - self.scroll_columns),
                        );

                    // if we exceed the line, pad spaces instead
                    } else if column + grapheme_width > self.scroll_columns + width {
                        buffer.extend(
                            std::iter::repeat(' ').take(self.scroll_columns + width - column),
                        );

                    // otherwise, add our characters
                    } else if column >= self.scroll_columns
                        && column + grapheme_width <= self.scroll_columns + width
                    {
                        buffer.extend(grapheme.chars());
                    }

                    // update
                    cursor = next_cursor;
                    column += grapheme_width;
                }
            }

            // final padding
            buffer.extend(std::iter::repeat(' ').take(
                (width + self.scroll_columns).saturating_sub(column.max(self.scroll_columns)),
            ));
        }

        buffer
    }
}

/// render the editor to a buffer
fn render_editor_to_buffer(
    editor: &TextEditor<TermLineLayoutSettings>,
    width: usize,
    height: usize,
    filename: &str,
) -> (String, Option<(usize, usize)>) {
    let lines = LineNumbers {
        start: editor.get_first_visible_line(),
        total: editor.len_lines(),
    };

    let status_line = format!(
        " {} {}:{}",
        filename,
        editor.get_row_and_column().0 + 1,
        editor.get_row_and_column().1 + 1
    );

    // cursor position
    let cursor_pos = editor
        .get_relative_cursor_pos()
        .map(|(x, y)| (x + lines.width(), y));

    // and simply perform layout
    (
        lines
            .left_of(&editor, SplitMode::ExactLeft(lines.width()))
            .top_of(&TextLine::new(&status_line), SplitMode::ExactRight(1))
            .get_buffer(width, height),
        cursor_pos,
    )
}

/// render to the terminal, with differencing to not redraw the whole screen
fn render(
    width: usize,
    cursor_position: Option<(usize, usize)>,
    buffer: &str,
    previous_buffer: &str,
) {
    // current position in the buffer
    let mut x = 0;
    let mut y = 0;

    // position of the other item in the buffer
    let mut prev_x = 0;
    let mut prev_y = 0;

    // previous buffer chars
    let mut prev_chars = previous_buffer.chars().peekable();

    // whether to force move to the position
    let mut force_move = true;

    // and draw, char per char to ensure the correct cursor position
    for c in buffer.chars() {
        // don't draw if the positions are the same, and the character is as well
        if x != prev_x || y != prev_y || Some(&c) != prev_chars.peek() {
            // only need to force move if the previous character was not ascii, or we are on a new line
            if force_move {
                queue!(stdout(), cursor::MoveTo(x as u16, y as u16)).unwrap();
            }

            // print as normal
            queue!(stdout(), style::Print(c)).unwrap();

            // we moved, so this can be false because our position is known
            // however, it may have changed if our character is not an ascii char (which has a known width)
            force_move = !c.is_ascii() || c.is_ascii_control();
        } else {
            // we skipped something, so our position is now unknown
            force_move = true;
        }

        // calculate the new cursor position
        x += string_width(std::iter::once(c));

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
                prev_x += string_width(std::iter::once(c));

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

fn terminal_main(file_path: OsString) {
    // get the file
    let Ok(file_content) = std::fs::read_to_string(&file_path) else {
        println!("Failed to open file");
        return;
    };

    // set panic hook
    std::panic::set_hook(Box::new(|info| {
        // return to normal mode
        execute!(
            stdout(),
            terminal::LeaveAlternateScreen,
            style::ResetColor,
            cursor::MoveTo(0, terminal::size().unwrap().1),
            DisableMouseCapture,
            cursor::RestorePosition
        )
        .unwrap();
        terminal::disable_raw_mode().unwrap();

        // panic message
        println!("panic!");

        if let Some(msg) = info.payload().downcast_ref::<&str>() {
            println!("Cause: {:?}", msg);
        }

        if let Some(loc) = info.location() {
            println!("Location: {}", loc);
        }
    }));

    // set mode
    terminal::enable_raw_mode().unwrap();

    execute!(
        stdout(),
        cursor::SavePosition,
        terminal::EnterAlternateScreen,
        terminal::Clear(terminal::ClearType::Purge),
        cursor::MoveTo(0, 0),
        EnableMouseCapture,
    )
    .unwrap();

    // state
    let (mut width, mut height) = terminal::size().unwrap();

    // editor
    let mut editor = TextEditor::new(&file_content, TermLineLayoutSettings {});

    // draw beforehand
    let (mut current_buffer, cursor_position) = render_editor_to_buffer(
        &editor,
        width as usize,
        height as usize,
        &file_path.to_string_lossy(),
    );

    // and render to the terminal
    render(width as usize, cursor_position, &current_buffer, "");

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
                    editor.set_relative_cursor_pos(column as usize, row as usize);

                    // fix scrolling before rendering
                    editor.set_scroll(width as usize, height as usize, 6, 6);

                    // render
                    let (next_buffer, cursor_position) = render_editor_to_buffer(
                        &editor,
                        width as usize,
                        height as usize,
                        &file_path.to_string_lossy(),
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

                    // move cursor
                    if code == KeyCode::Up {
                        editor.move_cursor_vertical(-1, false);
                    } else if code == KeyCode::Down {
                        editor.move_cursor_vertical(1, false);
                    } else if code == KeyCode::Left {
                        editor.move_cursor_horizontal(-1, true);
                    } else if code == KeyCode::Right {
                        editor.move_cursor_horizontal(1, true);
                    } else if code == KeyCode::Home {
                        editor.move_cursor_to_start_of_line(true);
                    } else if code == KeyCode::End {
                        editor.move_cursor_to_end_of_line(true);
                    }
                    // insert text
                    else if let KeyCode::Char(c) = code {
                        editor.insert_character(c);
                    } else if code == KeyCode::Enter {
                        editor.insert_newline();
                    }
                    // remove text
                    else if code == KeyCode::Backspace {
                        editor.remove_character(true);
                    } else if code == KeyCode::Delete {
                        editor.remove_character(false);
                    }

                    // fix scrolling before rendering
                    // TODO: fix in render step
                    editor.set_scroll(width as usize, height as usize, 6, 6);

                    // render
                    let (next_buffer, cursor_position) = render_editor_to_buffer(
                        &editor,
                        width as usize,
                        height as usize,
                        &file_path.to_string_lossy(),
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
                    let (current_buffer, cursor_position) = render_editor_to_buffer(
                        &editor,
                        width as usize,
                        height as usize,
                        &file_path.to_string_lossy(),
                    );

                    // and render to the terminal
                    render(width as usize, cursor_position, &current_buffer, "");
                }
                _ => (),
            }
        } else {
            // do nothing!
            // at least here, if you are running an lsp or something else, you might want to check that
        }
    }

    // reset mode
    execute!(
        stdout(),
        terminal::LeaveAlternateScreen,
        cursor::MoveTo(0, terminal::size().unwrap().1),
        DisableMouseCapture,
        cursor::RestorePosition,
    )
    .unwrap();
    terminal::disable_raw_mode().unwrap();

    println!("Done");
}

// gui ===============================================================
fn gui_main(file_path: OsString) {
    // get the file
    let Ok(file_content) = std::fs::read_to_string(&file_path) else {
        println!("Failed to open file");
        return;
    };

    // make a font
    let font_data = include_bytes!("fira-code.ttf");
    let font = FontRef::from_index(font_data, 0).unwrap();

    // make the font shaper

    // make the font scaler

    // make the font layout settings

    // make the editor
    let mut editor = TextEditor::new(&file_content, TermLineLayoutSettings {});

    // start the  window
    let mut window = Window::new(
        "mininotes",
        800,
        450,
        WindowOptions {
            borderless: false,
            resize: true,
            ..Default::default()
        },
    )
    .unwrap();

    while window.is_open() {
        // update editor

        // get all glyphs that need to be laid out

        // draw all glyphs to the right position

        // and update

        // update!
        window.update();
    }
}

// arg parsing ======================================================

#[derive(Parser)]
struct Args {
    #[arg()]
    /// file to edit
    file_path: OsString,

    /// whether to run in gui mode
    #[arg(long, short)]
    gui: bool,
}

// run ==============================================================

fn main() {
    // parse arguments
    let args = Args::parse();

    if args.gui {
        gui_main(args.file_path)
    } else {
        terminal_main(args.file_path)
    };
}
