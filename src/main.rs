// terminal deps
use crossterm::{
    cursor,
    event::{
        poll, read, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent,
        KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
    },
    execute, queue, style, terminal,
};

use std::io::{stdout, Write};

// editor deps
use ropey::{Rope, RopeSlice};
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete, UnicodeSegmentation};
use unicode_width::{UnicodeWidthChar, UnicodeWidthStr};

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
// TODO: string width and char width
fn rope_width(rope: RopeSlice) -> usize {
    rope.chars().map(|x| x.width_cjk().unwrap_or(0)).sum()
}

// TODO: all newlines
fn is_newline(c: char) -> bool {
    c == '\n'
}

// terminal layout ===========================================

/// grapheme information
pub struct GraphemePosition {
    /// where it starts
    pub start_column: usize,

    /// where it ends
    pub end_column: usize,

    /// and where the cursor is in the string, in bytes
    pub cursor: usize,
}

/// iterator state
pub struct TermLineLayout<'a> {
    // TODO: generic text layout algo
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
            let grapheme_width = rope_width(rope_slice);

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

// TODO: helix style grapheme width to fix it breaking on missing font terminals -> seems to just force the cursor to move to the right position

// editor ====================================================

/// multiline text editor
// TODO: const generics (?) to also make it work as a single line editor
// this disables the multi-line editing features, and fails to create when the input text has newlines in it
// TODO: seperate scroll state
// TODO: correct generic line layout iterator
pub struct TextEditor {
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
}

impl TextEditor {
    pub fn new(content: &str) -> Self {
        Self {
            text: Rope::from_str(content),
            cursor: 0,
            target_column: 0,
            scroll_lines: 0,
            scroll_columns: 0,
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
        if let Some(cursor) = TermLineLayout::new(line)
            .find(|x| (x.start_column..x.end_column).contains(&column))
            .map(|x| self.cursor + x.cursor)
        {
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
        TermLineLayout::new(line)
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

// TODO: better layout functionality
/// terminal rendering trait
trait TerminalBuffer {
    fn get_buffer(&self, width: usize, height: usize) -> String;

    /// compose the buffer vertically, top and bottom
    fn compose_vertical<T: TerminalBuffer>(
        &self,
        other: T,
        width: usize,
        height: usize,
        split: usize,
    ) -> String {
        let top = self.get_buffer(width, split);
        let bottom = other.get_buffer(width, height - split);
        top.chars().chain(bottom.chars()).collect()
    }

    /// compose the buffer horizontally, left and right
    fn compose_horizontal<T: TerminalBuffer>(
        &self,
        other: T,
        width: usize,
        height: usize,
        split: usize,
    ) -> String {
        let left = self.get_buffer(split, height);
        let right = other.get_buffer(width - split, height);

        let mut left_graphemes = left.graphemes(true);
        let mut right_graphemes = right.graphemes(true);

        // current column
        let mut column = 0;

        // string to generate
        // TODO: don't keep reallocating
        let mut buffer = String::with_capacity(left.len() + right.len());

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

        // combine
        // bit harder than vertical, as it's not possible to simply stitch them together
        buffer
    }
}

impl TerminalBuffer for String {
    fn get_buffer(&self, width: usize, height: usize) -> String {
        // simply add extra padding
        // TODO: imperative + proper graphemes
        self.chars()
            .scan(0, |acc, x| {
                *acc += x.width_cjk().unwrap_or(0);
                if *acc < width * height {
                    Some(x)
                } else {
                    None
                }
            })
            .chain(std::iter::repeat(' ').take((width * height).saturating_sub(self.width_cjk())))
            .collect()
    }
}

impl TerminalBuffer for LineNumbers {
    fn get_buffer(&self, _: usize, height: usize) -> String {
        // get the total width of the number
        let number_padding = self.width_number();

        // iterator over the numbered lines
        let numbered = (self.start + 1..self.total + 1).map(|x| format!(" {:>1$} ", x, number_padding));

        // iterator over the rest of the lines
        let rest = std::iter::repeat(format!(" {:>1$} ", "~", number_padding));

        // collect
        numbered.chain(rest).take(height).collect()
    }
}

impl TerminalBuffer for &TextEditor {
    /// get the currently visible buffer, as a list of lines
    /// this is assuming a terminal editor, and should not be used when doing a gui editor
    // TODO: compose: this uses  the calculated char position to select from either string
    fn get_buffer(&self, width: usize, height: usize) -> String {
        // all found text lines
        let mut buffer = String::new();

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
                    let grapheme_width = rope_width(grapheme);

                    // if it doesn't fit in the buffer, pad spaces
                    if column < self.scroll_columns
                        && column + grapheme_width >= self.scroll_columns
                    {
                        buffer.extend(
                            std::iter::repeat(' ')
                                .take(column + grapheme_width - self.scroll_columns),
                        );

                    // if we exceed the line, pad spaces instead
                    } else if column + grapheme_width >= self.scroll_columns + width {
                        buffer.extend(
                            std::iter::repeat(' ').take(self.scroll_columns + width - column),
                        );

                    // otherwise, add our characters
                    } else if column >= self.scroll_columns
                        && column + grapheme_width < self.scroll_columns + width
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

// TODO: horizontal and vertical compose, so multiple buffers can be next to eachother
// keeps track of the width of graphemes
// if one goes beyond the width, switch to the other iterator
// should be simpler due to dealing with strings, and not iterators, although less efficient

/// render to the terminal, with differencing to not redraw the whole screen
fn render(editor: &TextEditor, width: usize, height: usize, filename: &str) {
    let lines = LineNumbers {
        start: editor.get_first_visible_line(),
        total: editor.len_lines(),
    };

    let buffer = lines
        .compose_horizontal(editor, width, height.saturating_sub(1), lines.width())
        .compose_vertical(
            format!(
                " {} {}:{}",
                filename,
                editor.get_row_and_column().0 + 1,
                editor.get_row_and_column().1 + 1
            ),
            width,
            height,
            height.saturating_sub(1),
        );

    // print editor
    queue!(stdout(), cursor::MoveTo(0, 0), style::Print(buffer),).unwrap();

    // TODO: diff
    // check the graphemes of both
    // if one grapheme is different than the other, force emit set cursor pos and then the difference
    // different means their positions are different or their content is different

    // iterate over all characters and keep track of their column and row
    // if the column exceeds the width, go to the next row
    // then zip with the previous buffer and filter out the ones that changed
    // then do this
    /*let mut diff = Vec::new();

        // previous x value
        let mut prev_x = 0;
        let mut prev_y = 0;

        // get the changes
        let changes = self.get_changed_cells();

        // actually move to 0, 0, if there's any changes
        if changes.len() > 0 {
            diff.push(CharOrJump::Jump(0, 0));
        }

        // go over all items, loop wise
        for (x, y, change) in changes {
            // if we're not where the pointer would have advanced to
            // move to where we should be
            if prev_x + 1 != x || prev_y != y || prev_x == 0 {
                diff.push(CharOrJump::Jump(x, y));
            }

            // emit the change
            diff.push(CharOrJump::Char(change));

            // remember x
            prev_x = x;
            prev_y = y;
        }

        diff
    */

    // set cursor
    if let Some((x, y)) = editor.get_relative_cursor_pos() {
        queue!(
            stdout(),
            cursor::Show,
            cursor::MoveTo((x + lines.width()) as u16, y as u16)
        )
        .unwrap();
    } else {
        queue!(stdout(), cursor::Hide).unwrap();
    };

    // save changes
    stdout().flush().unwrap();
}

fn terminal_main() {
    // get the file
    let Some(file_path) = std::env::args_os().skip(1).next() else {
        println!("Usage: mininotes <file>");
        return;
    };

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
    let mut editor = TextEditor::new(&file_content);

    // draw beforehand
    render(
        &editor,
        width as usize,
        height as usize,
        &file_path.to_string_lossy(),
    );

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
                    render(
                        &editor,
                        width as usize,
                        height as usize,
                        &file_path.to_string_lossy(),
                    );
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
                    editor.set_scroll(width as usize, height as usize, 6, 6);

                    // render
                    render(
                        &editor,
                        width as usize,
                        height as usize,
                        &file_path.to_string_lossy(),
                    );
                }

                Event::Resize(..) => {
                    width = terminal::size().unwrap().0;
                    height = terminal::size().unwrap().1;

                    // fix cursor pos
                    editor.set_scroll(width as usize, height as usize, 6, 6);

                    // render
                    render(
                        &editor,
                        width as usize,
                        height as usize,
                        &file_path.to_string_lossy(),
                    );
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
fn gui_main() {
    todo!()
}

fn main() {
    // TODO: cmd args

    terminal_main();
}
