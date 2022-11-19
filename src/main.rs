// terminal deps
use crossterm::{
    cursor,
    event::{poll, read, Event, KeyCode, KeyEvent, KeyModifiers},
    execute, queue, style, terminal,
};

use std::io::{stdout, Write};

// editor deps
use ropey::{Rope, RopeSlice};
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete, UnicodeSegmentation};
use unicode_width::UnicodeWidthChar;

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
        // TODO: stop if we are at a newline if wrap is disabled

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

fn rope_width(rope: RopeSlice) -> usize {
    rope.chars().map(|x| x.width_cjk().unwrap_or(0)).sum()
}

fn is_newline(c: char) -> bool {
    c == '\n'
}

// TODO: grapheme width
// TODO: full string kerning and the layout stuff to make it work on gui
// TODO: helix style grapheme width to fix it breaking on missing font terminals

// editor ====================================================

/// multiline text editor
pub struct TextEditor {
    /// text
    text: Rope,

    /// cursor position, in bytes
    cursor: usize,

    /// current column the cursor is on
    column: usize,

    /// current row the cursor is on
    row: usize,

    /// cursor target column
    target_column: usize,
    
    /// cursor position and it's calculated column, if known
    current_column: Option<(usize, usize)>,

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
            column: 0,
            row: 0,
            target_column: 0,
            current_column: None,
            scroll_lines: 0,
            scroll_columns: 0,
        }
    }

    /// get  the character under the cursor
    pub fn get_character_under_cursor(&self) -> char {
        todo!()
    }

    /// move cursor horizontally
    pub fn move_cursor_horizontal(&mut self, amount: isize) {
        // just move it
        self.cursor = move_grapheme(amount, self.cursor, self.text.slice(..));

        // and recalculate the target column
        self.target_column = self.get_cursor_column();
    }

    /// move cursor vertically
    pub fn move_cursor_vertical(&mut self, amount: isize) {
        // find where the next line is
        let next_line = (self.text.byte_to_line(self.cursor) as isize + amount)
            .max(0)
            .min(self.text.len_lines().saturating_sub(1) as isize) as usize;

        // find where it starts and move there
        self.cursor = self.text.line_to_byte(next_line);

        // we are now at column 0, and a new row
        self.column = 0;
        self.row = next_line;

        // move horizontally until we are at the target
        self.move_cursor_to_column(self.target_column);
    }

    /// insert a character
    pub fn insert_character(&mut self, character: char) {
        // insert
        self.text
            .insert_char(self.text.byte_to_char(self.cursor), character);

        // move the cursor over
        self.move_cursor_horizontal(1);
    }

    /// insert a string
    pub fn insert_string(&mut self, string: &str) {
        // insert
        self.text
            .insert(self.text.byte_to_char(self.cursor), string);

        // move the cursor over
        self.move_cursor_horizontal(string.graphemes(true).count() as isize);
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
            .take_while(
                |x| x.is_whitespace() && !is_newline(*x)
            )
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
            self.move_cursor_horizontal(-1);

            // we are now at the start
            let start = self.text.byte_to_char(self.cursor);

            // remove it
            self.text.remove(start..end);
        } else {
            // start of the character to remove
            let start_byte = self.cursor;
            let start = self.text.byte_to_char(start_byte);

            // move forward
            self.move_cursor_horizontal(1);

            // we are now at the end
            let end = self.text.byte_to_char(self.cursor);

            // restore position
            self.cursor = start_byte;

            // remove the next character
            self.text.remove(start..end);
        }
    }

    /// move the cursor to a specific column, assuming a terminal program
    pub fn move_cursor_to_column(&mut self, column: usize) {
        // TODO: make this generic, together with get cursor column
        // let it take in a function that lays out the text for the line, and then returns the positions 
        // of all text items with their byte index (either with iterator or vec)
        // store this in the TextEditor so later a binary or linear search can be done easier to figure out the right grapheme to select
        
        // move the cursor to the start of the line
        self.move_cursor_to_start_of_line();

        // get the current line
        let line = self.text.byte_to_line(self.cursor);

        // find the line end
        let line_end = self.text.line(line).len_bytes() + self.cursor;

        // where we are now
        let mut current_column = 0;

        // go one to the right untill we are at the right position
        while current_column < column {
            // start byte pos
            let start_byte_pos = self.cursor;

            // and end byte pos to figure out the end of the grapheme
            let end_byte_pos = move_grapheme(1, self.cursor, self.text.slice(..));

            // the bit of the rope to search in
            let slice = self.text.slice(
                self.text.byte_to_char(start_byte_pos)..self.text.byte_to_char(end_byte_pos),
            );
            
            // stop if it contains a newline
            if slice.chars().find(|x| is_newline(*x)).is_some() {
                break;
            }
            
            // figure out it's length
            let grapheme_len = rope_width(slice);

            // add it to the column
            current_column += grapheme_len;

            // and move the cursor
            self.cursor = end_byte_pos;

            // stop if it's on the next line
            if end_byte_pos >= line_end {
                break;
            }
        }
    }

    /// move the cursor to the start of the line
    pub fn move_cursor_to_start_of_line(&mut self) {
        self.cursor = self.text.line_to_byte(self.text.byte_to_line(self.cursor));
    }

    /// move the cursor to the end of the line
    pub fn move_cursor_to_end_of_line(&mut self) {
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
                self.move_cursor_horizontal(-1);
            }
        }
    }

    /// get the current position of the column the cursor is on, assuming a terminal program
    pub fn get_cursor_column(&self) -> usize {i
        // TODO: also make this generic
        // using the same trick as in move cursor to column
        // effectively rasterize a line when going to it, then search in the laid out text to find where the cursor is
        
        // get the line and line number
        let line = self.text.byte_to_line(self.cursor);

        // where it starts
        let line_byte_idx = self.text.line_to_byte(line);

        // current byte position
        let mut byte_pos = line_byte_idx;

        // current horizontal position
        let mut column = 0;

        // now figure out where we are on the line
        while byte_pos < self.cursor {
            // start byte pos
            let start_byte_pos = byte_pos;

            // and end byte pos to figure out the end of the grapheme
            let end_byte_pos = move_grapheme(1, byte_pos, self.text.slice(..));

            // figure out it's length
            let grapheme_len = rope_width(self.text.slice(
                self.text.byte_to_char(start_byte_pos)..self.text.byte_to_char(end_byte_pos),
            ));

            // add it to the column
            column += grapheme_len;

            // move the byte pos
            byte_pos = end_byte_pos;
        }

        column
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

    /// get the currently visible buffer, as a list of lines
    // TODO: ITERATOR!
    pub fn get_buffer(&self, width: usize, height: usize) -> String {
        // all found text lines
        //let mut lines = String::new();

        // go over the lines in the buffer
        let lines = (self.scroll_lines..self.scroll_lines + height)
            .into_iter()
            .map(|i| {
                // get the line in the buffer, right padded with spaces
                // until it's too long to fit in the buffer

                self.text
                    .get_line(i)
                    .map(|x| x.chars())
                    .into_iter()
                    .flatten()
                    .chain(std::iter::repeat(' '))
                    .scan(0, |state, c| {
                        if *state < width {
                            Some(
                                c.width_cjk()
                                    .filter(|w| *state + w < width)
                                    .map(|width| {
                                        // TODO: fix this, idk how
                                        // TODO: tabs
                                        *state += width;
                                        c
                                    })
                                    .unwrap_or_else(|| {
                                        *state += 1;
                                        ' '
                                    }),
                            )
                        } else {
                            None
                        }
                    })
            })
            .flatten()
            .collect();

        // return the buffer
        lines
    }
}

/// iterator over the characters in the visible buffer
pub struct TextEditorBuffer<'a> {
    /// current width of the buffer
    width: usize,
    
    /// current height of the buffer
    height: usize,
    
    /// editor
    editor: &'a TextEditor,
}

impl<'a> Iterator for TextEditorBuffer<'a> {
    type Item = char;
    
    fn next(&mut self) -> Option<Self::Item> {
        todo!()
        
        // iterate over the lines
        
        // iterate over all characters and their column
        
        // now filter out the ones we don't want and instead emit a space in that case
        
    }
}

// terminal ==============================================

fn render(buffer: &str, cursor: Option<(usize, usize)>) {
    // print editor
    queue!(stdout(), cursor::MoveTo(0, 0), style::Print(buffer),).unwrap();

    // TODO: diff
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
    if let Some((x, y)) = cursor {
        queue!(stdout(), cursor::Show, cursor::MoveTo(x as u16, y as u16)).unwrap();
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
    let Ok(file_content) = std::fs::read_to_string(file_path) else {
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
            cursor::Show
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
        terminal::EnterAlternateScreen,
        terminal::Clear(terminal::ClearType::Purge),
        cursor::MoveTo(0, 0),
    )
    .unwrap();

    // state
    let (mut width, mut height) = terminal::size().unwrap();

    // editor
    let mut editor = TextEditor::new(&file_content);

    // draw beforehand
    render(
        &editor.get_buffer(width as usize, height as usize),
        Some(editor.get_cursor_pos()),
    );

    // event loop
    loop {
        if poll(std::time::Duration::from_millis(100)).unwrap() {
            // input
            match read().unwrap() {
                Event::Key(KeyEvent {
                    code, modifiers, ..
                }) => {
                    // quit if needed
                    if code == KeyCode::Char('q') && modifiers == KeyModifiers::ALT {
                        break;
                    }

                    // move cursor
                    if code == KeyCode::Up {
                        editor.move_cursor_vertical(-1);
                    } else if code == KeyCode::Down {
                        editor.move_cursor_vertical(1);
                    } else if code == KeyCode::Left {
                        editor.move_cursor_horizontal(-1);
                    } else if code == KeyCode::Right {
                        editor.move_cursor_horizontal(1);
                    } else if code == KeyCode::Home {
                        editor.move_cursor_to_start_of_line();
                    } else if code == KeyCode::End {
                        editor.move_cursor_to_end_of_line();
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
                        &editor.get_buffer(width as usize, height as usize),
                        editor.get_relative_cursor_pos(),
                    );
                }

                Event::Resize(..) => {
                    width = terminal::size().unwrap().0;
                    height = terminal::size().unwrap().1;

                    // render
                    render(
                        &editor.get_buffer(width as usize, height as usize),
                        Some(editor.get_cursor_pos()),
                    );
                }
                _ => (),
            }
        } else {
            // poll editor
            // TODO!
        }
    }

    // reset mode
    execute!(
        stdout(),
        terminal::LeaveAlternateScreen,
        cursor::MoveTo(0, terminal::size().unwrap().1),
        cursor::Show,
    )
    .unwrap();
    terminal::disable_raw_mode().unwrap();

    println!("Done");
}

fn main() {
    // TODO: cmd args

    terminal_main();
}
