// terminal deps
use crossterm::{
    cursor,
    event::{poll, read, Event, KeyCode, KeyEvent, KeyModifiers},
    execute, queue, style,
    style::{Attribute, Color},
    terminal,
    tty::IsTty,
};

use std::io::{stdout, Write};

// editor deps
use ropey::Rope;
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete};
use unicode_width::UnicodeWidthChar;

// editor ====================================================

// Ｈｅｌｌｏ, ｗｏｒｌｄ!
// اربك تكست هو اول موقع يسمح لزواره الكرام بتحويل الكتابة العربي الى كتابة مفهومة من قبل اغلب برامج التصميم
// 👩‍🔬👩🔬
// 🇷🇸🇮🇴

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

    /// scolled lines
    scroll_lines: usize,

    /// scrolled columns
    scroll_columns: usize,
}

impl TextEditor {
    pub fn new() -> Self {
        Self {
            text: Rope::from_str(include_str!("main.rs")),
            cursor: 0,
            column: 0,
            row: 0,
            target_column: 0,
            scroll_lines: 0,
            scroll_columns: 0,
        }
    }

    /// get  the character under the cursor
    pub fn get_character_under_cursor(&self) -> char {
        todo!()
    }

    /// move cursor horizontally
    /// wrap means that it can move past a newline, and go on to the next line
    // TODO! steal from helix
    pub fn move_cursor_horizontal(&mut self, amount: isize, wrap: bool) {
        // make a cursor
        let mut cursor = GraphemeCursor::new(self.cursor, self.text.len_bytes(), true);

        // make the context
        let (mut chunk, mut chunk_idx, _, _) = self.text.chunk_at_byte(self.cursor);

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
                        self.cursor = if amount > 0 { self.text.len_bytes() } else { 0 };
                        return;
                    }

                    // found a border, move the cursor there
                    Ok(Some(n)) => {
                        self.cursor = n;
                        break;
                    }

                    // need more context
                    Err(GraphemeIncomplete::NextChunk) => {
                        // get the next chunk
                        let (next_chunk, next_chunk_idx, _, _) =
                            self.text.chunk_at_byte(self.cursor + chunk.len());
                        chunk = next_chunk;
                        chunk_idx = next_chunk_idx;
                    }

                    // need more context
                    Err(GraphemeIncomplete::PrevChunk) => {
                        // get the previous chunk
                        let (prev_chunk, prev_chunk_idx, _, _) =
                            self.text.chunk_at_byte(self.cursor.saturating_sub(1));
                        chunk = prev_chunk;
                        chunk_idx = prev_chunk_idx;
                    }

                    // provide context
                    Err(GraphemeIncomplete::PreContext(n)) => {
                        // get the context
                        let ctx = self.text.chunk_at_byte(n.saturating_sub(1)).0;
                        cursor.provide_context(ctx, n - ctx.len());
                    }

                    _ => unreachable!(),
                }
            }
        }
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
        //self.move_cursor_horizontal(self.target_column as isize, false);
    }

    /// insert a character
    pub fn insert_character(&mut self, character: char) {
        // insert
        self.text.insert_char(self.text.byte_to_char(self.cursor), character);
        
        // move the cursor over
        self.move_cursor_horizontal(1, false);
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

    /// get the currently visible buffer, as a list of lines
    // TODO: ITERATOR!
    pub fn get_buffer(&self, width: usize, height: usize) -> String {
        // figure out the amount of characters in the gutter
        let gutter_chars_max = ((height as f32).log10().floor() + 1.0) as usize;

        // all found text lines
        //let mut lines = String::new();

        // go over the lines in the buffer
        let lines = (0..height)
            .into_iter()
            .map(|i| {
                // how wide this gutter is
                let gutter = ""; //format!("{}", i);

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
                                        if width == 0 {
                                            *state += 1;
                                            ' '
                                        } else {
                                            *state += width;
                                            c
                                        }
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

    pub fn get_cursor_pos(&self) -> (u16, u16) {
        (self.column as u16, self.row as u16)
    }
}

// terminal ==============================================

fn render(buffer: &str, cursor: Option<(u16, u16)>) {
    // print editor
    queue!(stdout(), cursor::MoveTo(0, 0), style::Print(buffer),).unwrap();

    // set cursor
    if let Some((x, y)) = cursor {
        queue!(stdout(), cursor::Show, cursor::MoveTo(x, y)).unwrap();
    } else {
        queue!(stdout(), cursor::Hide).unwrap();
    };

    // save changes
    stdout().flush().unwrap();
}

fn terminal_main() {
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
    let mut editor = TextEditor::new();

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
                        editor.move_cursor_horizontal(-1, true);
                    } else if code == KeyCode::Right {
                        editor.move_cursor_horizontal(1, true);
                    }

                    // insert text
                    if let KeyCode::Char(c) = code {
                        editor.insert_character(c);
                    }

                    // remove text
                    if code == KeyCode::Backspace {
                        editor.remove_character(true);
                    } else if code == KeyCode::Delete {
                        editor.remove_character(false);
                    }

                    // render
                    render(
                        &editor.get_buffer(width as usize, height as usize),
                        Some(editor.get_cursor_pos()),
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
