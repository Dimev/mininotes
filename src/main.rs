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
use unicode_segmentation::GraphemeCursor;
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

        // loop for as long as needed
        for _ in 0..amount.abs() {
            // get the context for the current chunk
            let (chunk, chunk_start, _, _) = self.text.chunk_at_byte(self.cursor);

            // give context based on if we are on the boundary
            if chunk_start == self.cursor {
                // context at the previous one
                let (ctx_chunk, ctx_chunk_start, _, _) =
                    self.text.chunk_at_byte(self.cursor.saturating_sub(1));

                // provide
                cursor.provide_context(ctx_chunk, ctx_chunk_start);
            } else if chunk_start + chunk.len() == self.cursor {
                // context at the next one
                let (ctx_chunk, ctx_chunk_start, _, _) = self
                    .text
                    .chunk_at_byte((self.cursor + 1).min(self.text.len_bytes()));

                // provide
                cursor.provide_context(ctx_chunk, ctx_chunk_start);
            }

            // go to the next boundary
            let next_boundary = if amount > 0 {
                cursor.next_boundary(chunk, chunk_start)
            } else {
                cursor.prev_boundary(chunk, chunk_start)
            }
            .unwrap()
            .unwrap();

            // if it contained a newline *and* wrap is false, stop here

            // if it contained a newline, reset the column

            // figure out how wide the boundary was

            // use it to adjust the column

            // now adjust the cursor

            self.column = if amount > 0 {
                self.column + 1
            } else {
                self.column.saturating_sub(1)
            };
            self.cursor = next_boundary;
            //break;

            // if the next boundary is on a newline, stop
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
        self.move_cursor_horizontal(self.target_column as isize, false);
    }

    /// insert a character
    pub fn insert_character(&mut self, character: char) {
        todo!()
    }

    /// remove a character
    pub fn remove_character(&mut self, character: char) {
        todo!()
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
                std::iter::repeat(' ')
                    .take(gutter_chars_max - gutter.chars().count())
                    .chain(gutter.chars())
                    .chain(
                        self.text
                            .get_line(i)
                            .map(|x| x.chars())
                            .into_iter()
                            .flatten()
                            .chain(std::iter::repeat(' '))
                            .scan(gutter_chars_max, |state, c| {
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
                            }),
                    )
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

                    // remove text

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
