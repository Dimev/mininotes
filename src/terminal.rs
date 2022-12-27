//! Terminal rendering

use crate::unicode::{TERM_TAB_WIDTH, move_grapheme, string_width};
use crate::editor::{LineLayout, GraphemePosition};
use ropey::RopeSlice;
use crossterm::{
    cursor::{self, CursorShape},
    event::{
        DisableMouseCapture, EnableMouseCapture, 
    },
    execute, queue, style,
    style::Color,
    terminal::{enable_raw_mode, disable_raw_mode, LeaveAlternateScreen, EnterAlternateScreen},
};
use std::io::{stdout, Write};

/// Layout settings, which there are none of
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

/// character type
#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct Char {
    /// character
    pub c: char,

    /// color
    pub color: Highlight,
}

impl Char {
	/// create a new char, as a text element
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

	/// create a new char from the given color and highlight
    pub fn new(c: char, color: Highlight) -> Self {
        Self { c, color }
    }
}

/// buffer type + cursor position
pub type TerminalBuffer = (Vec<Char>, Option<(usize, usize)>);

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

/// setup the terminal
pub fn setup_terminal(disable_mouse_interaction: bool) {
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
    enable_raw_mode().unwrap();

    execute!(
        stdout(),
        // so it can be restored
        cursor::SavePosition,
        // so it won't clutter other activities
        EnterAlternateScreen,
        // change cursor to a bar, as that's more clear
        cursor::SetCursorShape(CursorShape::Line),
    )
    .unwrap();

    // allow mouse usage
    if !disable_mouse_interaction {
        execute!(stdout(), EnableMouseCapture).unwrap();
    }
}

/// clean up the terminal
pub fn cleanup_terminal(message: &str) {
    execute!(
        stdout(),
        // go back to the normal screen
        LeaveAlternateScreen,
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
    disable_raw_mode().unwrap();

    // print the final message
    println!("{}", message);
}