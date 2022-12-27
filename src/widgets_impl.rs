//! UI widget implementation

use crate::widgets::*;
use crate::editor::*;
use crate::ui::*;
use crate::terminal::*;
use crate::unicode::*;

/// Enum representing an action in the UI
/// For a larger program, it may make more sense to have a large program struct manage this, and let the actual widgets manage the state
/// Mininotes is small enough to not care about this
#[derive(Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum UiEvent {
    /// clicked a position, or dragged it to there
    Clicked(usize, usize, bool),

    /// scroll an entire page, bool is for up (true) or down
    ScrollPage(bool),

    /// do nothing
    Nothing,
}

/// reaction that comes back from the ui
#[derive(Copy, Clone)]
pub enum UiReaction {
    /// fix scrolling
    FixScrol(usize, usize),

    /// set the real cursor position in the editor
    SetRelativeCursorPos(usize, usize, bool),

    /// scroll by some amount
    ScrollBy(isize),
}

impl<'a> Drawable<TerminalBuffer> for TextLine<'a> {
    fn draw(&self, width: u32, height: u32) -> TerminalBuffer {
        // simply add extra padding
        (
            self.string
                .chars()
                .chain(std::iter::repeat(' '))
                .scan(0, |acc, x| {
                    *acc += string_width(std::iter::once(x), TERM_TAB_WIDTH);
                    if *acc <= width as usize * height as usize {
                        Some(x)
                    } else {
                        None
                    }
                })
                .map(|c| Char::new(c, Highlight::Status))
                .collect(),
            None,
        )
    }
}

impl<'a> Interactive<UiEvent, Vec<UiReaction>> for TextLine<'a> {
    fn interact(&self, _: &UiEvent, _: u32, _: u32, _: u32, _: u32) -> Vec<UiReaction> {
        Vec::new()
    }
}

impl<'a> Widget<TerminalBuffer, UiEvent, Vec<UiReaction>> for TextLine<'a> {
    fn minimum_size(&self, _: u32, _: u32) -> (u32, u32) {
        (string_width(self.string.chars(), TERM_TAB_WIDTH) as u32, 1)
    }

    fn maximum_size(&self, width: u32, height: u32) -> (u32, u32) {
        (width, height)
    }
}

impl Drawable<TerminalBuffer> for LineNumbers {
    fn draw(&self, width: u32, height: u32) -> TerminalBuffer {
        // buffer to add to
        let mut buffer = Vec::with_capacity(width as usize * height as usize);

        // figure out the max number padding
        let padding = self.width_number(height as usize);

        // figure out the number of spaces to pad at the front
        let space_padding = (width as usize).saturating_sub(padding + 1).max(1);

        // lines to show, start and end
        let start = self.start + 1;
        let end = (self.start + 1 + height as usize).min(self.total + 1);

        // add the line numbers
        for line in start..end {
            // current column
            let mut column = 0;

            // push starting space
            while column < (width as usize).min(space_padding) {
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
            while base > 0 && column < width as usize {
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
            if column < width as usize {
                buffer.push(Char::new(' ', Highlight::Gutter));
            }
        }

        // and the rest
        buffer.extend(
            // generate the ~, over and over
            std::iter::repeat((0..width as usize).map(|x| {
                if x == padding {
                    Char::new('~', Highlight::Gutter)
                } else {
                    Char::new(' ', Highlight::Gutter)
                }
            }))
            // for the remaining lines
            .take((height as usize).saturating_sub(end - start))
            .flatten(),
        );

        // return
        (buffer, None)
    }
}

impl Interactive<UiEvent, Vec<UiReaction>> for LineNumbers {
    fn interact(&self, _: &UiEvent, _: u32, _: u32, _: u32, _: u32) -> Vec<UiReaction> {
        Vec::new()
    }
}

impl Widget<TerminalBuffer, UiEvent, Vec<UiReaction>> for LineNumbers {
    fn minimum_size(&self, _: u32, height: u32) -> (u32, u32) {
        (self.width(height as usize) as u32, height)
    }

    fn maximum_size(&self, _: u32, height: u32) -> (u32, u32) {
        (self.width(height as usize) as u32, height)
    }
}

impl Drawable<TerminalBuffer> for TextEditor<TermLineLayoutSettings> {
    fn draw(&self, width: u32, height: u32) -> TerminalBuffer {
        // all found text lines
        let mut buffer = Vec::with_capacity(width as usize * height as usize);

        // selection range, empty if none exists
        let selection_range = self.get_selection_range().unwrap_or(0..0);

        // go over all lines in the buffer
        for line_num in self.get_lines_scrolled()..self.get_lines_scrolled() + height as usize {
            // current column
            let mut column = 0;

            // current cursor in the column
            let mut cursor = 0;

            // get the line
            if let Some(line) = self.get_text().get_line(line_num) {
                // where in the buffer the line is
                let line_start = self.get_text().line_to_byte(line_num);

                // go over all columns until we are either out of bounds or end of the line
                while cursor < line.len_bytes()
                    && column < self.get_columns_scrolled() + width as usize
                {
                    // get the next cursor pos
                    let next_cursor = move_grapheme(1, cursor, line);

                    // get the grapheme
                    let grapheme = line.byte_slice(cursor..next_cursor);

                    // stop if it's a newline
                    if grapheme.chars().any(|x| is_newline(x)) {
                        // if there's still room on the line, mark this as selected, if that's the case
                        if column >= self.get_columns_scrolled()
                            && column + 1 <= self.get_columns_scrolled() + width as usize
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
                    let grapheme_width = string_width(grapheme.chars(), self.get_tab_width());

                    // if it doesn't fit in the buffer, pad spaces
                    if column < self.get_columns_scrolled()
                        && column + grapheme_width > self.get_columns_scrolled()
                    {
                        buffer.extend(
                            std::iter::repeat(Char::new_text(' ', false))
                                .take(column + grapheme_width - self.get_columns_scrolled()),
                        );

                    // if we exceed the line, pad spaces instead
                    } else if column + grapheme_width > self.get_columns_scrolled() + width as usize
                    {
                        buffer.extend(
                            std::iter::repeat(Char::new_text(' ', false))
                                .take(self.get_columns_scrolled() + width as usize - column),
                        );

                    // otherwise, add spaces if it's a tab
                    } else if column >= self.get_columns_scrolled()
                        && column + grapheme_width <= self.get_columns_scrolled() + width as usize
                        && grapheme.chars().eq(std::iter::once('\t'))
                    {
                        buffer.extend(std::iter::repeat(' ').take(self.get_tab_width()).map(|x| {
                            Char::new_text(x, selection_range.contains(&(cursor + line_start)))
                        }));

                    // otherwise, add characters
                    } else if column >= self.get_columns_scrolled()
                        && column + grapheme_width <= self.get_columns_scrolled() + width as usize
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
            buffer.extend(
                std::iter::repeat(Char::new_text(' ', false)).take(
                    (width as usize + self.get_columns_scrolled())
                        .saturating_sub(column.max(self.get_columns_scrolled())),
                ),
            );
        }

        // relative cursor position
        let cursor_pos = self.get_relative_cursor_pos();

        // and return the buffer with the cursor position)
        (buffer, cursor_pos)
    }
}

impl Interactive<UiEvent, Vec<UiReaction>> for TextEditor<TermLineLayoutSettings> {
    fn interact(
        &self,
        event: &UiEvent,
        x: u32,
        y: u32,
        width: u32,
        height: u32,
    ) -> Vec<UiReaction> {
        let extra = match event {
            // make sure the position is clicked correctly
            UiEvent::Clicked(cx, cy, select) => {
                // click position
                let (click_x, click_y) = (*cx as isize - x as isize, *cy as isize - y as isize);

                // only return it if it's in bounds
                if click_x >= 0
                    && click_x < width as isize
                    && click_y >= 0
                    && click_y < height as isize
                {
                    Some(UiReaction::SetRelativeCursorPos(
                        click_x as usize,
                        click_y as usize,
                        *select,
                    ))
                } else {
                    None
                }
            }

            // scroll up
            UiEvent::ScrollPage(up) => Some(if *up {
                UiReaction::ScrollBy(-(height as isize))
            } else {
                UiReaction::ScrollBy(height as isize)
            }),

            // rest means no extra reaction
            _ => None,
        };

        // and emit the  right events
        [UiReaction::FixScrol(width as usize, height as usize)]
            .into_iter()
            .chain(extra.into_iter())
            .collect()
    }
}

impl Widget<TerminalBuffer, UiEvent, Vec<UiReaction>> for TextEditor<TermLineLayoutSettings> {
    fn minimum_size(&self, width: u32, height: u32) -> (u32, u32) {
        (width, height)
    }

    fn maximum_size(&self, width: u32, height: u32) -> (u32, u32) {
        (width, height)
    }
}

impl OutputResult for Vec<UiReaction> {
    fn empty() -> Self {
        Vec::new()
    }

    fn combine(self, other: Self) -> Self {
        self.into_iter().chain(other.into_iter()).collect()
    }
}

impl DrawResult for TerminalBuffer {
    fn empty(width: u32, height: u32) -> Self {
        (
            std::iter::repeat(Char::new_text(' ', false))
                .take(width as usize * height as usize)
                .collect(),
            None,
        )
    }

    fn combine_vertical(self, other: Self, _: u32, split: u32, _: u32) -> Self {
        (
            // stitch together
            self.0.iter().chain(other.0.iter()).copied().collect(),
            // pick one, and adjust for size
            self.1.or(other.1.map(|(x, y)| (x, y + split as usize))),
        )
    }

    fn combine_horizontal(self, other: Self, width: u32, split: u32, _: u32) -> Self {
        // iterator over the buffers
        let mut left_chars = self.0.iter();
        let mut right_chars = other.0.iter();

        // current column
        let mut column = 0;

        // string to generate
        let mut buffer = Vec::with_capacity(self.0.len() + other.0.len());

        // and add the lines together
        while let Some(character) = if column < split as usize {
            left_chars.next()
        } else {
            right_chars.next()
        } {
            // add to the column
            column += string_width(std::iter::once(character.c), TERM_TAB_WIDTH);

            // wrap if it's on the next line
            if column >= width as usize {
                column -= width as usize;
            }

            // push it
            buffer.push(*character);
        }

        (
            buffer,
            // adjust for size
            self.1.or(other.1.map(|(x, y)| (x + split as usize, y))),
        )
    }
}