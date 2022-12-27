//! Unicode tools

use ropey::RopeSlice;
use unicode_segmentation::{GraphemeCursor, GraphemeIncomplete};
use unicode_width::UnicodeWidthChar;

/// Tab width the terminal renders with
pub const TERM_TAB_WIDTH: usize = 0;

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
pub fn is_newline(c: char) -> bool {
    [
        '\n', '\r', '\u{000B}', '\u{000C}', '\u{0085}', '\u{2028}', '\u{2029}',
    ]
    .contains(&c)
}