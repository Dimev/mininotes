// modules
mod clipboard;
mod editor;
mod terminal;
mod ui;
mod unicode;
mod widgets;
mod widgets_impl;

// clipboard
use clipboard::Clipboard;
use editor::*;
use terminal::*;
use ui::*;
use widgets::*;
use widgets_impl::*;

// terminal deps
use crossterm::{
    event::{
        poll, read, Event, KeyCode, KeyEvent, KeyModifiers, MouseButton, MouseEvent, MouseEventKind,
    },
    terminal::size,
};

// path saving
use std::path::{Path, PathBuf};

// arg parsing
use clap::Parser;

/// update internal editor state, and render to a buffer
pub fn update_and_render_to_buffer(
    editor: &mut TextEditor<TermLineLayoutSettings>,
    width: usize,
    height: usize,
    filepath: &Path,
    relative_line_numbers: bool,
    event: UiEvent,
) -> TerminalBuffer {
    // get the editor cursor pos
    let (pos_x, pos_y) = {
        let (x, y) = editor.get_row_and_column();
        (x + 1, y + 1)
    };

    // make all the needed widgets
    let lines = LineNumbers::new(
        editor.get_first_visible_line(),
        editor.len_lines(),
        editor.get_current_line() + 1,
        relative_line_numbers,
    );
    let status_bar_text = format!(
        " {}{} {pos_x}:{pos_y}",
        filepath.to_string_lossy(),
        if editor.has_changed_since_save() {"*" } else { "" }
    );
    let status_bar = TextLine::new(&status_bar_text);

    // layout, and respond to the input
    let events = Layout::new(width as u32, height as u32)
        .add_item(&status_bar, Align::Bottom, Restriction::Shrink)
        .add_item(&lines, Align::Left, Restriction::Shrink)
        .add_item(editor, Align::Left, Restriction::Grow)
        .interact(&event);

    // see what happened in all events
    // in reverse to order the events right
    for event in events.into_iter().rev() {
        match event {
            UiReaction::ScrollBy(amount) => editor.scroll_vertically(amount),
            UiReaction::FixScrol(x, y) => editor.set_scroll(x, y, 6, 6),
            UiReaction::SetRelativeCursorPos(x, y, select) => {
                editor.set_relative_cursor_pos(x, y, select)
            }
        }
    }

    // remake this, as it might have changed
    let lines = LineNumbers::new(
        editor.get_first_visible_line(),
        editor.len_lines(),
        editor.get_current_line() + 1,
        relative_line_numbers,
    );

    // again, draw this time
    let buffer = Layout::new(width as u32, height as u32)
        .add_item(&status_bar, Align::Bottom, Restriction::Shrink)
        .add_item(&lines, Align::Left, Restriction::Shrink)
        .add_item(editor, Align::Left, Restriction::Grow)
        .draw();

    // and return
    buffer
}

/// run the program in the terminal
fn terminal_main(
    file_content: String,
    newly_loaded: bool,
    save_path: PathBuf,
    relative_line_numbers: bool,
    tab_width: usize,
    disable_mouse_interaction: bool,
) {
    // set up the terminal
    setup_terminal(disable_mouse_interaction);

    // state
    let (mut width, mut height) = size().unwrap();

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
    let (mut current_buffer, cursor_position) = update_and_render_to_buffer(
        &mut editor,
        width as usize,
        height as usize,
        &save_path,
        relative_line_numbers,
        UiEvent::Nothing,
    );

    // and render to the terminal
    render(width as usize, cursor_position, &current_buffer, &[]);

    // event loop
    loop {
        if poll(std::time::Duration::from_millis(100)).unwrap() {
            // input
            match read().unwrap() {
                Event::Mouse(MouseEvent {
                    row, column, kind, ..
                }) if !disable_mouse_interaction
                    && (kind == MouseEventKind::Down(MouseButton::Left)
                        || kind == MouseEventKind::Drag(MouseButton::Left)) =>
                {
                    // render
                    let (next_buffer, cursor_position) = update_and_render_to_buffer(
                        &mut editor,
                        width as usize,
                        height as usize,
                        &save_path,
                        relative_line_numbers,
                        UiEvent::Clicked(
                            column as usize,
                            row as usize,
                            kind == MouseEventKind::Drag(MouseButton::Left),
                        ),
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

                    // the ui event to send for updating
                    let mut ui_event = UiEvent::Nothing;

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
                        if !clip.is_empty() {
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
                                if !y.is_empty() {
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
                    } else if code == KeyCode::PageUp {
                        ui_event = UiEvent::ScrollPage(true);
                    } else if code == KeyCode::PageDown {
                        ui_event = UiEvent::ScrollPage(false);
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

                    // render
                    let (next_buffer, cursor_position) = update_and_render_to_buffer(
                        &mut editor,
                        width as usize,
                        height as usize,
                        &save_path,
                        relative_line_numbers,
                        ui_event,
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
                    width = size().unwrap().0;
                    height = size().unwrap().1;

                    // render
                    let (next_buffer, cursor_position) = update_and_render_to_buffer(
                        &mut editor,
                        width as usize,
                        height as usize,
                        &save_path,
                        relative_line_numbers,
                        UiEvent::Nothing,
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

/// A small editor for the terminal
#[derive(Parser)]
struct Args {
    #[arg()]
    /// file to edit
    file_path: PathBuf,

    /// whether to allow mouse navigation
    #[arg(long, short, default_value_t = false)]
    disable_mouse_interaction: bool,

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
        args.disable_mouse_interaction,
    );
}
