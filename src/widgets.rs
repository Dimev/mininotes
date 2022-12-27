//! UI widgets

/// Show a bar of line numbers
#[derive(Copy, Clone)]
pub struct LineNumbers {
    /// start of the range
    pub start: usize,

    /// total number of lines
    pub total: usize,

    /// current line
    pub current: usize,

    /// whether it's relative
    pub relative: bool,
}

impl LineNumbers {
	/// create a new line numbers bar
	/// bar starts at start, total is the total number of lines that can be displayed, 
	/// relative for whether to display line numbers as relative. 
    pub fn new(start: usize, total: usize, current: usize, relative: bool) -> Self {
        Self {
            start,
            total,
            current,
            relative,
        }
    }

    /// get how wide this should be
    pub fn width(self, height: usize) -> usize {
        self.width_number(height) + 2
    }

    /// how wide the number should be
    pub fn width_number(self, height: usize) -> usize {
        if self.relative {
            // get the max difference
            let max_diff = height;

            // get the one with the most digits, aka highest
            let max = max_diff.max(self.total);

            // figure out the number of digits
            (max as f64).log10() as usize + 1
        } else {
            // number of digits
            (self.total as f64).log10() as usize + 1
        }
    }
}

/// Display a single line of text
pub struct TextLine<'a> {
    /// string to display
	pub string: &'a str,
}

impl<'a> TextLine<'a> {
	/// Create a new display
    pub fn new(string: &'a str) -> Self {
        Self { string }
    }
}