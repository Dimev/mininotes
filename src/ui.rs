//! UI layout

/// Layout align
#[derive(Copy, Clone, Eq, Ord, PartialEq, PartialOrd)]
#[allow(dead_code)]
pub enum Align {
    /// Stick to the bottom.
    /// Takes maximum size left and right
    Bottom,

    /// Stick to the top.
    /// Takes maximum size left and right
    Top,

    /// Stick to the left.
    /// Takes maximum size top and bottom
    Left,

    /// Stick to the right.
    /// Takes maximum size top and bottom
    Right,
}

/// Layout restrictions
pub enum Restriction {
    /// grow as far out as possible
    Grow,

    /// become as small as possible
    Shrink,
}

/// layout item
pub struct LayoutItem<'a, R: DrawResult, I, O: OutputResult> {
    /// x position, left to right
    x: u32,

    /// y position, top to bottom
    y: u32,

    /// width of the element
    width: u32,

    /// height of the element
    height: u32,

    /// widget
    widget: &'a dyn Widget<R, I, O>,
}

/// UI layout widget
/// if this needs to be expanded more, layers of UI layout can be added, where each layer is stacked on top of eachother
/// if UI layout is then done in stages (once for interaction, once for visuals) it would be possible to add those hovering panels with ease (assuming no interaction needed)
pub struct Layout<'a, R: DrawResult, I, O: OutputResult> {
    /// all items
    items: Vec<LayoutItem<'a, R, I, O>>,

    /// currently leftover space
    /// as a rectangle
    space: (u32, u32, u32, u32),
}

impl<'a, R: DrawResult, I, O: OutputResult> Layout<'a, R, I, O> {
    /// create a new layout
    pub fn new(width: u32, height: u32) -> Self {
        Self {
            space: (0, 0, width, height),
            items: Vec::new(),
        }
    }

    /// handle interactions
    pub fn interact(self, interactions: &I) -> O {
        // simply pass the interaction over all widgets, and combine their result
        // because `O` is a monoid, folds are trivial
        self.items
            .iter()
            .map(|x| x.widget.interact(interactions, x.x, x.y, x.width, x.height))
            .fold(O::empty(), O::combine)
    }

    /// draw the UI
    pub fn draw(self) -> R {
        // start with any empty area that is left over
        let mut result = R::empty(self.space.2 - self.space.0, self.space.3 - self.space.1);

        // figure out the current start positions
        let (mut x, mut y, mut width, mut height) = (
            self.space.0,
            self.space.1,
            self.space.2 - self.space.0,
            self.space.3 - self.space.1,
        );

        // next, go over all widgets backwards
        for item in self.items.iter().rev() {
            // calculate the new sizes
            let (new_x, new_y, new_width, new_height) = (
                // the lowest position is the new origin
                x.min(item.x),
                y.min(item.y),
                // if one of these changed, the width/height for that axis also changed
                if item.x != x || width == 0 {
                    item.width + width
                } else {
                    item.width
                },
                if item.y != y || height == 0 {
                    item.height + height
                } else {
                    item.height
                },
            );

            // figure out how to combine it with the current state
            // because we can guarantee we only expand to either one of the sides, this can be done somewhat easily
            if item.x > x || width == 0 {
                // expand to the right
                result = result.combine_horizontal(
                    item.widget.draw(item.width, item.height),
                    new_width,
                    width,
                    new_height,
                );
            } else if item.x < x {
                // expand to the left
                result = item
                    .widget
                    .draw(item.width, item.height)
                    .combine_horizontal(result, new_width, item.width, new_height);
            } else if item.y > y || height == 0 {
                // expand to the top
                result = result.combine_vertical(
                    item.widget.draw(item.width, item.height),
                    new_width,
                    item.height,
                    new_height,
                );
            } else if item.y < y {
                // expand to the bottom
                result = item
                    .widget
                    .draw(item.width, item.height)
                    .combine_vertical(result, new_width, height, new_height);
            }

            // set the new sizes
            x = new_x;
            y = new_y;
            width = new_width;
            height = new_height;
        }

        // return the result
        result
    }

    /// add an item to the UI layout
    /// This is intended to work via the builder pattern, so it consumes self, and returns a modified version
    /// It might also be possible to have an "add multiple", that accepts a list of items and lays them out horizontally/vertically
    /// This isn't needed for mininotes, so not implemented
    pub fn add_item(
        mut self,
        item: &'a dyn Widget<R, I, O>,
        align: Align,
        restriction: Restriction,
    ) -> Self {
        // figure out the orientation and in what direction to certainly expand
        let expand_horizontal = match align {
            Align::Top | Align::Bottom => true,
            Align::Left | Align::Right => false,
        };

        // current width and height
        let width = self.space.2 - self.space.0;
        let height = self.space.3 - self.space.1;

        // figure out the size this will take up
        let (min_width, min_height) = item.minimum_size(width, height);
        let (max_width, max_height) = item.maximum_size(width, height);

        // actually scale the widget
        let (widget_width, widget_height) = if expand_horizontal {
            (
                width,
                match restriction {
                    Restriction::Grow => max_height.min(height),
                    Restriction::Shrink => min_height.min(height),
                },
            )
        } else {
            (
                match restriction {
                    Restriction::Grow => max_width.min(width),
                    Restriction::Shrink => min_width.min(width),
                },
                height,
            )
        };

        // figure out the top left corner
        let (widget_x, widget_y) = match align {
            Align::Top | Align::Left => (self.space.0, self.space.1),
            Align::Bottom => (self.space.0, self.space.3 - widget_height),
            Align::Right => (self.space.2 - widget_width, self.space.1),
        };

        // add the widget
        self.items.push(LayoutItem {
            x: widget_x,
            y: widget_y,
            width: widget_width,
            height: widget_height,
            widget: item,
        });

        // resize the available space
        match align {
            Align::Top => {
                // widget fills top bar, moves down
                self.space.1 += widget_height;
            }
            Align::Bottom => {
                // widget fills bottom bar, moves up
                self.space.3 -= widget_height;
            }
            Align::Left => {
                // widget fills left bar, moves right
                self.space.0 += widget_width;
            }
            Align::Right => {
                // widget fills right bar, moves left
                self.space.2 -= widget_width;
            }
        }

        // return
        self
    }
}

/// UI widget
pub trait Widget<R: DrawResult, I, O: OutputResult>: Drawable<R> + Interactive<I, O> {
    /// Returns the preferred minimum width and height, frrom the available width and height
    fn minimum_size(&self, width: u32, height: u32) -> (u32, u32);

    /// returns the preferred maximum width and height, from the available width and height
    fn maximum_size(&self, width: u32, height: u32) -> (u32, u32);
}

/// draw a widget
/// `R` is the result a draw produces
pub trait Drawable<R: DrawResult> {
    /// draw
    fn draw(&self, width: u32, height: u32) -> R;
}

/// handle input for a widget
/// `I` is the input type, `O` is the result of an interaction
pub trait Interactive<I, O: OutputResult> {
    /// react to events
    fn interact(&self, input: &I, x: u32, y: u32, width: u32, height: u32) -> O;
}

/// Trait representing a result of a draw
/// These need to be combined after layout to produce a full draw result, so it needs to implement those functions
pub trait DrawResult {
    /// Produce a result for an empty area
    fn empty(width: u32, height: u32) -> Self;

    /// combine horizontally, self is left, other is right
    fn combine_horizontal(self, other: Self, width: u32, split: u32, height: u32) -> Self;

    /// combine vertically, self is top, other is bottom
    fn combine_vertical(self, other: Self, width: u32, split: u32, height: u32) -> Self;
}

/// Trait representing a result of an interaction
/// Multiple interactions may happen for a widget, so they need to be combined
pub trait OutputResult {
    /// Produce a result for an empty interaction
    fn empty() -> Self;

    /// combine the results
    fn combine(self, other: Self) -> Self;
}