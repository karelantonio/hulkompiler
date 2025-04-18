//! To print better error messages

use thiserror::Error;

/// A source code hint (show the code that produces the error)
/// The error may look like this:
/// ```text
///    Lines:
///     2| function hello() => 1;
///        ~~~~~~~~~~~~~~~~
/// ```
/// See [`make`], [`make_highlight_range`], [`make_highlight`]
#[derive(Debug, Error)]
#[error("Lines:\n{0}")]
pub struct LocError(pub String);


/// Create a highlight used in the error message, a highlight is just a group of ~~~~~ representing
/// an underscore.
fn make_highlight_range(start: usize, end: usize) -> String {
    return String::from_utf8(
        (0..end + 1)
            .map(|e| if e + 1 >= start { '~' } else { ' ' } as u8)
            .collect::<Vec<u8>>(),
    )
    .expect("This should be valid utf8");
}

/// Creates the highlight if is inside the given range of lines. Also highlights only the segment
/// between the given columns, not just the entire line.
fn make_highlight(
    lineno: usize,
    start_line: usize,
    start_col: usize,
    end_line: usize,
    end_col: usize,
    line: &str,
) -> Option<String> {
    if lineno < start_line || lineno > end_line {
        return None;
    }

    if lineno == start_line && lineno == end_line {
        Some(make_highlight_range(start_col, end_col))
    } else if lineno == start_line {
        Some(make_highlight_range(start_col, line.len()))
    } else if lineno == end_line {
        Some(make_highlight_range(1, end_col))
    } else {
        Some(make_highlight_range(1, line.len()))
    }
}

/// Creates the left shift in the code, like an editor.
pub fn space_shift(num: usize) -> &'static str {
    if num < 100 {
        "  "
    } else if num < 1000 {
        "   "
    } else if num < 10000 {
        "    "
    } else if num < 100000 {
        "     "
    } else {
        "      "
    }
}

/// Create a new [`LocError`] with the given text and range. A [`LocError`] is a way to inform a
/// user in a more interesting way, where the error is located, and not just give a plain: In line
/// XX, column XX. The lines and columns should start from 1, not 0
pub fn make(text: &[String], start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> LocError {
    let mut res = Vec::new();
    let mut lineno = start_line.max(3) - 2;

    for line in &text[start_line.max(3) - 3..(end_line + 2).min(text.len())] {
        res.push(format!("{lineno:2}|{line}"));

        // Make highlight
        if let Some(hi) = make_highlight(lineno, start_line, start_col, end_line, end_col, &line) {
            res.push(format!("{} {hi}", space_shift(lineno)));
        }

        lineno += 1;
    }

    LocError(res.join("\n"))
}
