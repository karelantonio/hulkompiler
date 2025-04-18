//! To print better error messages

use thiserror::Error;

/// A source code hint (show the code that produces the error)
#[derive(Debug, Error)]
#[error("Lines:\n{0}")]
pub struct LocError(pub String);

fn make_highlight_range(start: usize, end: usize) -> String {
    return String::from_utf8(
        (0..end + 1)
            .map(|e| if e + 1 >= start { '~' } else { ' ' } as u8)
            .collect::<Vec<u8>>(),
    )
    .expect("This should be valid utf8");
}

fn make_highlight(
    lineno: usize,
    start: &crate::lex::Addr,
    end: &crate::lex::Addr,
    line: &str,
) -> Option<String> {
    if lineno < start.line || lineno > end.line {
        return None;
    }

    if lineno == start.line && lineno == end.line {
        Some(make_highlight_range(start.col, end.col))
    } else if lineno == start.line {
        Some(make_highlight_range(start.col, line.len()))
    } else if lineno == end.line {
        Some(make_highlight_range(1, end.col))
    } else {
        Some(make_highlight_range(1, line.len()))
    }
}

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

pub fn make(text: &[String], start: &crate::lex::Addr, end: &crate::lex::Addr) -> LocError {
    let mut res = Vec::new();
    let mut lineno = start.line.max(3) - 2;

    for line in &text[start.line.max(3) - 3..(end.line + 2).min(text.len())] {
        res.push(format!("{lineno:2}|{line}"));

        // Make highlight
        if let Some(hi) = make_highlight(lineno, start, end, &line) {
            res.push(format!("{} {hi}", space_shift(lineno)));
        }

        lineno += 1;
    }

    LocError(res.join("\n"))
}
