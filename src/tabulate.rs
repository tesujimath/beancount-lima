use std::fmt::Display;
#[cfg(test)]
use test_case::test_case;

struct Tabulated<T>(Vec<Vec<T>>);

impl<T> Display for Tabulated<T>
where
    T: AsRef<str>,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO: padding, left/right/decimal justification
        let mut column_widths = Vec::<usize>::default();

        // determine column widths
        for row in self.0.iter() {
            while row.len() > column_widths.len() {
                column_widths.push(0);
            }

            for i in 0..row.len() {
                let cell_len = row[i].as_ref().len();
                if cell_len > column_widths[i] {
                    column_widths[i] = cell_len
                }
            }
        }

        // output values in those column widths

        for row in self.0.iter() {
            for i in 0..row.len() {
                let cell = &row[i];
                let cell_len = cell.as_ref().len();
                // right justified in with single space padding for now
                let n_sep = if i > 0 { 1 } else { 0 };
                let pad = " ".repeat(column_widths[i] + n_sep - cell_len);
                write!(f, "{}{}", &pad, cell.as_ref())?;
            }
            f.write_str("\n")?;
        }
        Ok(())
    }
}

pub fn tabulate(rows: Vec<Vec<String>>) -> String {
    Tabulated(rows).to_string()
}

#[cfg(test)]
#[test_case(
    vec![
        vec!["simon", "was", "here"],
        vec!["but", "jimmy", "wasn't"], vec!["short row"]], r#"
    simon   was   here
      but jimmy wasn't
short row
"#)]
fn test_tabulate(rows: Vec<Vec<&str>>, expected: &str) {
    let rows_as_strings = rows
        .into_iter()
        .map(|row| row.into_iter().map(|s| s.to_string()).collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let actual = tabulate(rows_as_strings);
    let expected = &expected[1..];
    assert_eq!(actual, expected);
}
