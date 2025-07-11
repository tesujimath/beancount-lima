pub enum Align {
    Left,
    Right,
}

pub fn tabulate(rows: Vec<Vec<String>>, mut align: Vec<Align>, inter_column: &str) -> Vec<String> {
    // get width of each column, max of cell lengths
    let mut width = Vec::default();
    for row in rows.iter() {
        while width.len() < row.len() {
            width.push(0usize);
        }

        for (i, cell) in row.iter().enumerate() {
            if width[i] < cell.len() {
                width[i] = cell.len();
            }
        }
    }

    // default alignment if under-length
    while align.len() < width.len() {
        align.push(Align::Left);
    }

    rows.into_iter()
        .map(|row| {
            row.into_iter()
                .enumerate()
                .map(|(i, cell)| match align[i] {
                    Align::Left => format!("{:<width$}", cell, width = width[i]),
                    Align::Right => format!("{:>width$}", cell, width = width[i]),
                })
                .collect::<Vec<_>>()
                .join(inter_column)
        })
        .collect::<Vec<_>>()
}
