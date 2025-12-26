# Table Wrapping and Long Content

| Key | Description | Notes |
| --- | --- | --- |
| Short | normal text | ok |
| LongWord | averyveryverylongwordwithoutbreakstotestwrappingbehavior | should not crash |
| Sentence | This is a long sentence that should wrap within the cell when the table is narrow. | Another long sentence with multiple clauses to test wrapping. |
| Mixed | LongWordNoBreak plus spaced words to see how the renderer splits width decisions. | trailing |
| Numbers | 1234567890123456789012345678901234567890 | numeric wrap |
