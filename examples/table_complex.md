# Complex Table Formats

This file exercises complex table formatting, alignment, wrapping, escaping, and placement.
Use it for manual QA of the table renderer.

## Alignment and delimiter variants

### Standard alignment markers (outer pipes)

| Left | Center | Right |
| :--- | :----: | ---: |
| L1 | C1 | 100 |
| L2 | C2 | 200 |

### First column alignment variations

| Center | Left | Right |
| :---: | :--- | ---: |
| C1 | L1 | 100 |
| C2 | L2 | 200 |

| Right | Left | Center |
| ---: | :--- | :---: |
| 300 | L3 | C3 |
| 400 | L4 | C4 |

### No outer pipes

Left | Center | Right
:--- | :----: | ---:
L3 | C3 | 300
L4 | C4 | 400

### Extra spacing in delimiter row

| Left | Center | Right |
|  :---  |  :---:  |  ---:  |
| L5 | C5 | 500 |
| L6 | C6 | 600 |

## Inline formatting and escapes

| Type | Example | Notes |
| --- | --- | --- |
| Bold | **bold** | strong text |
| Italic | *italic* | emphasis |
| Mixed | **bold** and *italic* | combined styles |
| Strike | ~~strike~~ | strikethrough |
| Code | `inline code` | monospaced |
| Code with pipes | `a|b|c` | pipes inside code |
| Escaped pipe | a\|b | stays in one cell |
| Entity pipe | a&#124;b | html entity for pipe |
| Backslash | C:\\path\\file | literal backslashes |
| Link | [Example](https://example.com) | standard link |
| Link text with pipe | [a\|b](https://example.com) | escaped pipe in link text |
| Autolink | <https://example.com> | angle bracket form |
| HTML | <kbd>Ctrl+K</kbd> | inline html tag |

## Wrapping and long content

| Key | Description | Notes |
| --- | --- | --- |
| Short | normal text | ok |
| LongWord | averyveryverylongwordwithoutbreakstotestwrappingbehavior | should wrap |
| Sentence | This is a long sentence that should wrap within the cell when the table is narrow. | Another long sentence with multiple clauses to test wrapping. |
| Mixed | LongWordNoBreak plus spaced words to see how the renderer splits width decisions. | trailing |
| Numbers | 1234567890123456789012345678901234567890 | numeric wrap |

## Missing or extra cells

| A | B | C |
| --- | --- | --- |
| 1 | 2 | 3 |
| 4 | 5 |
| 6 | 7 | 8 | 9 |
| only one |
| | empty | |

### Table with empty header cell

| | H2 | H3 |
| --- | --- | --- |
| a | b | c |
| d | | f |

## Line breaks inside cells

| Item | Notes |
| --- | --- |
| Basic | line one<br>line two |
| Self close | line one<br/>line two |
| Spaced | line one<br />line two |
| Mixed | start<br>**bold** end |
| Split | a<br>b<br>c |
| Strong | **alpha**<br>**beta** |
| Attr | line one<br>line two |

## Wide tables

| C1 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | C11 | C12 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| r1c1 | r1c2 | r1c3 | r1c4 | r1c5 | r1c6 | r1c7 | r1c8 | r1c9 | r1c10 | r1c11 | r1c12 |
| r2c1 | r2c2 | r2c3 | r2c4 | r2c5 | r2c6 | r2c7 | r2c8 | r2c9 | r2c10 | r2c11 | r2c12 |
| r3c1 | r3c2 | r3c3 | r3c4 | r3c5 | r3c6 | r3c7 | r3c8 | r3c9 | r3c10 | r3c11 | r3c12 |

## Single column

| Item |
| --- |
| Alpha |
| line one<br>line two |
| **bold** and `code` |

## Tables in lists and blockquotes

- Simple table in list item:

  | Name | Value |
  | --- | --- |
  | Alpha | 1 |
  | Beta | a\|b |

- With blank line before the table:

  | Key | Notes |
  | --- | --- |
  | One | line one<br>line two |
  | Two | inline `x|y` code |

1. Ordered list item with table:

   | Col | Val |
   | --- | --- |
   | R1 | 10 |
   | R2 | 20 |

> Simple table in a quote:
>
> | Col | Notes |
> | --- | --- |
> | One | line one<br>line two |
> | Two | inline `a|b` code |
>
> After table text.
