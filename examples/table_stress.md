# Table Stress Suite

This file expands on examples/table_complex.md with additional edge cases for table parsing and layout.
Use it for manual QA of complex table rendering.

## Header formatting and alignment edges

| **Bold Head** | *Italic Head* | `Code|Head` | Plain |
| :-- | :--: | --: | --- |
| left | center | 100 | right |
| L2 | C2 | 200 | R2 |

| Head\|Pipe | Align | Notes |
| ---: | :---: | --- |
| 1 | 2 | escaped pipe in header |

## Minimal delimiter rows

| A | B | C |
| - | - | - |
| 1 | 2 | 3 |
| 4 | 5 | 6 |

## Whitespace and empty cells

| A | B | C | D |
| --- | --- | --- | --- |
| 1 | 2 | 3 | 4 |
| 5 | | 7 | |
| | 10 | | 12 |
| "  leading" | "trailing  " | "  both  " | " mid  space " |
| 8 | 9 | 10 | 11 | 12 |
| only one cell |

## Escapes and entities

| Case | Value | Notes |
| --- | --- | --- |
| Escaped pipe | a\|b | stays in one cell |
| Escaped backslash pipe | a\\|b | literal backslash + pipe |
| Entity pipe | a&#124;b | html entity |
| Inline code | `a|b|c` | pipes inside code |
| Inline code with backtick | ``code with `tick` `` | double backtick fence |
| Link text | [a\|b](https://example.com) | escaped pipe in link text |
| Autolink | <https://example.com?x=1&y=2> | url with query |

## Inline HTML and list-like content

| Item | Details | Notes |
| --- | --- | --- |
| Line breaks | line one<br>line two<br>line three | stacked lines |
| HTML tags | <kbd>Ctrl+K</kbd> <sub>sub</sub> <sup>sup</sup> | inline tags |
| Pseudo list | - one<br>- two<br>- three | list style text |

## Tables without outer pipes

Key | Value | Notes
--- | ---: | :---:
A | 1 | right aligned numbers
B | 200 | alignment check
C | 3000 | narrow col

## Mixed alignment with long headers

| Short | Centered header text | Right aligned number header |
| :--- | :---: | ---: |
| L1 | C1 | 100 |
| L2 | C2 | 200 |

## Nested contexts

- List item with a table:

  | Name | Value |
  | --- | --- |
  | Alpha | 1 |
  | Beta | a\|b |

- List item with blank line before table:

  | Key | Notes |
  | --- | --- |
  | One | line one<br>line two |
  | Two | inline `x|y` code |

> Blockquote with a table:
>
> | Col | Notes |
> | --- | --- |
> | One | line one<br>line two |
> | Two | inline `a|b` code |
>
> After table text.

## Wide table (15 columns)

| H1 | H2 | H3 | H4 | H5 | H6 | H7 | H8 | H9 | H10 | H11 | H12 | H13 | H14 | H15 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| r1c1 | r1c2 | r1c3 | r1c4 | r1c5 | r1c6 | r1c7 | r1c8 | r1c9 | r1c10 | r1c11 | r1c12 | r1c13 | r1c14 | r1c15 |
| r2c1 | r2c2 | r2c3 | r2c4 | r2c5 | r2c6 | r2c7 | r2c8 | r2c9 | r2c10 | r2c11 | r2c12 | r2c13 | r2c14 | r2c15 |
