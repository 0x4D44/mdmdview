# Table alignment with long lines

| Left | Center | Right |
| :--- | :---: | ---: |
| L1 | C1 | 100 |
| L2 | C2 | 200 |

# Table line breaks
| Item | Notes |
| --- | --- |
| Basic | line one<br>line two |
| Self close | line one<br/>line two |
| Spaced | line one<br />line two |
| Mixed | start<br>**bold** end |
| Split | a<br>**b**<br>c |
| Strong | **alpha<br>beta** |
| Attr | line one<br class="tight">line two |

# Table inline elements
| Type | Example | Notes |
| --- | --- | --- |
| Strong | **bold and *italic*** | mixed emphasis |
| Strike | ~~strike~~ | combined with `code` |
| Code | `inline code` | `a\|b` stays together |
| Link | [Example](https://example.com) | https://example.com |
| Emoji | :smile: 🦇 | shortcode expansion |
| Image | ![smile](assets/emoji/1f600.png) | small image in cell |

# Tables in blockquotes
> Simple table in a quote:
> | Col | Notes |
> | --- | --- |
> | One | line one<br>line two |
> | Two | inline `a\|b` code |
> After table text.

> Nested quote with table:
> > | Left | Center | Right |
> > | :--- | :---: | ---: |
> > | L1 | C1 | 100 |
> > | L2 | C2 | 200 |

# Tables in lists
- Simple table in list item:
  | Name | Value |
  | --- | --- |
  | Alpha | 1 |
  | Beta | a\|b |

- With blank line:

  | Key | Notes |
  | --- | --- |
  | One | line one<br>line two |
  | Two | inline `x\|y` code |

1. Ordered list item with table:

   | Col | Val |
   | --- | --- |
   | R1 | 10 |
   | R2 | 20 |

- Nested list with table:
  - Inner item
    | L | R |
    | --- | --- |
    | A | 1 |
    | B | 2 |
