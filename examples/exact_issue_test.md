# Inline Code Test - Exact Issue

This tests the exact formatting issue reported.

## Project Structure

- `dac_tokens_export.py`: Encode audio → `.npq` tokens (default: stereo M/S).
- `npq_to_wav.py`: Decode `.npq` tokens → WAV.
- `npq_out/`: Outputs (generated `.npq` and `.wav`).
- Samples: `01.mp3`, `02.mp3` for quick trials.

Expected result: All inline code like `.npq`, `.wav`, `.py`, `.mp3` should stay inline with the text.

## Additional Tests

Multiple inline codes in one item:
- Use `.input` files to generate `.output` and `.log` files via the `.exe` tool.

Text with inline code at different positions:
- `.start` at beginning
- Middle has `.middle` code
- Ends with `.end`

Mixed formatting:
- **Bold** with `.code` inside: This is **bold text with `.inline` code** in it.
- *Italic* with `.code`: This is *italic with `.code` inline* text.

---

*Testing complete inline code rendering within list items.*
