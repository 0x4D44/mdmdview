# Project Structure Test

This tests the specific formatting issue you reported.

## Project Structure

- `dac_tokens_export.py`: Encode audio → `.npq` tokens (default: stereo M/S).
- `npq_to_wav.py`: Decode `.npq` tokens → WAV.
- `npq_out/`: Outputs (generated `.npq` and `.wav`).
- Samples: `01.mp3`, `02.mp3` for quick trials.

## More Complex Examples

- `file.py`: Process the `.data` files and output to `.result` format
- Use `pip install package` and run the `.exe` file afterwards
- The `config.json` contains settings for `.log` and `.tmp` files

## Expected Behavior

All the inline code elements like `.npq`, `.wav`, `.py`, etc. should now stay properly inline with the surrounding text instead of breaking to separate lines.

---

*Testing complete inline code formatting within list items.*
