# Syntax of a Binary Pattern

The syntax for this crate's binary patterns is primarily inspired by the [pelite](https://docs.rs/pelite/latest/pelite/pattern/fn.parse.html) crate's pattern system, aligning with existing de facto standards to simplify migration. Additionally, numerous enhancements have been introduced to facilitate matching against generated assembly instructions for function or code signatures.

Below, all available operators are defined and explained.

# Available Operators

## Binary Data (`<hex>`)

The **Binary Data** operator is the most fundamental operator. It performs a byte-by-byte comparison of the input with the specified hexadecimal values. Each byte must be written in hexadecimal format and padded to two digits.

The following example searches for the hexadecimal sequence `0xFF 0xDE 0x01 0x23` in the target:

```pattern
FF DE 01 23
```

Spaces between the hexadecimal values are optional. The following example is equivalent to the one above:

```pattern
FFDE0123
```

## Masked Binary Data (`<hex> & <hex>`)

The **Masked Binary Data** operator is extends the Binary Data Operator. It performs a byte-by-byte comparison of the input with the specified hexadecimal values under the given mask (binary and). Each byte and mask entry must be written in hexadecimal format and padded to two digits.

The following example searches for the hexadecimal sequence `0xFF 0xEE` wit hthe mask `0xFF 0x0F` in the target:

```pattern
FFEE & FF0F
```

In opposition to the Binary Data operator, spaces are not allowed in the binary and the mask data.

## Byte Wildcard (`?`)

The Byte Wildcard operator (`?`) matches any byte value, serving as the opposite of the Binary Data operator.
For example, the following pattern matches any 32-bit relative call instruction (`E8 rel32`) followed by a return (`C3`) in x86 assembly:

```pattern
E8 ? ? ? ? C3
```

Note that a single question mark matches a whole byte.

## Range Wildcard (`[<min>-<max>]` / `[<count>]`)

The **Range Wildcard** operator (`[<min>-<max>]` / `[<count>]`) extends the capabilities of the byte wildcard operator by allowing you to match a specific range or a fixed count of bytes with any value.

- Fixed Count Wildcard (`[<count>]`)  
  Matches an exact number of bytes. For example, the following matches a 32-bit relative call instruction (`E8`), skips four bytes, and then matches a return instruction (`C3`):

  ```pattern
  E8 [4] C3
  ```

- Variable Range Wildcard (`[<min>-<max>]`)  
  Matches a variable range of bytes.
  The matcher aligns the remaining pattern with any offset within the range.
  For instance, the following matches a sequence starting with 0xFF, followed by four to eight random bytes, and ending with 0x00:

  ```pattern
  FF [4-8] FF
  ```

## Save Cursor (`'`)

The **Save Cursor** operator (`'`) acts as a bookmark to save the current cursor's relative virtual address (RVA) in the save array returned by the matcher.
The following example would save the rva of the beginning of the counting sequence in the result array at index 1:

```pattern
FF ' 01 02 03 04
```

Note:
The first index (index 0) in the returned array from the matcher always contains the start address of the matched pattern.

## Rel/Abs Jump (`%` / `$` / `@`)

The **Jump** operator follows either a relative or absolute jump, allowing the pattern to continue matching at the resolved jump target. The following jump modes are supported:

- **1-byte relative jump**: `%`
- **4-byte relative jump**: `$`
- **8-byte absolute jump**: `@`

When using a jump operator, subsequent operations will be performed at the resolved jump location.

Example:
The following pattern matches a function call (`E8`), resolves a 4-byte relative jump (`$`), saves the function's start address to the save array, and confirms the function begins with `push rsp` (`54`):

```pattern
E8 $ ' 54
```

## Rel/Abs Jump with Sub-Pattern (`%` / `$` / `@` with `{}`)

The **Jump** operator can also match a sub-pattern at the resolved jump destination while returning the cursor to its original location after the jump. This is achieved by enclosing the sub-pattern in curly braces (`{}`) immediately following the jump symbol.

Behavior:

- The sub-pattern within the curly braces is matched at the resolved jump destination.
- After the sub-pattern is matched successfully, the cursor returns to the original location before the jump.
- The bytes defining the jump are skipped, and matching continues from that point.

Example:

The following pattern matches a function call (`E8`), resolves a 4-byte relative jump (`$`), confirms the jump target begins with `push rsp` (`54`), saves the target address, and then continues matching after the jump:

```pattern
E8 $ { ' 54 }
```

## Branch (`(<pattern a> | <pattern b> [ | <pattern n> ])`)

The **Branch** operator enables matching against one of multiple specified patterns. It allows for flexibility in matching sequences where alternatives are valid. This operator is especially useful when dealing with multiple valid opcode variations or alternative byte sequences.

Example:  
The following pattern matches any of these sequences: 0xFF 0x01 0xFF, 0xFF 0x03 0xFF, or 0xFF 0xFF 0xFF:

```pattern
FF ( 01 | 03 | FF ) FF
```

### Branch Behaviour

The Branch operator processes alternatives from left to right:

- The matcher attempts to match each branch sequentially.
- When a branch matches, the matcher proceeds to evaluate the rest of the pattern.
- If the rest of the pattern fails to match, the matcher will backtrack and test the next branch.

## Read Value (`r1` / `r2` / `r4`)

The **Read Value** operator reads and saves a value from the matched bytes. It supports reading 1, 2, or 4 bytes and stores the result in the matched stack. This operator is particularly useful for extracting values like offsets, addresses, or immediate data from matched byte sequences.

Example:
The following pattern matches a 32-bit relative call instruction (`E8`) and saves the RVA (read from the 4 bytes following the instruction) into the matched stack at index 1:

```pattern
E8 r4
```

# Formal Syntax specification

The following ABNF specifies the general syntax:

```abnf
match_string := *(operand " ")

operand := operand_bin / operand_bin_masked / operand_wildcard_byte / operand_wildcard_range / operand_jump / operand_read / operand_cursor_save / operand_branch

operand_bin := 1*(2HEXDIG)
operand_bin_masked := operand_bin "&" operand_bin
operand_wildcard_byte := "?"
operand_wildcard_range := "[" (wildcard_fixed / wildcard_range)  "]"
operand_jump := "%" / "$" / "@" [jump_target_matcher]
operand_read := "r" ("1" / "2" / "4")
operand_cursor_save := "'"
operand_branch := "(" *( *(match_string) "|") ")"

wildcard_range := 1*DIGIT "-" 1*DIGIT
wildcard_fixed := 1*DIGIT

jump_target_matcher := "{" *(match_string) "}"
```
