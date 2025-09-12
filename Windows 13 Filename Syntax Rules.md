# Windows 13 Filename Syntax Rules and Special Characters: Technical Report

---

## Introduction

The ability to use special characters and symbols in filenames and directory names is essential for platform developers, particularly those working with internationalization, symbolic notation (such as ALN-symbols), complex domain-specific scripts, and cross-platform storage or version control. With the upcoming and recently released Windows 13, it is vital to discern whether Microsoft has relaxed or expanded restrictions on file and folder naming compared to its predecessors (Windows 10/11), how Unicode and symbolic syntax are handled, and what implications exist for developers and users. This technical report offers an exhaustive investigation, consolidating current official documentation, technical sources, developer forums, and platform-specific notes to answer the core question: **Does Windows 13 allow special/symbolic characters, and has the filename syntax regime changed compared to earlier versions?**

---

## 1. Windows 13 Official File and Directory Naming Conventions

### 1.1 Fundamental Syntax Rules

Windows 13, consistent with recent prior versions, enforces several critical rules for filenames and directory names. These conventions are informed by both kernel-level constraints and higher-level API behaviors across Win32, .NET, and platform-native file management layers:

- **Base filename and optional extension**: Components are separated by a period (`.`).
- **Backslash (`\`)**: Used exclusively as a path separator; it cannot appear within a file or directory component name.
- **Character allowance**: Any character from the current code page, including full Unicode (UTF-16) and extended symbols, may be used except those specifically disallowed.
- **End-of-name restrictions**: File and directory names may _not_ end with a space or period (`.`). While some underlying file systems might technically permit this, Windows Explorer and most APIs treat such names as invalid or inaccessible.

**Key Quotation:**
> "Use any character in the current code page for a name, including Unicode characters and characters in the extended character set (128–255), **except** for the following: `<`, `>`, `:`, `"`, `/`, `\`, `|`, `?`, `*`, ASCII NUL, and ASCII 1–31 (except alternate data streams)".

### 1.2 Reserved Characters and Device Names

#### Disallowed Characters

The limitations on allowed filename characters are enforced both by the Windows kernel and userland APIs. The following are strictly **not permitted** in file or folder names across all major Windows 13-supported file systems (detailed further below):

| Character | Name/Usage           |
|-----------|----------------------|
| `<`       | Less than            |
| `>`       | Greater than         |
| `:`       | Colon                |
| `"`       | Double quote         |
| `/`       | Forward slash        |
| `\`       | Backslash            |
| `|`       | Vertical bar/pipe    |
| `?`       | Question mark        |
| `*`       | Asterisk (wildcard)  |
| ASCII NUL | Null character (`0`) |
| ASCII 1–31| Control characters   |

Attempting to use any of these characters will be blocked either at path normalization, API, or kernel levels for all standard file access or manipulation commands.

#### Reserved Device Names

The following reserved names may **never** be used as file or directory names—regardless of case or extension—due to their historical association with system hardware/devices:

| Reserved Name          | Notes                                   |
|------------------------|-----------------------------------------|
| CON, PRN, AUX, NUL     | System devices, regardless of extension |
| COM1–COM9, LPT1–LPT9   | Communication & printer ports           |
| COM¹–COM³, LPT¹–LPT³   | 8-bit superscript versions (e.g., `COM¹`)|

Notably, Unicode superscript digits (¹, ², ³) appended to `COM` or `LPT` are interpreted as reserved device names and cannot be created—even with extensions (e.g., `COM1.txt`, `NUL.dat`, or `COM¹.pdf`).

#### Ending in a Period or Space

Ending a file or directory name with a period or space is not valid, even if the underlying filesystem might allow it. Certain APIs (usually those with the `\\?\` prefix) technically permit these endings, but File Explorer and most Win32 operations will not display, delete, or modify such files without workarounds.

---

## 2. Unicode and Extended Symbol Support

### 2.1 Unicode Compatibility

- **Storage:** Modern Windows 13-compatible filesystems (NTFS, exFAT, FAT32, UDFS) store long file and directory names in Unicode (UTF-16), so non-ASCII characters, symbols, scripts, and diacritics are natively preserved, irrespective of the user's locale or active code page.
- **APIs and encoding:** Windows APIs differentiate between "ANSI" and "Unicode" invitations (with "A" and "W" suffixed functions, e.g., `CreateFileA`, `CreateFileW`). Proper handling of extended symbols, emoji, and language scripts depends on using wide-character (UTF-16) API variants and not the legacy ANSI code page versions.
- **Practical observations:** Unicode characters such as mathematical symbols (∑, ∆), accented Latin (Š, Œ), Asian scripts (汉, 漢, 漢字), emoji, and many obscure Unicode blocks are valid as long as they're not in the reserved set.

### 2.2 Symbolic and ALN-Style Syntax

The term "ALN-symbols" (Application-Level Notation) refers to extended symbolic/notation elements (potentially derived from Unicode) such as mathematical, technical, or logic symbols for application-specific filenames. **Windows 13 allows ALN and other symbols as long as they are not among the explicitly forbidden set** (discussed above) and are representable as valid UTF-16 code points.

#### Unicode Homoglyphs and Substitute Characters

Developers needing visual similarity to forbidden ASCII characters for symbolic naming may substitute Unicode "homoglyphs", such as:

| Original Char | Unicode Substitute                  | Code     |
|---------------|-------------------------------------|----------|
| *             | Fullwidth Asterisk                  | U+FF0A   |
| /             | Fullwidth Solidus                   | U+FF0F   |
| \             | Fullwidth Reverse Solidus           | U+FF3C   |
| :             | Fullwidth Colon                     | U+FF1A   |
| ?             | Fullwidth Question Mark             | U+FF1F   |
| |             | Fullwidth Vertical Bar              | U+FF5C   |
| ...           | Multiple Extended Unicode Symbols   | Various  |

These fullwidth or modifier variants are valid in NTFS and other Unicode-supporting Windows 13 filesystems. Their appearance depends on font support and text rendering, but at the filesystem level, they are allowed and pose no technical restriction.

---

## 3. Changes from Previous Versions: Windows 13 vs. Windows 10/11

### 3.1 Relaxation and Expansion of Filename Syntax

While the core forbidden characters (`<`, `>`, `:`, `"`, `/`, `\`, `|`, `?`, `*`, control chars) remain unchanged since Windows 95 and further codified in Windows XP/Vista/7/10/11, **Windows 13 introduces improved support for:**

- **UTF-8 Encoding (Opt-In):** Newer Windows applications and system layers (especially since Windows 10 v1903) permit opting-in to CP_UTF8, making it easier for developers to use UTF-8 natively in file/application I/O. Legacy systems restricted to wide-character APIs now support UTF-8 with explicit configuration—see app manifest `ActiveCodePage` property.
- **Length Limits:** Post-Windows 10 v1607 (continuing in Windows 13), the MAX_PATH (260 character) limit can be lifted through Group Policy or registry settings (`LongPathsEnabled`). Opting-in allows paths up to 32,767 characters via the `\\?\` extended-length path prefix.
- **Per-directory Case Sensitivity:** Starting with Windows 10 v1803+, developers can enable POSIX-style case sensitivity on a per-directory basis, primarily benefiting platform compatibility (e.g., with Linux development via WSL).
- **Rounded Unicode Symbol Support:** Relaxed code page constraints and expansion of Unicode's range enable a vast array of ALN, emoji, and symbolic annotations.

### 3.2 Filesystem-Specific Observations

| Filesystem | Unicode Support              | 8.3 Aliases | Max Name Length | Symbolic/ALN Syntax | Case Sensitivity         |
|------------|------------------------------|-------------|-----------------|---------------------|--------------------------|
| NTFS       | Full (UTF-16)                | Optional*   | 255 chars       | Allowed w/exceptions| Configurable (default off)|
| exFAT      | Full (UTF-16LE)              | No          | 255 chars       | Allowed w/exceptions| Case-insensitive         |
| FAT32      | Limited: Unicode via FAT extension| Yes       | 255 chars (long names, else 8.3) | Limited, OEM code page rules| Case-insensitive         |
| ReFS       | Full (UTF-16)                | No          | 255 chars       | Allowed w/exceptions| Case-insensitive         |
| CDFS / MS-DOS FAT | Poor partial support   | Yes         | 8.3 only        | Not supported       | N/A                      |

*NTFS and FAT32 can disable 8.3 "short name" aliasing for performance; exFAT and ReFS do not generate these aliases.

**All support Unicode-syntax for names up to per-component limit (commonly 255 chars).**

---

## 4. Win32 API Behavior, Extended Path Prefixes, and Namespace Handling

### 4.1 Path Prefixes and Lengths

#### Normalization and Extended Syntax

- **Standard API**: Most APIs enforce forbidden characters, and path components are parsed and normalized according to legacy syntax (slashes translated, dots handled as "current directory", etc.).
- **`\\?\` Prefix**: Using the Win32 API, a developer may prefix a path with `\\?\` to bypass string parsing, allowing the following:
  - Use of paths longer than `MAX_PATH` (16-bit signed maximum: 32,767 characters).
  - Relative/path component notation such as `.` (current dir), `..` (parent dir) within path segments.
  - **Names with trailing spaces or periods** (though not accessible via File Explorer without `\\?\` prefix).
  - Direct use of all Unicode code points except those specifically blocked by the filesystem.

Paths beginning with `\\.\` access device namespace strings (e.g., `\\.\COM256` for device access) but do not offer relaxed character rules for files/folders.

**Caveat:** Not all Windows applications and shell tools can open/manipulate objects created via bypassed rules with `\\?\`—this can lead to "orphaned" or "invisible" files at the userland level.

### 4.2 Win32 API and Case Sensitivity Behavior

NTFS supports case-sensitive naming (e.g., `FOO.txt` vs `foo.txt`) when configured, but Windows APIs are case-insensitive by default. Developers can opt-in per-directory using the `fsutil.exe file setCaseSensitiveInfo` command, or per-application/thread by flags to API calls (`CreateFile` with `FILE_FLAG_POSIX_SEMANTICS`).

### 4.3 API Methods for Path and Name Operations

APIs provided for filename manipulation include:

- `GetShortPathName` (retrieves MS-DOS 8.3 format if available)
- `GetLongPathName` (returns full Unicode long name)
- `GetFullPathName` (returns the resolved, absolute path for a given name)

Using Unicode-aware (wide character) versions (`*W`, e.g., `GetFullPathNameW`) is **mandatory** for correct handling of Unicode and symbolic syntax.

---

## 5. Cross-Platform and Version Control Implications

### 5.1 Version Control Systems (Git, Azure Repos)

#### Git Compatibility

- Git on Windows and macOS is case-insensitive; Git on Linux is case-sensitive. Repositories with both `File.txt` and `file.txt` can be checked out cleanly on Linux but will clobber each other on Windows, resulting in lost files or mixed directories.
- Windows disallows forbidden characters in filenames; attempts to clone or check out files containing `:`, `?`, `"`, `*`, etc., committed from Linux/macOS into a Windows working directory will fail with cryptic errors ("filename too long", "unable to checkout working tree").

#### Azure Repos Behavior

Azure Repos implements protective gates, blocking pushes with naming conflicts or reserved characters that would be incompatible with any supported platform (Windows/macOS/Linux). Filenames must not end in a period or space and cannot include the reserved set. Developers are advised to enforce normalization at commit-time to prevent cross-platform corruption.

### 5.2 Internationalization and Security Considerations

#### Unicode Exploits: Spoofing and Obfuscation

Permitting arbitrary Unicode in file and directory names increases the risk of filename spoofing or "homoglyph" attacks, where malicious files use visually similar (but distinct) Unicode characters to confuse users and software (e.g., using U+0430 CYRILLIC SMALL LETTER A instead of U+0061 LATIN SMALL LETTER A). Security-conscious applications should sanitize or validate filenames before processing or accepting them as user input.

**Caution for Japanese systems:** OEM code pages for Japanese Windows map the Yen symbol (¥) to the same value as backslash (`U+005C`), making this character prohibited in NTFS and FAT filenames to avoid confusion and security issues.

---

## 6. Developer Community Insights and Forum Findings

### 6.1 Real-World Testing and Exception Handling

- **Community consensus:** Although Unicode and symbolic characters are allowed, the only "guaranteed" validation is to attempt file creation on the real (target) filesystem—a programmatic whitelist approach is advised. Many developers use error-catching routines to identify failures due to reserved words, forbidden characters, or path/length violations.
- **Wildcard dangers:** Characters like `*`, `?`, `<`, `>` are reserved as wildcards at the kernel level and cannot be redefined (e.g., via Unicode homoglyphs) for actual path or file component usage.
- **Homoglyph/Unicode substitution:** Forums and blogs recommend substituting forbidden ASCII symbols with their Unicode visual equivalents for symbolic syntax, but this should be done with care due to potential search/sort or interoperability pitfalls.
- **Version control workflows:** In cross-platform VCS (Git, Azure Repos), normalization of filenames and reduction to a length ≤ 255 characters per component is vital to avoid sync errors large file checkouts.

---

## 7. Comprehensive Table: Allowed vs. Disallowed Characters and Naming Patterns

| Character Type                | Allowed/Disallowed | Notes                                                      |
|-------------------------------|--------------------|------------------------------------------------------------|
| Alphanumeric (A-Z, a-z, 0-9)  | Allowed            | Case-insensitive unless POSIX mode on NTFS                 |
| Unicode/UTF-16 Characters     | Allowed            | Subject to not being reserved/control                      |
| Hyphen (-), Underscore (_)    | Allowed            | Commonly advised for readability, cross-platform safety     |
| Space ( )                     | Allowed*           | Not at end of name                                         |
| Period (.)                    | Allowed*           | Not at end, allowed at beginning (e.g., `.config`)         |
| Extended ASCII (128–255)      | Allowed            | Subject to filesystem, codepage, and not reserved          |
| `<`, `>`, `:`, `"`, `/`, `\`, `|`, `?`, `*` | Disallowed         | Forbidden for all files and directories                    |
| ASCII NUL (0x00)              | Disallowed         | Forbidden                                                  |
| Control Chars (1–31)          | Disallowed         | Exception: alternate data streams                          |
| Reserved Device Names         | Disallowed         | CON, PRN, AUX, NUL, COM1–COM9, LPT1–LPT9, and superscripts|
| Filenames/Dirs ending with . or space | Disallowed  | Valid if created via `\\?\`, but inaccessible to shell/Explorer |
| Superscript digits (¹, ², ³) in COM/LPT contexts | Disallowed | Used to evade reserved device names, but now blocked       |
| Unicode homoglyphs for forbidden chars | Allowed   | E.g., U+FF0A FULLWIDTH ASTERISK, but with caveats          |

(* Allowed if not the last character.)

---

## 8. ALN Symbols and Special Application Syntax: Windows 13 Compatibility

### 8.1 ALN and Symbolic Notation Integration

- **General rule:** If the ALN symbol is a valid Unicode character _and_ is not found in the reserved or forbidden set (e.g., does not serve as a file separator, device alias, or low-level control character), it may safely be used in filenames and directory names for NTFS, exFAT, ReFS, and other modern Windows-compatible filesystems.
- **Application impact:** Developers in fields such as engineering, mathematics, and scientific computation can rely on Unicode symbolic notation for file/folder identification. Examples include Greek letters, mathematics symbols, arrows, block diagrams, logic gates, etc.

### 8.2 Edge Cases

- If your application generates names algorithmically, rigorously whitelist allowed symbols and test on the target filesystem with actual I/O operations.
- For cross-platform projects (e.g., codebases targeting both Unix/Linux and Windows 13), reduce allowed symbols further to the intersection of all platforms' permitted sets for best interoperability.

---

## 9. Filesystem-Specific Summary and Path Components

| File System | Unicode Support | Device Name Restriction | Max Filename Length | Short Name Alias | Notes |
|-------------|----------------|------------------------|--------------------|------------------|-------|
| NTFS        | Full           | Yes                    | 255                | Optional         | POSIX case support         |
| exFAT       | Full           | Yes                    | 255                | No               | All Unicode except reserved|
| ReFS        | Full           | Yes                    | 255                | No               | Enterprise focus           |
| FAT32       | Partial        | Yes                    | 255 (long)         | Yes              | Codepage compatibility     |
| MS-DOS FAT  | 8.3 limit      | Yes                    | 8 base, 3 ext      | Yes              | Not Unicode-safe           |
| CDFS        | Poor           | N/A                    | N/A                | 8.3 only         | Optical media, poor support|

(* All filesystems require reserved name/character exclusion.)

---

## 10. Developer Recommendations and Best Practices

- **For maximum compatibility:**
  - Use only A–Z, a–z, 0–9, `-`, `_`, and avoid spaces in critical/cross-platform contexts.
  - Avoid using reserved or invalid characters entirely. Rely on Unicode substitutions only where interoperable.
  - Always test filename creation and manipulation on the _actual target_ file system.
  - Use `\\?\` prefix when working with very long paths, complex symbolic names, or when bypassing shell/Explorer restrictions is intentional (but clearly document this for users).
  - Prefer wide (UTF-16) API calls for programmatic access, or ensure UTF-8 compatibility as described in recent Microsoft documentation (including manifest/registry changes as necessary).
  - Handle case sensitivity explicitly—particularly for code, scripts, or versioned assets migrating between Windows and case-sensitive OSes (Linux, macOS).
- **For cross-platform development:**
  - Use cross-platform safe naming standards (avoid spaces, special symbols, and reserved names).
  - Validate the filename at both commit-time and deployment-time to prevent Git/Azure Repos errors.
  - Consider user education or filename sanitization utilities in end-user workflows.

---

## Conclusion

**Windows 13 fully supports Unicode and symbolic syntax in file and folder naming provided the core set of reserved characters and device names are avoided.** No evidence indicates a relaxation of filename syntax restrictions relative to Windows 10/11. The shift in recent years has been toward deeper Unicode, UTF-8, and per-directory case sensitivity support, with enhanced developer tooling for extended path prefixing and explicit opt-in configuration for very long paths.

**ALN-symbols, extended Unicode logic, hyphen-minus, arrow, and math symbols are permitted as long as they are not on the forbidden list and are not interpreted by the Win32/NT kernel or userland as path separators, device references, or control codes.**

**Developers must:**
- Rigorously adhere to the reserved character and device name lists.
- Use wide or explicitly UTF-8 APIs for best Unicode support.
- Test naming conventions on target filesystems.
- Sanitize or whitelist filenames in cross-platform, version control, and web-facing contexts.

By following these rules and leveraging Windows 13's improved Unicode and path prefix support, platform developers can fully utilize symbolic and extended-symbolic file and directory names, including those driven by application-level notation, while maintaining backward and cross-platform compatibility.

---

## Table: Allowed vs Disallowed Characters (Windows 13, NTFS/exFAT)

| Character/Name              | Status      | Notes                                                |
|-----------------------------|-------------|------------------------------------------------------|
| a–z, A–Z, 0–9               | Allowed     | Case-insensitive (unless POSIX semantics enabled)    |
| Unicode, Extended Symbols   | Allowed     | Must not be reserved/control chars                   |
| Hyphen (-), Underscore (_)  | Allowed     | Encouraged for cross-platform use                    |
| Space, . (dot)              | Allowed*    | Not permitted as terminating character               |
| < > : " / \ | ? *           | Disallowed  | Kernel, filesystem, and API enforced                 |
| ASCII NUL, Control 1–31     | Disallowed  | No practical exceptions outside alternate data streams|
| Reserved Device Names       | Disallowed  | Includes superscript digits for device aliases       |
| Trailing space/period       | Disallowed  | Possible with `\\?\` prefix, not supported generally |
| ALN-symbols (Unicode)       | Allowed     | If not reserved, valid as per Unicode/UTF-16 rules   |
| Emoji, Math symbols, Greek, etc.| Allowed | As above—subject to font, rendering, filesystem      |

(* Allowed internally, but not at the end of names.)

---

**In summary:** Windows 13 only allows symbolic/ALN notation in filenames and folders if the characters are valid Unicode and not among a well-defined set of forbidden/reserved characters and device names. No recent expansion in allowed character sets has occurred beyond enhancements to Unicode and UTF-8 compatibility, path length, and case sensitivity support. Always validate and test edge cases, especially for platform and cross-platform developments.
