# derkt - Hermes Bytecode Disassembler

A tool for parsing and disassembling Hermes Bytecode (HBC) files, written in Racket.

## Project Structure

```text
derkt/
├── common/             # Core utilities and macros
│   ├── decode.rkt      # Binary decoding macros (define-binary-struct, etc.)
│   ├── isa.rkt         # ISA definition macro (define-hbc-isa)
│   └── isa-manager.rkt # Dynamic loader for versioned instruction sets
├── model/              # Data structure definitions
│   ├── header.rkt      # HBC Header structures
│   ├── function-header.rkt # Function Header structures
│   └── hbc.rkt         # Main HBC file and table entry structures
├── parser/             # Parsing and disassembly logic
│   ├── core.rkt        # Main parsing orchestration
│   ├── tables.rkt      # Table-specific reading functions
│   ├── functions.rkt   # Function header and bytecode parsing
│   └── debug-info.rkt  # Debug information parsing
├── isa/                # Generated instruction set files
│   └── hermes-instructions-v[VER].rkt
├── def_versions/       # ISA source definitions and generator
│   ├── [VER].def       # Hermes .def files for different versions
│   └── gen-isa.rkt     # Instruction set generator script
├── tests/              # Unit and integration tests
└── main.rkt            # CLI entry point
```

## Core Features

- **Macro-based Decoding**: Uses powerful Racket macros to define binary structures and bitfields with minimal boilerplate.
- **Dynamic ISA Versioning**: Automatically detects the Hermes Bytecode version and loads the corresponding instruction set dynamically.
- **Modular Architecture**: Separated concerns for models, low-level decoding, and high-level parsing logic.
- **Support for Large Bundles**: Optimized to parse and disassemble production bundles with tens of thousands of functions.

## Usage

### Disassemble an HBC File

```powershell
racket main.rkt [path_to_hbc_file]
```

If no path is provided, it defaults to a pre-defined production bundle path. It is recommended to redirect output to a file for large bundles:

```powershell
racket main.rkt my_app.hbc > output.txt
```

### Regenerate Instruction Sets

To generate a Racket instruction set from a Hermes `.def` file:

```powershell
cd def_versions
racket gen-isa.rkt 96.def > ../isa/hermes-instructions-v96.rkt
```

### Run Tests

```powershell
raco test tests/decode-test.rkt
```

## Development

### Adding New Binary Structures

Use the `define-binary-struct` macro in the appropriate model file:

```racket
(define-binary-struct MyStruct
  [field1 UInt32]
  [field2 UInt16])
```

This generates the Racket struct `MyStruct` and its corresponding reader `read-MyStruct`.

### Adding New Bitfields

Use the `define-bitfield-struct` macro:

```racket
(define-bitfield-struct SmallHeader 16
  [offset   0  25]
  [flags    25 7])
```

## License

This project is for educational and research purposes.
