const binaryOperators = [
  "||",
  "&&",
  choice("==", "!="),
  choice("<", "<=", ">", ">="),
  choice("+", "-"),
  choice("*", "/", "%"),
  "|",
  "^",
  "&",
  choice("<<", ">>"),
];

const PREC = {
  unary: binaryOperators.length + 1,
};

const instructions = [
  "abs",
  "abs.b",
  "abs.h",
  "absdif",
  "absdif.b",
  "absdif.h",
  "absdifs",
  "absdifs.h",
  "abss",
  "abss.h",
  "add",
  "add.a",
  "add.b",
  "add.f",
  "add.h",
  "addc",
  "addi",
  "addih",
  "addih.a",
  "adds",
  "adds.h",
  "adds.hu",
  "adds.u",
  "addsc.a",
  "addsc.at",
  "addx",
  "and",
  "and.and.t",
  "and.andn.t",
  "and.eq",
  "and.ge",
  "and.ge.u",
  "and.lt",
  "and.lt.u",
  "and.ne",
  "and.nor.t",
  "and.or.t",
  "and.t",
  "andn",
  "andn.t",
  "bisr",
  "bmerge",
  "bsplit",
  "cachea.i",
  "cachea.w",
  "cachea.wi",
  "cachei.i",
  "cachei.w",
  "cachei.wi",
  "cadd",
  "caddn",
  "call",
  "calla",
  "calli",
  "clo",
  "clo.h",
  "cls",
  "cls.h",
  "clz",
  "clz.h",
  "cmov",
  "cmovn",
  "cmp.f",
  "cmpswap.w",
  "crc32.b",
  "crc32b.w",
  "crc32l.w",
  "crcn",
  "csub",
  "csubn",
  "debug",
  "dextr",
  "disable",
  "div",
  "div.u",
  "div.f",
  "dsync",
  "dvadj",
  "dvinit",
  "dvinit.b",
  "dvinit.bu",
  "dvinit.h",
  "dvinit.hu",
  "dvinit.u",
  "dvstep",
  "dvstep.u",
  "enable",
  "eq",
  "eq.a",
  "eq.b",
  "eq.h",
  "eq.w",
  "eqany.b",
  "eqany.h",
  "eqz.a",
  "extr",
  "extr.u",
  "fcall",
  "fcalla",
  "fcalli",
  "fret", 
  "ftoi",
  "ftoq31",
  "ftou",
  "ftoiz",
  "ftoq31z",
  "ftouz",
  "ftohp",
  "ge",
  "ge.a",
  "ge.u",
  "hptof",
  "imask",
  "ins.t",
  "insert",
  "insn.t",
  "isync",
  "itof",
  "ixmax",
  "ixmax.u",
  "ixmin",
  "ixmin.u",
  "j",
  "ja",
  "jeq",
  "jeq.a",
  "jge",
  "jge.u", 
  "jgez",
  "jgtz",
  "ji",
  "jl",
  "jla",
  "jlez",
  "jli", 
  "jlt",
  "jlt.u",
  "jltz",
  "jne",
  "jne.a",
  "jned",
  "jnei",
  "jnz",
  "jnz.a",
  "jnz.t",
  "jz",
  "jz.a",
  "jz.t",
  "ld.a",
  "ld.b",
  "ld.bu",
  "ld.d",
  "ld.da",
  "ld.h",
  "ld.hu",
  "ld.q",
  "ld.w",
  "ldlcx",
  "ldmst",
  "lducx",
  "lea",
  "lha",
  "loop",
  "loopu",
  "lt",
  "lt.a",
  "lt.b",
  "lt.bu",
  "lt.h",
  "lt.hu",
  "lt.u",
  "lt.w",
  "lt.wu",
  "madd",
  "madd.f",
  "madd.h",
  "madd.q",
  "madd.u",
  "maddm.h",
  "maddms.h",
  "maddr.h",
  "maddr.q",
  "maddrs.h",
  "maddrs.q",
  "madds",
  "madds.h",
  "madds.q",
  "madds.u",
  "maddsu.h",
  "maddsum.h",
  "maddsums.h",
  "maddsur.h",
  "maddsurs.h",
  "maddsus.h",
  "max",
  "max.b",
  "max.bu",
  "max.h",
  "max.hu",
  "max.u",
  "mfcr",
  "min",
  "min.b",
  "min.bu",
  "min.h",
  "min.hu",
  "min.u",
  "mov",
  "mov.a",
  "mov.aa",
  "mov.d",
  "mov.u",
  "movh",
  "movh.a",
  "msub",
  "msub.f",
  "msub.h",
  "msub.q",
  "msub.u",
  "msubad.h",
  "msubadm.h",
  "msubadms.h",
  "msubadr.h",
  "msubadrs.h",
  "msubads.h",
  "msubm.h",
  "msubms.h",
  "msubr.h",
  "msubr.q",
  "msubrs.h",
  "msubrs.q",
  "msubs",
  "msubs.h",
  "msubs.q",
  "msubs.u",
  "mtcr",
  "mul",
  "mul.f",
  "mul.h",
  "mul.q",
  "mul.u",
  "mulm.h",
  "mulms.h",
  "mulr.h",
  "mulr.q",
  "muls",
  "muls.u",
  "nand",
  "nand.t",
  "ne",
  "ne.a",
  "nez.a",
  "nop",
  "nor",
  "nor.t",
  "not",
  "or",
  "or.and.t",
  "or.andn.t",
  "or.eq",
  "or.ge",
  "or.ge.u",
  "or.lt",
  "or.lt.u",
  "or.ne",
  "or.nor.t",
  "or.or.t",
  "or.t",
  "orn",
  "orn.t",
  "pack",
  "parity", 
  "popcnt.w",
  "q31tof",
  "qseed.f",
  "restore",
  "ret",
  "rfe",
  "rfm",
  "rslcx",
  "rstv",
  "rsub",
  "rsubs",
  "rsubs.u",
  "sat.b", 
  "sat.bu",
  "sat.h",
  "sat.hu",
  "sel",
  "seln",
  "sh",
  "sh.and.t",
  "sh.andn.t",
  "sh.eq",
  "sh.ge",
  "sh.ge.u",
  "sh.h",
  "sh.lt",
  "sh.lt.u",
  "sh.nand.t",
  "sh.ne",
  "sh.nor.t",
  "sh.or.t",
  "sh.orn.t",
  "sh.xnor.t",
  "sh.xor.t",
  "sha", 
  "sha.h",
  "shas",
  "shuffle",
  "st.a", 
  "st.b", 
  "st.d",
  "st.da",
  "st.h", 
  "st.q",
  "st.t",
  "st.w",
  "stlcx",
  "stucx",
  "sub",
  "sub.a",
  "sub.b",
  "sub.f",
  "sub.h",
  "subc",
  "subs",
  "subs.h",
  "subs.hu",
  "subs.u",
  "subx",
  "svlcx",
  "swap.w",
  "swapmsk.w",
  "syscall",
  "trapsv",
  "trapv",
  "unpack",
  "updfl",
  "utof",
  "wait",
  "xnor",
  "xnor.t",
  "xor",
  "xor.eq",
  "xor.ge",
  "xor.ge.u",
  "xor.lt",
  "xor.lt.u",
  "xor.ne",
  "xor.t",
]

const builtIn_functions = [
  "abs",
  "acs",
  "asn",
  "at2",
  "atn",
  "cel",
  "coh",
  "cos",
  "flr",
  "l10",
  "log",
  "max",
  "min",
  "pow",
  "rnd",
  "sgn",
  "sin",
  "snh",
  "sqt",
  "tan",
  "tnh",
  "xpn",
  "cvf",
  "cvi",
  "fld",
  "fract",
  "sfract",
  "lng",
  "lun",
  "rvb",
  "unf",
  "cat",
  "len",
  "pos",
  "scp",
  "sub",
  "arg",
  "cnt",
  "mac",
  "mxp",
  "hi",
  "his",
  "lo",
  "los",
  "lsb",
  "msb",
  "astc",
  "def",
  "exp",
  "lnt",
  "lst",
];

module.exports = grammar({
  name: "tricore",

  word: ($) => $._symbol_chars,

  conflicts: ($) => [
    [$._numeric_literal],
    [$.instruction],
    [$._label_colon, $._start_line],
    [$.binary_expression]
  ],

  extras: () => [],

  rules: {
    source_file: ($) => 
      optional(
        seq(
          optional($._nl), 
          repeat(seq($._element, $._nl)), 
          optional(choice($._element, $._nl, $._ws))
        )
      ),

    _element: ($) =>
      choice(
        $._statement,
        $.control,
        $._standalone_label,
        $._standalone_comment,
      ),

    _statement: ($) => 
      seq(
        $._start_line,
        choice(
          $.instruction,
          $._directive,
          $.macro_call
        ),
        optional(alias($._end_line, $.comment))
      ),

    _standalone_label: ($) => seq($._label, optional($.comment)),

    _standalone_comment: ($) => prec(-1, seq(optional($._ws), $.comment)),

    _start_line: ($) => choice($._label, $._ws),

    //----------------------------------------------------------------------
    // Labels:
    //----------------------------------------------------------------------

    _label: ($) =>
      choice(alias($._label_definition, $.label), $._local_label),

    _label_definition: ($) => 
      seq(choice($._label_colon, $._name), optional($._ws)),

    _name: ($) => field("name", $._identifier),

    _label_colon: ($) => 
      seq(
        optional($._ws), 
        $._name, 
        ":"
      ),

    _local_label: ($) => seq(optional($._ws), alias(/\d+/, $.local_label), ":", optional($._ws)),

    //----------------------------------------------------------------------
    // Comments:
    //----------------------------------------------------------------------

    comment: () => /;[^\n\r]*/,

    _end_line: () => /[ \t]*;[^\n\r]*/,

    //----------------------------------------------------------------------
    // Instructions
    //----------------------------------------------------------------------

    instruction: ($) =>
      seq(
        field("mnemonic", $._instruction_mnemonic),
        optional(field("operands", $.operand_list))
      ),

    _instruction_mnemonic: ($) => 
      alias(mnemonicChoice(instructions), $.instruction_mnemonic),

    operand_list: ($) => seq($._ws, commaSep($._operand, $._sep)),

    _operand: ($) => 
      choice($._address, $._register, seq(optional("#"), $._expression)),

    //----------------------------------------------------------------------
    // Directives:
    //----------------------------------------------------------------------

    _directive: ($) => 
      choice(
        $.directive_comment,
        $.directive_end,
        $.directive_include,
        $.directive_message,
        $.directive_alias,
        $.directive_equ,
        $.directive_declare_symbol,
        $.directive_import_symbol,
        $.directive_org,
        $.directive_sdecl,
        $.directive_sect,
        $.directive_set,
        $.directive_size,
        $.directive_type,
        $.directive_data_definition,
        $.directive_define,
        $.directive_dup,
        $.directive_dupa,
        $.directive_dupc,
        $.directive_dupf,
        $.directive_conditional,
        $.directive_exitm,
        $.directive_macro,
        $.directive_pmacro,
        $.directive_undef,
        $.directive_calls,
        $.directive_hll
      ),

    directive_comment: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".comment"]), $.directive_mnemonic)),
        $._ws,
        field("operands", $.string_literal)
      ),

    directive_end: ($) =>
      field("mnemonic", alias(mnemonicChoice([".end"]), $.directive_mnemonic)),

    directive_include: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".include"]), $.directive_mnemonic)),
        $._ws,
        field("operands", choice($.string_literal, alias(/<[^"'\s,<>]+>/, $.filename)))
      ),

    directive_message: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".fail", ".message", ".warning"]), $.directive_mnemonic)),
        $._ws,
        field("operands", commaSep($._expression, $._sep))
      ),

    directive_alias: ($) =>
      seq(
        field("left", $._identifier),
        $._ws,
        field("mnemonic", alias(mnemonicChoice([".alias"]), $.directive_mnemonic)),
        $._ws,
        field("right", $._identifier)
      ),

    directive_equ: ($) =>
      seq(
        field("left", $._identifier),
        $._ws,
        field("mnemonic", alias(mnemonicChoice([".equ"]), $.directive_mnemonic)),
        $._ws,
        field("right", $._expression)
      ),

    directive_declare_symbol: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".global", ".local"]), $.directive_mnemonic)),
        $._ws,
        field("operands", commaSep($._identifier, $._sep))
      ),

    directive_import_symbol: ($) =>
      seq(
        field("mnemonic", alias(mnemonicChoice([".extern", ".weak"]), $.directive_mnemonic)),
        $._ws,
        field("operands", commaSep($._identifier, $._sep))
      ),

    directive_org: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".org"]), $.directive_mnemonic)),
        optional($._ws),
        field(
          "operands", 
          commaSep(
            optional(
              choice(
                $._expression,
                seq(/(cluster|group)\(/, alias($.string_literal, $.cluster), /\)/)
              )
            ), 
            $._sep
          )
        )
      ),

    directive_sdecl: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".sdecl"]), $.directive_mnemonic)),
        $._ws,
        field("operands", commaSep(
          choice(
            $._expression,
            seq(/cluster\(/, alias($.string_literal, $.cluster), /\)/)
          ),
          $._sep
        )),
        optional(field("operands", seq(/ at /, $._expression)))
      ),

    directive_sect: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".sect"]), $.directive_mnemonic)),
        $._ws,
        field("operands", commaSep($._expression, $._sep))
      ),
    
    directive_set: ($) =>
      choice(
        seq(
          field("left", $._identifier),
          $._ws,
          field("mnemonic", alias(mnemonicChoice([".set"]), $.directive_mnemonic)),
          $._ws,
          field("right", $._expression)
        ),
        seq(
          field("mnemonic", alias(mnemonicChoice([".set"]), $.directive_mnemonic)),
          $._ws,
          field("left", $._identifier),
          $._ws,
          field("right", $._expression)
        ),
      ),

    directive_size: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".size"]), $.directive_mnemonic)),
        $._ws,
        field("operands", seq($._identifier, $._sep, $._expression))
      ),

    directive_type: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".type"]), $.directive_mnemonic)),
        $._ws,
        field("type", alias(mnemonicChoice(["func", "object", "file"]), $.label_type))
      ),

    directive_data_definition: ($) =>
      seq(
        field(
          "mnemonic", 
          alias(
            mnemonicChoice(
              [".accum", ".align", ".ascii", ".asciiz", ".byte", ".double", ".float", ".fract", ".half", ".sfract", ".space", ".word"]
            ), 
            $.directive_mnemonic)
          ),
        $._ws,
        field("operands", commaSep(optional($._expression), $._sep))
      ),

    directive_define: ($) =>
      seq(
        field("mnemonic", alias(mnemonicChoice([".define"]), $.directive_mnemonic)),
        $._ws,
        field("left", $._identifier),
        $._ws,
        field("right", $.string_literal)
      ),

    directive_dup: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".dup"]), $.directive_mnemonic)),
        $._ws,
        field("count", $._expression),
        optional($._end_line),
        $._nl,
        field("body", alias(repeat(seq($._element, $._nl)), $.element_list)),
        $._start_line,
        field("mnemonic", alias(mnemonicChoice([".endm"]), $.directive_mnemonic)),
      ),

    directive_dupa: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".dupa"]), $.directive_mnemonic)),
        $._ws,
        field("formal_arg", $._identifier),
        $._sep,
        field("argument", commaSep(optional($._expression), $._sep)),
        optional($._end_line),
        $._nl,
        field("body", alias(repeat(seq($._element, $._nl)), $.element_list)),
        $._start_line,
        field("mnemonic", alias(mnemonicChoice([".endm"]), $.directive_mnemonic)),
      ),

    directive_dupc: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".dupc"]), $.directive_mnemonic)),
        $._ws,
        field("formal_arg", $._identifier),
        $._sep,
        field("argument", $.string_literal),
        optional($._end_line),
        $._nl,
        field("body", alias(repeat(seq($._element, $._nl)), $.element_list)),
        $._start_line,
        field("mnemonic", alias(mnemonicChoice([".endm"]), $.directive_mnemonic)),
      ),

    directive_dupf: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".dupf"]), $.directive_mnemonic)),
        $._ws,
        field("formal_arg", $._identifier),
        $._sep,
        field("argument", commaSep($._expression, $._sep)),
        optional($._end_line),
        $._nl,
        field("body", alias(repeat(seq($._element, $._nl)), $.element_list)),
        $._start_line,
        field("mnemonic", alias(mnemonicChoice([".endm"]), $.directive_mnemonic)),
      ),

    directive_conditional: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".if"]), $.directive_mnemonic)),
        $._ws,
        field("test", $._expression),
        optional($._end_line),
        $._nl,
        field("body", alias(repeat(seq($._element, $._nl)), $.element_list)),
        repeat(
          prec.left(
            seq(
              $._start_line,
              field("mnemonic", alias(mnemonicChoice([".elif"]), $.directive_mnemonic)),
              $._ws,
              field("test", $._expression),
              optional($._end_line),
              $._nl,
              field("body", alias(repeat1(seq($._element, $._nl)), $.element_list)),
            )
          )
        ),
        optional(
          seq(
            $._start_line,
            field("mnemonic", alias(mnemonicChoice([".else"]), $.directive_mnemonic)),
            optional($._end_line),
            $._nl,
            field("body", alias(repeat(seq($._element, $._nl)), $.element_list)),
          )
        ),
        $._start_line,
        field("mnemonic", alias(mnemonicChoice([".endif"]), $.directive_mnemonic)),
      ),

    directive_exitm: ($) => 
      field("mnemonic", alias(mnemonicChoice([".exitm"]), $.directive_mnemonic)),

    directive_macro: ($) => 
      seq(
        field("left", $._identifier),
        $._ws,
        field("mnemonic", alias(mnemonicChoice([".macro"]), $.directive_mnemonic)),
        $._ws,
        field("right", commaSep($._identifier, $._sep)),
        optional($._end_line),
        $._nl,
        field("body", alias(repeat(seq($._element, $._nl)), $.element_list)),
        $._start_line,
        field("mnemonic", alias(mnemonicChoice([".endm"]), $.directive_mnemonic)),
      ),

    directive_pmacro: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".pmacro"]), $.directive_mnemonic)),
        $._ws,
        field("operands", commaSep($._identifier, $._sep))
      ),
    
    directive_undef: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".undef"]), $.directive_mnemonic)),
        $._ws,
        field("operands", $._identifier)
      ),

    directive_calls: ($) => 
      seq(
        field("mnemonic", alias(mnemonicChoice([".calls"]), $.directive_mnemonic)),
        $._ws,
        field("operands", commaSep(optional($._expression), $._sep))
      ),

    directive_hll: ($) =>
      seq(
        field(
          "mnemonic", 
          alias(
            mnemonicChoice(
              [".compiler_invocation", ".compiler_name", ".compiler_version", ".misrac"]
            ), 
            $.directive_mnemonic
          )
        ),
        $._ws,
        $.string_literal
      ),
    
    control: ($) => 
      seq(
        optional($._ws),
        choice(
          seq(
            alias(
              mnemonicChoice(
                [
                  "$list", "$page", "$prctl", "$stitle", "$title", "$case", "$debug", "$hw_only", "$ident", 
                  "$mmu", "$no_fpu", "$object", "$tc131", "$tc16", "$tc16x", "$tc162", "$tc18"
                ]
              ),
              $.control_mnemonic
            ),
            optional($._ws),
            optional(commaSep($._expression, $._sep)),
          ),
          seq(alias(mnemonicChoice(["$warning"]), $.control_mnemonic), $._ws, mnemonicChoice(["off"]), $._ws, $.decimal_integer),
          seq(alias(/\$[cC][pP][uU]_[tT][cC]\d+/, $.control_mnemonic), $._ws, mnemonicChoice(["off", "on"]))
        ),
        optional($._end_line)
      ),


    //----------------------------------------------------------------------
    // Macro calls:
    //----------------------------------------------------------------------

    macro_call: ($) => 
      prec.right(
        seq(
          field("macro", $._identifier),
          optional(field("operands", $.operand_list))
        )
      ),

    // Address:

    _address: ($) => 
      choice(
        $.base_offset,
        $.pre_increment,
        $.post_increment,
        $.circular,
        $.bit_reverse
      ),

    base_offset: ($) => 
      seq(
        /\[[ \t]*/,
        choice(
          $.address_register, 
          alias($.interpolated, $.register_macro_argument)
        ),
        /[ \t]*\][ \t]*/, 
        optional(field("offset", $._expression))
      ),

    pre_increment: ($) => 
      seq(
        /\[[ \t]*\+[ \t]*/,
        choice(
          $.address_register, 
          alias($.interpolated, $.register_macro_argument)
        ),
        /[ \t]*\][ \t]*/, 
        optional(field("offset", $._expression))
      ),

    post_increment: ($) => 
      seq(
        /\[[ \t]*/,
        choice(
          $.address_register, 
          alias($.interpolated, $.register_macro_argument)
        ),
        /[ \t]*\+[ \t]*\][ \t]*/,
        optional(field("offset", $._expression))
      ),

    circular: ($) => 
      seq(
        /\[[ \t]*/,
        choice(
          $.extended_address_register, 
          alias($.interpolated, $.register_macro_argument)
        ),
        /[ \t]*\+[ \t]*c[ \t]*\][ \t]*/,
        optional(field("offset", $._expression))
      ),

    bit_reverse: ($) => 
      seq(
        /\[[ \t]*/,
        choice(
          $.extended_address_register, 
          alias($.interpolated, $.register_macro_argument)
        ),
        /[ \t]*\+[ \t]*r[ \t]*\]/,
      ),

    // Register:

    _register: ($) =>
      choice(
        $.data_register,
        $.data_register_bit,
        $.address_register,
        $.extended_data_register,
        $.extended_address_register,
      ),

    data_register: () => 
      /[dD](?:[0-9]|1[0-5])([lLuU]{1,2})?/,
    data_register_bit: ($) =>
      seq(
        $.data_register,
        optional($._ws),
        ":",
        optional($._ws),
        alias(/\d+/, $.bit_position)
      ),
    address_register: () => /[aA](?:[0-9]|1[0-5])|sp/,
    extended_data_register: () => /[eE](?:[02468]|1[024])|[dD](?:[02468]|1[024])\s*\/\s*[dD](?:[13579]|1[135])/,
    extended_address_register: () => /[aA](?:[02468]|1[024])\s*\/\s*[aA](?:[13579]|1[135])/,

    // Expressions:

    _expression: ($) => 
      choice(
        $._numeric_literal,
        $._string,
        $.unary_expression,
        $.binary_expression,
        $.parenthesized_expression,
        $._identifier,
        $.function_call,
        alias($._local_label_ref, $.symbol)
      ),
    
    _local_label_ref: ($) =>
      seq(alias(/\d+/, $.local_label), /[pPnN]/),

    _numeric_literal: ($) =>
      choice(
        $.binary,
        $.hexadecimal,
        seq(optional("#"), $.decimal_integer),
        $.decimal_float
      ),

    binary: () => /0[bB][01]+/,
    hexadecimal: () => /0[xX][0-9a-fA-F]+/,
    decimal_integer: () => /\d+/,
    decimal_float: () => token(/\d+((\.\d+([eE][+-]?\d+)?)|[eE][+-]?\d+)|\.\d+/),

    _string: ($) =>
      prec(
        2,
        choice(
          $.string_literal,
          $.string_concate,
          $.substring
        ),
      ),

    string_literal: ($) =>
      choice(
        // Single quoted
        seq(
          "'",
          repeat(
            choice(
              prec(1, /[^'\n\\]+/), // Normal chars
              "''", // Repeat quote escape
              $._string_escape_codes
            )
          ),
          "'"
        ),
        // Double quoted
        seq(
          '"',
          repeat(choice(prec(1, /[^"\n\\]+/), '""', $._string_escape_codes)),
          '"'
        ),
        $.string_concate,
        $.substring
      ),

    string_concate: ($) => 
      prec.left(
        seq(
          $._string,
          /[ \t]*\+\+[ \t]*/,
          $._string,
        ),
      ),

    substring: ($) => 
      seq(
        /\[[ \t]*/,
        $._string,
        /[ \t]*,[ \t]*/,
        alias($.decimal_integer, $.start_pos),
        /[ \t]*,[ \t]*/,
        alias($.decimal_integer, $.length),
        /[ \t]*\]/,
      ),

    _string_escape_codes: ($) =>
      choice(
        /\\[\\bfnrt"'e]/, // Single char escape codes
        /\\[0-7]+/, // Octal character code
        /\\[xX][0-9a-f]+/, // Hex charcter code
      ),

    unary_expression: ($) => 
    prec(
      PREC.unary,
      seq(
        field("operator", alias(choice("+", "-", "!", "~"), $.operator)),
        field("operand", $._expression)
      )
    ),

    binary_expression: ($) =>
      choice(
        ...binaryOperators.map((operator, i) => 
          prec.left(
            i,
            seq(
              field("left", $._expression),
              field("operator", seq(optional($._ws), alias(operator, $.operator), optional($._ws))),
              field("right", $._expression)
            )
          ))
      ),

    parenthesized_expression: ($) =>
      choice(
        seq(/\([ \t]*/, $._expression, /\)[ \t]*/)
      ),

    // Identifiers

    _identifier: ($) =>
      choice($.symbol, $.interpolated),

    // symbol: () => {
    //   const symbolChars = /[a-zA-Z0-9._]/;
    //   const symbolStartChars = /[a-zA-Z._]/;
    //   return token(prec.right(seq(symbolStartChars, repeat(symbolChars))));
    // },

    symbol: ($) => 
      prec.right(
        $._symbol_chars
      ),

    _symbol_chars: () => /[a-zA-Z0-9._]+/,

    macro_arg: () => 
      token(
        choice(
          /[\\?%^][a-zA-Z._][a-zA-Z0-9._]*/,
          /\\[?%^][a-zA-Z._][a-zA-Z0-9._]*/,
        )
      ),

    interpolated: ($) => 
      prec.right(
        1,
        seq(
          choice(
            seq($.macro_arg, $._symbol_chars),
            seq($._symbol_chars, $.macro_arg),
            $.macro_arg
          ),
          repeat(choice($.macro_arg, $._symbol_chars)),
        ),
      ),

    function_call: ($) => 
      seq(
        "@",
        alias(mnemonicChoice(builtIn_functions), $.builtIn_function),
        /\([ \t]*/,
        optional($.expression_list),
        /[ \t]*\)/
      ),

    expression_list: ($) => commaSep($._expression, $._sep),

    //----------------------------------------------------------------------
    // Misc:
    //----------------------------------------------------------------------

    _sep: () => /[ \t]*,[ \t]*/,
    _ws: () => /[ \t]+/,
    _nl: () => /([ \t]*(\r\n|\n|\r))+/,
  }
});

function commaSep(rule, sep) {
  return seq(rule, repeat(seq(sep, rule)));
}

function toCaseInsensitive(a) {
  let ca = a.charCodeAt(0);
  if (ca >= 97 && ca <= 122) return `[${a}${a.toUpperCase()}]`;
  if (ca >= 65 && ca <= 90) return `[${a.toLowerCase()}${a}]`;
  return `[${a}]`;
}

function caseInsensitive(keyword) {
  return new RegExp(keyword.split("").map(toCaseInsensitive).join(""));
}

function mnemonicChoice(options) {
  return choice(...options.map(caseInsensitive));
}