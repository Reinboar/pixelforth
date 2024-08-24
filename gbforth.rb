#!/usr/bin/ruby -- 

require 'optparse'

# Register specification
# r0 = general purpose
# r1 = data stack
# r2 = return stack
# r3 = instruction pointer
# r4 = top of stack

def PREAMBLE(here_offset)
"
.macro PushD
  str r4, [r1]
  sub r1, r1, #0x04
  mov r4, r0
.endm

.macro PopD
  mov r0, r4
  add r1, r1, #0x04
  ldr r4, [r1]
.endm

.macro PeekD
  mov r0, r4
.endm

.macro PushR
  str r0, [r2]
  sub r2, r2, #0x04
.endm

.macro PopR
  add r2, r2, #0x04
  ldr r0, [r2]
.endm

.macro PeekR
  ldr r0, [r2, #0x04]
.endm

.macro GoToNext
  ldr r0, =Next
  bx r0
  .ltorg
.endm

.align 4
.section .text
.global _start
_start:
b InitInterp
.skip 0xE0
InitInterp:
  mov r4, #0x0
  ldr r1, =DataStackTop
  ldr r2, =ReturnStackTop
  ldr r3, =Main
  ldr r5, =HereStart + #{here_offset}
  GoToNext

Next:
  ldr r0, [r3]      @ load word stored at IP
  add r3, r3, #0x04 @ increment IP
  bx r0             @ jump to next word

DoConst:
  ldr r0, [r0, #0x08]
  PushD
  GoToNext

DoCol:
  mov r6, r0
  add r6, r6, #0x04
  mov r0, r3
  PushR
  mov r3, r6
  GoToNext

EndCol:
  PopR
  mov r3, r0
  GoToNext

.section .ewram
.align 4
.skip 0x100
.align 4
DataStackTop:
.skip 0x100
.align 4
ReturnStackTop:
.skip 0x2
.align 4
HereValue:
.skip 0x2
.align 4
HereStart:
.skip 0x2
.align 4
"
end

class ForthDef
  attr_accessor :name, :label, :interpret, :compile
  def initialize(name: "UNNAMED", label: nil, interpret: nil, compile: nil)
    @name = name
    @label = label ? label : name
    @interpret = interpret
    @compile = compile
  end

  def compile_definition
    "#{@label}:#{@interpret}\n" if @interpret
  end

  def execute_and_compile(state)
    state.output(".word #{@label}\n") if @interpret
    @compile.call(state) if @compile
  end
end

def parse_number(token)
  return nil if token.length < 2
  suffix = token[-1]
  number = token[0..-2]
  if suffix == 'h'
    return number
  elsif suffix == 'o'
    return number.to_i(8).to_s(16)
  elsif suffix == 'd'
    return number.to_i(10).to_s(16)
  elsif suffix == 'b'
    return number.to_i(2).to_s(16)
  else
    puts "NOT A VALID NUMBER: #{token}"
    abort(-1)
  end
end

def is_byte (token)
  return false if !token || token.length != 3
  return false if token[0] != '#'
  return false unless token[1..2].match(/[0-9a-f]{2}/i)
  true
end

def is_short (token)
  return false if !token || token.length != 5
  return false if token[0] != '#'
  return false unless token[1..4].match(/[0-9a-f]{4}/i)
  true
end

def is_word(token)
  return false if !token || token.length > 9
end

##
# DEF_TABLE contains all forth words known to the compiler.
DEF_TABLE = {
  "DUP" => ForthDef.new(
    name: "DUP",
    interpret: "
    PeekD
    PushD
    GoToNext
    "
  ),

  "2DUP" => ForthDef.new(
    name: "2DUP",
    label: "TWO_DUP_FORTH",
    interpret: "
    ldr r0, [r1, #0x04]
    PushD
    ldr r0, [r1, #0x04]
    PushD
    GoToNext
    "
  ),

  "DROP" => ForthDef.new(
    name: "DROP",
    interpret: "
    PopD
    GoToNext
    "
  ),

  "2DROP" => ForthDef.new(
    name: "2DROP",
    label: "TWO_DROP_FORTH",
    interpret: "
    PopD
    PopD
    GoToNext
    "
  ),

  "NIP" => ForthDef.new(
    name: "NIP",
    interpret: "
    add r1, r1, #0x04
    GoToNext
    "
  ),

  ">R" => ForthDef.new(
    name: ">R",
    label: "STASH_FORTH",
    interpret: "
    PopD
    PushR
    GoToNext
    "
  ),

  "R>" => ForthDef.new(
    name: "R>",
    label: "FETCH_FORTH",
    interpret: "
    PopR
    PushD
    GoToNext
    "
  ),

  "SWAP" => ForthDef.new(
    name: "SWAP",
    label: "SWAP_FORTH",
    interpret: "
    mov r0, r4
    ldr r4, [r1, #0x04]
    str r0, [r1, #0x04]
    GoToNext
    "
  ),

  "OVER" => ForthDef.new(
    name: "OVER",
    interpret: "
    ldr r0, [r1, #0x04]
    PushD
    GoToNext
    "),

  "YONDER" => ForthDef.new(
    name: "YONDER",
    interpret: "
    ldr r0, [r1, #0x08]
    PushD
    GoToNext
    "
  ),

  "LIT" => ForthDef.new(
    name: "LIT",
    interpret: "
    ldr r0, [r3]
    add r3, r3, #0x04
    PushD
    GoToNext
    ",
    compile: ->(state) { 
      t = state.next_word
      state.output(".word 0x#{t[1..2]}\n") if is_byte(t)
    }
  ),

  # Pushes the character pointed to by '&a'.
  "C@" => ForthDef.new( # ( &a -- c )
    name: "C@",
    label: "LOAD_AT",
    interpret: "
    ldrb r0, [r4]
    mov r4, r0
    GoToNext
    "
  ),

  # Pushes the halfword pointed to by '&a'.
  "H@" => ForthDef.new( # ( &a -- c )
    name: "H@",
    label: "LOAD_AT_16",
    interpret: "
    ldrh r0, [r4]
    mov r4, r0
    GoToNext
    "
  ),

  # Pushes the cell pointed to by '&a'.
  "@" => ForthDef.new( # ( &a -- n )
    name: "@",
    label: "LOAD_AT_32",
    interpret: "
    ldr r0, [r4]
    mov r4, r0
    GoToNext
    "
  ),

  # Stores the character 'c' at the address '&a'.
  "C!" => ForthDef.new( # ( c &a -- )
    name: "C!",
    label: "STORE_AT",
    interpret: "
    ldr r0, [r1, #0x04] @ r0 = character
    strb r0, [r4] @ store character at address (r4)
    add r1, r1, #0x04 @ pop stack by 1 item
    ldr r4, [r1] @ update top of stack
    GoToNext
    "
  ),

  # Stores the character 'c' at the address '&a'.
  "H!" => ForthDef.new( # ( c &a -- )
    name: "H!",
    label: "STORE_AT_16",
    interpret: "
    ldr r0, [r1, #0x04] @ r0 = character
    strh r0, [r4] @ store character at address (r4)
    add r1, r1, #0x04 @ pop stack by 1 item
    ldr r4, [r1] @ update top of stack
    GoToNext
    "
  ),

  # Stores the cell 'n' at the address '&a'.
  "!" => ForthDef.new( # ( n &a -- )
    name: "!",
    label: "STORE_AT_32",
    interpret: "
    ldr r0, [r1, #0x04] @ r0 = character
    str r0, [r4] @ store word at address (r4)
    add r1, r1, #0x04 @ pop stack by 1 item
    ldr r4, [r1] @ update top of stack
    GoToNext
    "
  ),

  # Performs 16-bit addition on the top two cells and pushes the result.
  "+" => ForthDef.new( # ( a b -- a+b )
    name: "+",
    label: "ADD_FORTH_16",
    interpret: "
    PopD
    add r4, r4, r0
    GoToNext
    "
  ),

  # Performs 16-bit subtraction on the top two cells and pushes the result.
  "-" => ForthDef.new( # ( a b -- a-b )
    name: "-",
    label: "SUB_FORTH_16",
    interpret: "
    PopD
    sub r4, r4, r0
    GoToNext
    "
  ),

  # Performs a bitwise AND between two cell operands
  "&" => ForthDef.new( # ( a b -- a&b )
    name: "&",
    label: "BITWISE_AND_FORTH_16",
    interpret: "
    PopD
    and r4, r4, r0
    GoToNext
    "
  ),

  # Performs a bitwise OR between two cell operands
  "|" => ForthDef.new( # ( a b -- a|b )
    name: "|",
    label: "BITWISE_OR_FORTH_16",
    interpret: "
    PopD
    orr r4, r4, r0
    GoToNext
    "
  ),

  # Performs a bitwise XOR between two cell operands
  "^" => ForthDef.new( # ( a b -- a^b )
    name: "^",
    label: "BITWISE_XOR_FORTH_16",
    interpret: "
    PopD
    eor r4, r4, r0
    GoToNext
    "
  ),

  # Performs a bitwise NOT on a cell operand
  "NEG" => ForthDef.new( # ( a -- a ^ FFFFh )
    name: "NEG",
    label: "BITWISE_NOT_FORTH_16",
    interpret: "
    PopD
    neg r4, r4
    GoToNext
    "
  ),
  
  # Shifts `n` to the left by `bits` amount of bits
  "<<" => ForthDef.new( # ( bits n -- n<<bits )
    name: "<<",
    label: "SHIFT_LEFT_FORTH",
    interpret: "
    PopD
    lsl r4, r0, r4
    GoToNext
    "
  ),

  ">>" => ForthDef.new( # ( bits n -- n>>bits )
    name: ">>",
    label: "SHIFT_RIGHT_FORTH",
    interpret: "
    PopD
    lsr r4, r0, r4
    GoToNext
    "
  ),

  # Pushes the address of the HERE pointer variable.
  "HERE" => ForthDef.new(
    name: "HERE",
    interpret: "
    ldr r0, =DoConst
    bx r0
    .word HereValue
    .ltorg
    "
  ),

  # Stores a character to the next position pointed to by HERE.
  "C," => ForthDef.new( # ( c -- )
    name: "C,",
    label: "COMPILE_CHAR",
    interpret: "
    PopD
    strb r0, [r5]
    add r5, r5, #0x01
    GoToNext
    "
  ),

  # Stores the TOS cell to the next position pointed to by HERE.
  "," => ForthDef.new( # ( n -- )
    name: ",",
    label: "COMPILE_CELL",
    interpret: "
    PopD
    str r0, [r5]
    add r5, r5, #0x04
    GoToNext
    "
  ),

  # Converts cells to characters.
  "CELLS" => ForthDef.new( # ( n -- n )
    name: "CELLS",
    interpret: "
    add r4, r4, r4
    GoToNext
    "
  ),

  # Advances the HERE pointer by `n` amount of bytes. Used to allocate memory prior to use.
  "ALLOT" => ForthDef.new( # ( n -- )
    name: "ALLOT",
    interpret: "
    PopD
    add r5, r0, r5
    GoToNext
    "
  ),
  # Compiles a string to ROM and pushes its address at runtime
  '"' => ForthDef.new( # ( -- addr )
    name: '"',
    compile: ->(state) {
      forth_string = ""
      while ( c = state.next_char ) != '"' do
        forth_string += c
      end
      state.output(".word BRANCH\n.word 2f\n1:\n")
      state.output(".asciz \"#{forth_string}\"\n")
      state.output(".align 4\n2:\n.word LIT\n.word 1b\n") # TODO: convert anonymous labels to GAS labels
    }
  ),

  # Begins a code comment. Everything up to and including the nearest ')' is ignored by the compiler.
  "(" => ForthDef.new(
    name: "(",
    compile: -> (state) {
      while state.next_word != ")" do
      end
    }),

  # Begins a single-line comment. Everything up to the next newline character is ignored by the compiler.
  "\\" => ForthDef.new(
    name: "\\",
    compile: -> (state) {
      while state.next_char != "\n" do
      end
    }),

  # Performs an unconditional jump to the address immediately following the instruction.
  "BRANCH" => ForthDef.new(
    name: "BRANCH",
    interpret: "
    ldr r3, [r3]
    GoToNext
    "
  ),

  # Takes an address and transfers execution to it. This works on both execution tokens and quotation addresses.
  "CALL" => ForthDef.new( # ( xt -- ... )
    name: "CALL",
    label: "CALL_FORTH",
    interpret: "
    mov r0, r3
    PushR
    PopD
    mov r3, r0
    GoToNext
    "
  ),

  # Exits a word or quotation early.
  "EXIT" => ForthDef.new( # ( -- )
    name: "EXIT",
    label: "EXIT_FORTH",
    interpret: "
    PopR
    mov r3, r0
    GoToNext
    "
  ),

  # Exits out of a parent word or quotation, from inside a child word or quotation.
  "[EXIT]" => ForthDef.new( # ( -- )
    name: "[EXIT]",
    label: "QUOTE_EXIT_FORTH",
    interpret: "
    PopR
    PopR
    mov r3, r0
    GoToNext
    "
  ),

  # Causes execution to jump to the beginning of the current word just as if you had called it directly.
  "RECURSE" => ForthDef.new(
    name: "RECURSE",
    interpret: "
    ldr r6, [r2]
    ldr r6, [r6, #-0x04]
    add r6, r6, #0x04
    mov r3, r6
    GoToNext

    @ ld hl,sp+0
    @ ld a,[hl+]
    @ ld h,[hl]
    @ ld l,a
    @ dec hl
    @ dec hl
    @ ld a,[hl+]
    @ ld h,[hl]
    @ ld l,a
    @ inc hl
    @ inc hl
    @ inc hl
    @ ld d,h
    @ ld e,l
    @ jp Next
    "
  ),

  # Use this version of RECURSE when inside a quotation, otherwise you will recurse on the quotation
  #   rather than the current word.
  "[RECURSE]" => ForthDef.new(
    name: "[RECURSE]",
    label: "QUOTE_RECURSE_FORTH",
    interpret: "
    ldr r6, [r2]
    ldr r6, [r6, #-0x04]
    add r6, r6, #0x04
    mov r3, r6
    PopR
    GoToNext
    "
  ),

  # Pushes the execution token (ie. address) of the following word.
  "'" => ForthDef.new( # ( -- xt )
    name: "'",
    label: "XTOKEN",
    compile: ->(state) {
      word_name = state.next_word
      word = state.definitions[word_name]
      state.error("Cannot compile XT of '#{word_name}' because it has not been defined.") unless word
      state.output(".word LIT\n.word #{word.label}\n")
    }
  ),

  # Begins the definition of a new word.
  ":" => ForthDef.new( 
    name: ":",
    label: "WORD_START",
    compile: ->(state) {
      old_output = state.output_code
      state.output_code = ""
      word_name = state.next_word
      word_def = "\nb DoCol\n" + raw_compile!(state, ';') + ".word QUOTE_END\n"
      state.definitions[word_name] = ForthDef.new(
        name: word_name,
        label: state.new_label + sanitize_label("_#{word_name}"),
        interpret: word_def
      )
      state.output_code = old_output
    }
  ),

  # Begins the definition of a new inline word. ( Compiles its definition wherever it is called )
  # NOTE: Inline words that generate new temporary labels directly cannot be called more than once.
  #       In general, avoid using `[`, `INCBIN"`, and other similar words in inline words.
  "::" => ForthDef.new(
    name: "::",
    label: "INLINE_WORD_START",
    compile: ->(state) {
      old_output = state.output_code
      state.output_code = ""
      word_name = state.next_word
      word_def = raw_compile!(state, ';')
      state.definitions[word_name] = ForthDef.new(
        name: word_name,
        compile: ->(state) {
          state.output(word_def)
	}
      )
      state.output_code = old_output
    }
  ),

  # Begins a new quotation and pushes its address onto the stack.
  "[" => ForthDef.new(
    name: "[",
    label: "QUOTE_START",
    compile: ->(state) {
      end_quote_label = state.new_label
      state.push(end_quote_label)
      state.output(".word LIT\n.word 1f\n.word BRANCH\n.word #{end_quote_label}\n1:\n")
    }
  ),

  # Finishes the definition of a quotation.
  "]" => ForthDef.new(
    name: "]",
    label: "QUOTE_END",
    compile: ->(state) {
      end_quote_label = state.pop
      state.output("#{end_quote_label}:\n")
    },
    interpret: "
    PopR
    mov r3, r0
    GoToNext
    "
  ),

  "CONSTANT:" => ForthDef.new(
    name: "CONSTANT:",
    compile: ->(state) {
      const_name = state.next_word
      const_val = parse_number(state.next_word)
      state.definitions[const_name] = ForthDef.new(
	name: const_name,
	compile: ->(state) {
          state.output(".word LIT\n.word 0x#{const_val}\n")
	}
      )  
    }
  ),

  "VARIABLE:" => ForthDef.new(
    name: "VARIABLE:",
    compile: ->(state) {
      var_name = state.next_word
      var_offset = state.here_offset
      state.here_offset += 4
      state.definitions[var_name] = ForthDef.new(
        name: var_name,
        compile: ->(state) {
          state.output(".word LIT\n.word HereStart+#{var_offset}\n")
        }
      )
      state.output(".word LIT\n.word HereStart+#{var_offset}\n")
    }
  ),

  # Tests for equality between two cells and pushes the result. #0001 for True, #0000 for False
  "=" => ForthDef.new( # ( a b -- c )
    name: "=",
    label: "EQUALS_FORTH",
    interpret: "
    PopD
    subs r4, r4, r0
    moveq r4, #0x01
    movne r4, #0x00
    GoToNext
    "
  ),

  # Tests if the first byte is greater than the second and pushes the result.
  ">" => ForthDef.new( # ( a b -- c )
    name: ">",
    label: "GREATER_FORTH",
    interpret: "
    PopD
    subs r4, r4, r0
    movgt r4, #0x01
    movle r4, #0x00
    GoToNext
    "
  ),

  # Tests if the first byte is less than the second and pushes the result.
  "<" => ForthDef.new( # ( a b -- c )
    name: "<",
    label: "LESS_FORTH",
    interpret: "
    PopD
    subs r4, r4, r0
    movlt r4, #0x01
    movge r4, #0x00
    GoToNext
    "
  ),

  # ANDs two boolean cells. This is a logical AND, not a bitwise AND.
  "AND" => ForthDef.new( # ( a b -- c )
    name: "AND",
    label: "AND_FORTH",
    interpret: "
    PopD
    orrs r0, r0, #0x0
    movne r0, #0x01
    orrs r4, r4, #0x0
    movne r4, #0x01
    and r4, r4, r0
    GoToNext
    "
  ),

  # ORs two boolean bytes. This is a logical OR, not a bitwise OR.
  "OR" => ForthDef.new( # ( a b -- c )
    name: "OR",
    label: "OR_FORTH",
    interpret: "
    PopD
    orrs r0, r0, #0x0
    movne r0, #0x01
    orrs r4, r4, #0x0
    movne r4, #0x01
    orr r4, r4, r0
    GoToNext
    "
  ),

  # Performs a logical NOT on a boolean byte. This is a logical NOT, not a bitwise NOT.
  "NOT" => ForthDef.new( # ( a -- !a )
    name: "NOT",
    label: "NOT_FORTH",
    interpret: "
    orrs r4, r4, #0x0
    moveq r4, #0x01
    movne r4, #0x00
    GoToNext
    "
  ),
  
  # Takes two quotations and a boolean byte. Calls the first quotation if the byte is True,
  #   calls the second quotation if the byte is False.
  "IF" => ForthDef.new( # ( n &a &b -- ... )
    name: "IF",
    label: "IF_FORTH",
    interpret: "
    mov r0, r3 @ save current IP to return stack
    PushR
    PopD @ r6 = false branch, r0 = true branch, r4 = condition value
    mov r6, r0
    PopD
    movs r4, r4
    moveq r3, r6
    movne r3, r0
    GoToNext
    "
  ),
  
  # Halts the system by creating an infinite loop.
  "PAUSE" => ForthDef.new(
    name: "PAUSE",
    interpret: "
    b PAUSE
    "
  ),

  "UNDEFINE" => ForthDef.new(
    name: "UNDEFINE",
    compile: ->(state) {
      undefine_word = state.next_word
      state.error("Cannot undefine word \"#{undefine_word.upcase}\" since it is not defined.") unless state.definitions[undefine_word]
      state.definitions[state.next_word].delete
    }
  ),

  "INCLUDE\"" => ForthDef.new(
    name: "INCLUDE\"",
    compile: ->(state) {
      filename = ""
      while (c = state.next_char) != '"'
        filename += c
      end
      full_filepath = state.in_include_path?(filename)
      state.error("File could not be found: \"#{filename}\"") unless full_filepath
      include_file_code = File.open(full_filepath).read
      state.code.insert(state.code_index, include_file_code)
    }
  ),

  # Compiles a binary file to ROM and passes the address and length onto the data stack.
  # The path that is searched is dependent on arm-none-eabi-as, so be sure to set its include path accordingly.
  "INCBIN\"" => ForthDef.new( # ( -- len addr )
    name: "INCBIN\"",
    compile: ->(state) {
      filename = ""
      while (c = state.next_char) != '"'
        filename += c
      end
      bin_label = state.new_label
      end_bin_label = state.new_label
      state.output(".word BRANCH\n.word #{end_bin_label}\n#{bin_label}:\n.incbin \"#{filename}\"\n#{end_bin_label}:\n.word LIT\n.word #{end_bin_label} - #{bin_label}\n.word LIT\n.word #{bin_label}\n")
    }
  ),

  "ASM`" => ForthDef.new(
     name: "ASM",
     compile: ->(state) {
       assembly = ""
       while (c = state.next_char) != '`'
         assembly += c
       end
       start_asm_label = state.new_label
       end_asm_label = state.new_label
       state.output(".word BRANCH\n.word #{end_asm_label}\n.align 4\n#{start_asm_label}:\n#{assembly}\n#{end_asm_label}:\n.word #{start_asm_label}\n")

     }
  ),

  "RUBY`" => ForthDef.new(
    name: "RUBY",
    compile: ->(state) {
      ruby_code = ""
      while (c = state.next_char) != '`'
        ruby_code += c
      end
      eval(ruby_code)
    }
  ),

}

class CompilerState
  attr_accessor :compile, :code, :code_index, :output_code, :definitions, :include_paths, :here_offset
  def initialize(code, compile: false, definitions: {})
    @code = code.strip
    @output_code = ""
    @code_index = 0
    @here_offset = 0
    @compile = compile
    @definitions = definitions
    @stack = []
    @label_counter = 0
    @code_line = 1
    @include_paths = ["./"]
  end

  def next_char
    @code_index += 1
    @code_line += 1 if @code[@code_index - 1] == "\n"
    @code[@code_index - 1]
  end

  def next_word
    cur_char = next_char
    while cur_char == " " || cur_char == "\n" || cur_char == "\t"
      cur_char = next_char
      return nil if end_of_buffer
    end
    result_word = ''
    while cur_char != ' ' && cur_char != "\n" && cur_char != "\t" && !end_of_buffer
      result_word += cur_char
      cur_char = next_char
    end
    result_word
  end

  def output(code)
    @output_code += code
  end

  def end_of_buffer
    @code_index > @code.length
  end

  def error(msg)
    abort("Error encountered near line #{@code_line}:\n    #{msg}\n")
  end

  def push(value)
    @stack.push(value)
  end

  def pop
    @stack.pop
  end

  def new_label
    label = "_temp#{@label_counter.to_s.rjust(8,'0')}"
    @label_counter += 1
    label
  end

  def in_include_path?(filename)
    @include_paths.each do |i|
      return i + '/' + filename if File.exist?(i + '/' + filename)
    end
    false
  end
end

def raw_compile!(compiler_state, end_token)
  output_index = compiler_state.output_code.length
  until compiler_state.end_of_buffer
    t = compiler_state.next_word
    break if t == end_token

    if compiler_state.definitions[t]
      compiler_state.definitions[t].execute_and_compile(compiler_state)
    elsif (num = parse_number(t)) != nil
      compiler_state.output(".word LIT\n.word 0x#{num}\n")
    else
      compiler_state.error("'#{t}' is not defined.")
    end
  end
  compiler_state.output_code[output_index..-1]
end

def compile(code, def_table, include_paths)
  state = CompilerState.new(code, compile: false, definitions: def_table)
  state.include_paths = include_paths
  asm_defs = ''
  state.output("\n.align 4\nMain:\n")
  raw_compile!(state, nil)
  def_table.each_value { |d| asm_defs += ".align 4\n" + d.compile_definition if d.interpret }
  state.output(".word PAUSE\n")
  PREAMBLE(state.here_offset) + "\n.align 4\n.section .text\n" + asm_defs + state.output_code
end

def sanitize_label(label)
  label.gsub(/[^A-Z0-9]/i, '_')
end

options = { include: ["./"] }
OptionParser.new do |opts|
  opts.banner = "Usage: gbforth.rb <source> [-I include_path]"

  opts.on("-o", "--output OUTPUT", "Specify the output assembly source file") do |o|
    options[:output] = o
  end

  opts.on("-I", "--include INCLUDE_PATH", "Specify the path to include source files from") do |i|
    options[:include].unshift(i)
  end
end.parse!

if ARGV.length == 1 && File.exist?(ARGV[0])
  options[:output] = File.basename(ARGV[0], ".*") + ".asm" unless options[:output]
  out = compile(File.open(ARGV[0], 'r').read, DEF_TABLE, options[:include])
  File.open(options[:output], 'w').write(out)
  puts 'Compiled succesfully.'
else
  puts 'Invalid argument.'
end

