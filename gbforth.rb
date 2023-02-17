#!/usr/bin/ruby -- 

require 'optparse'

# Register specification
# HL = general purpose / 16bit argument
# SP = return stack pointer
# A  = general purpose / 8bit argument
# DE = IP
# BC = data stack pointer

PREAMBLE = 
"
INCLUDE \"hardware.inc\"
SECTION \"Memory\",WRAM0
  ds $100
DataStackTop:
  ds $100
ReturnStackTop:
  ds $02
HereValue:
  ds $02
HereStart:
  ds $02

SECTION \"Forth\", ROM0[$100]
  jp InitInterp
  ds $150 - @,0
InitInterp:
  ld de,Main
  ld bc,DataStackTop
  ld sp,ReturnStackTop
  ld hl,HereValue
  ld a,LOW(HereStart)
  ld [hl+],a
  ld a,HIGH(HereStart)
  ld [hl],a
  jp Next

DEF LastWord = $FFFF

MACRO PushD
  ld [bc],a
  dec bc
ENDM

MACRO PopD
  inc bc
  ld a,[bc]
ENDM

MACRO PushD16
  ld a,l
  ld [bc],a
  dec bc
  ld a,h
  ld [bc],a
  dec bc
ENDM

MACRO PopD16
  inc bc
  ld a,[bc]
  ld h,a
  inc bc
  ld a,[bc]
  ld l,a
ENDM

MACRO PeekD
  inc bc
  ld a,[bc]
  dec bc
ENDM

MACRO PeekD16
  inc bc
  ld a, [bc]
  ld h, a
  inc bc
  ld a, [bc]
  ld l, a
  dec bc
  dec bc
ENDM

MACRO PushR
  add sp,-1
  ld hl,sp+0
  ld [hl],a
ENDM

MACRO PopR
  ld hl,sp+0
  ld a,[hl]
  add sp,1
ENDM

MACRO PeekR
  ld hl,sp
  inc hl
  ld a,[hl]
ENDM

MACRO WordDef
  :
  DEF CurWord = :-
  DB \\1, 0
  DW LastWord
  DEF LastWord = CurWord
ENDM

MACRO LoadInstr
  ld h,d
  ld l,e
  push de
  ld d,[hl+]
  ld e,[hl]
  ld h,d
  ld l,e
  pop de
ENDM

Next:
  ld h,d
  ld l,e
  inc de
  inc de
  push de
  ld a,[hl+]
  ld e,a
  ld d,[hl]
  ld h,d
  ld l,e
  pop de
  jp hl

DoConst8:
  inc hl
  inc hl
  inc hl
  ld a,[hl]
  PushD16
  jp Next

DoConst16:
  inc hl
  inc hl
  inc hl
  ld a,[hl+]
  ld h,[hl]
  ld l,a
  PushD16
  jp Next

DoVar:
  inc hl
  inc hl
  inc hl
  PushD16
  jp Next

DoCol:
  inc hl
  inc hl
  inc hl
  push de
  ld d,h
  ld e,l
  jp Next

EndCol:
  pop de
  jp Next

"

class ForthDef
  attr_accessor :name, :label, :interpret, :compile
  def initialize(name: "UNNAMED", label: nil, interpret: nil, compile: nil)
    @name = name
    @label = label ? label : name
    @interpret = interpret
    @compile = compile
  end

  def compile_definition
    "  WordDef \"#{@name}\"
    #{@label}:#{@interpret}\n" if @interpret
  end

  def execute_and_compile(state)
    state.output("DW #{@label}\n") if @interpret
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
    puts "NOT A VALID NUMBER"
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

##
# DEF_TABLE contains all forth words known to the compiler.
DEF_TABLE = {
  "DUP" => ForthDef.new(
    name: "DUP",
    interpret: "
    PeekD16
    PushD16
    jp Next
    "
  ),

  "2DUP" => ForthDef.new(
    name: "2DUP",
    label: "TWO_DUP_FORTH",
    interpret: "
    jp DoCol
    DW OVER
    DW OVER
    DW QUOTE_END
    "
  ),

  "DROP" => ForthDef.new(
    name: "DROP",
    interpret: "
    PopD16
    jp Next
    "
  ),

  "2DROP" => ForthDef.new(
    name: "2DROP",
    label: "TWO_DROP_FORTH",
    interpret: "
    jp DoCol
    DW DROP
    DW DROP
    DW QUOTE_END
    "
  ),

  "NIP" => ForthDef.new(
    name: "NIP",
    interpret: "
    PopD16
    inc bc
    inc bc
    PushD16
    jp Next
    "
  ),

  ">R" => ForthDef.new(
    name: ">R",
    label: "STASH_FORTH",
    interpret: "
    PopD16
    push hl
    jp Next
    "
  ),

  "R>" => ForthDef.new(
    name: "R>",
    label: "FETCH_FORTH",
    interpret: "
    pop hl
    PushD16
    jp Next
    "
  ),

  "SWAP" => ForthDef.new(
    name: "SWAP",
    label: "SWAP_FORTH",
    interpret: "
    PopD16
    push de
    ld d, h
    ld e, l
    PopD16
    ld a, d
    ld d, h
    ld h, a
    ld a, e
    ld e, l
    ld l, a
    PushD16
    ld h, d
    ld l, e
    PushD16
    pop de
    jp Next
    "
  ),

  "OVER" => ForthDef.new(
    name: "OVER",
    interpret: "
    inc bc
    inc bc
    inc bc
    inc bc
    ld a, [bc]
    ld l, a
    dec bc
    ld a, [bc]
    ld h, a
    dec bc
    dec bc
    dec bc
    PushD16
    jp Next
    "),

  "YONDER" => ForthDef.new(
    name: "YONDER",
    interpret: "
    inc bc
    inc bc
    inc bc
    inc bc
    inc bc
    inc bc
    ld a, [bc]
    ld l, a
    dec bc
    ld a, [bc]
    ld h, a
    dec bc
    dec bc
    dec bc
    dec bc
    dec bc
    PushD16
    jp Next
    "
  ),

  "LIT" => ForthDef.new(
    name: "LIT",
    interpret: "
    ld h,d
    ld l,e
    ld a,[hl+]
    PushD
    ld d,h
    ld e,l
    jp Next
    ",
    compile: ->(state) { 
      t = state.next_word
      state.output("DB $#{t[1..2]}\n") if is_byte(t)
    }
  ),

  "LIT2" => ForthDef.new(
    name: "LIT2",
    interpret: "
    ld h,d
    ld l,e
    ld a,[hl+]
    ld d,[hl]
    inc hl
    ld e,a
    ld a,e
    PushD
    ld a,d
    PushD
    ld d,h
    ld e,l
    jp Next
    ",
    compile: ->(state) {
      t = state.next_word
      state.output("DW $#{t[1..4]}\n") if is_short(t)
    }
  ),

  # Pushes the character pointed to by '&a'.
  "C@" => ForthDef.new( # ( &a -- c )
    name: "C@",
    label: "LOAD_AT",
    interpret: "
    PopD16
    ld a,[hl]
    PushD
    jp Next
    "
  ),

  # Pushes the cell pointed to by '&a'.
  "@" => ForthDef.new( # ( &a -- n )
    name: "@",
    label: "LOAD_AT_16",
    interpret: "
    PopD16
    ld a,[hl+]
    ld h,[hl]
    ld l,a
    PushD16
    jp Next
    "
  ),

  # Stores the character 'c' at the address '&a'.
  "C!" => ForthDef.new( # ( c &a -- )
    name: "C!",
    label: "STORE_AT",
    interpret: "
    PopD16
    PopD
    ld [hl],a
    jp Next
    "
  ),

  # Stores the cell 'n' at the address '&a'.
  "!" => ForthDef.new( # ( n &a -- )
    name: "!",
    label: "STORE_AT_16",
    interpret: "
    PopD16
    inc hl
    PopD
    ld [hl-],a
    PopD
    ld [hl],a
    jp Next
    "
  ),

  # Performs 16-bit addition on the top two cells and pushes the result.
  "+" => ForthDef.new( # ( a b -- a+b )
    name: "+",
    label: "ADD_FORTH_16",
    interpret: "
    PopD16
    push de
    ld d,h
    ld e,l
    PopD16
    add hl,de
    PushD16
    pop de
    jp Next
    "
  ),

  # Performs 16-bit subtraction on the top two cells and pushes the result.
  "-" => ForthDef.new( # ( a b -- a-b )
    name: "-",
    label: "SUB_FORTH_16",
    interpret: "
    PopD16
    push de
    ld e,l
    ld d,h
    PopD16
    ld a,l
    sub e
    ld l,a
    ld a,h
    sbc d
    ld h,a
    PushD16
    pop de
    jp Next
    "
  ),

  # Pushes the address of the HERE pointer variable.
  "HERE" => ForthDef.new(
    name: "HERE",
    interpret: "
    jp DoConst16
    DW HereValue
    "
  ),

  # Stores a character to the next position pointed to by HERE.
  "C," => ForthDef.new( # ( c -- )
    name: "C,",
    label: "COMPILE_CHAR",
    interpret: "
    ld hl,HereValue
    ld a,[hl+]
    ld h,[hl]
    ld l,a
    PopD
    ld [hl],a
    inc hl
    push de
    ld d,h
    ld e,l
    ld hl,HereValue
    ld [hl],e
    inc hl
    ld [hl],d
    pop de
    jp Next
    "
  ),

  # Stores the TOS cell to the next position pointed to by HERE.
  "," => ForthDef.new( # ( n -- )
    name: ",",
    label: "COMPILE_CELL",
    interpret: "
    jp DoCol
    DW COMPILE_CHAR
    DW COMPILE_CHAR
    DW QUOTE_END
    "
  ),

  # Converts cells to characters.
  "CELLS" => ForthDef.new( # ( n -- n )
    name: "CELLS",
    interpret: "
    PopD16
    add hl,hl
    PushD16
    jp Next
    "
  ),

  # Advances the HERE pointer by the cell on TOS. Used to allocate memory prior to use.
  "ALLOT" => ForthDef.new( # ( n -- )
    name: "ALLOT",
    interpret: "
    jp DoCol
    DW HERE
    DW LOAD_AT_16
    DW ADD_FORTH_16
    DW HERE
    DW STORE_AT_16
    DW QUOTE_END
    "
  ),

  # Begins a code comment. Everything up to and including the nearest ')' is ignored by the compiler.
  "(" => ForthDef.new(
    name: "(",
    compile: -> (state) {
      while state.next_word != ")" do
      end
    }),

  # Begins a single-line comment. Everything up to to the next newline character is ignored by the compiler.
  "\\" => ForthDef.new(
    name: "\\",
    compile: -> (state) {
      while state.next_char != "\n" do
      end
    }),

  # Performs an unconditional jump to the address immediately following the instruction.
  # Only used by the compiler in order to jump over quotations when being defined.
  "BRANCH" => ForthDef.new(
    name: "BRANCH",
    interpret: "
    ld h,d
    ld l,e
    ld a,[hl+]
    ld h,[hl]
    ld l,a
    ld d,h
    ld e,l
    jp Next
    "
  ),

  # Takes an address and transfers execution to it. This works on both execution tokens and quotation addresses.
  "CALL" => ForthDef.new( # ( xt -- ... )
    name: "CALL",
    label: "CALL_FORTH",
    interpret: "
    PopD16
    push de
    ld d,h
    ld e,l
    jp Next
    "
  ),

  # Causes execution to jump to the beginning of the current word just as if you had called it directly.
  "RECURSE" => ForthDef.new(
    name: "RECURSE",
    interpret: "
    ld hl,sp+0
    ld a,[hl+]
    ld h,[hl]
    ld l,a
    dec hl
    dec hl
    ld a,[hl+]
    ld h,[hl]
    ld l,a
    inc hl
    inc hl
    inc hl
    ld d,h
    ld e,l
    jp Next
    "
  ),

  # Use this version of RECURSE when inside a quotation, otherwise you will recurse on the quotation
  #   rather than the current word.
  "[RECURSE]" => ForthDef.new(
    name: "[RECURSE]",
    label: "QUOTE_RECURSE_FORTH",
    interpret: "
    ld hl,sp+2
    ld a,[hl+]
    ld h,[hl]
    ld l,a
    dec hl
    dec hl
    ld a,[hl+]
    ld h,[hl]
    ld l,a
    inc hl
    inc hl
    inc hl
    ld d,h
    ld e,l
    PopR
    PopR
    jp Next
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
      state.output("DW LIT2\nDW #{word.label}\n")
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
      word_def = "\njp DoCol\n" + raw_compile!(state, ';') + "DW QUOTE_END\n"
      state.definitions[word_name] = ForthDef.new(
        name: word_name,
        label: state.new_label,
        interpret: word_def
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
      state.output("DW LIT2\nDW :+\nDW BRANCH\nDW #{end_quote_label}\n:\n")
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
    pop de
    jp Next
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
          state.output("DW LIT2\nDW $#{const_val}\n")
	}
      )  
    }
  ),
  # Tests for equality between two cells and pushes the result. #0001 for True, #0000 for False
  "=" => ForthDef.new( # ( a b -- c )
    name: "=",
    label: "EQUALS_FORTH",
    interpret: "
    PopD16
    PopD
    xor h
    jp nz, :+
    PopD
    xor l
    jp nz, :+
    ld hl,1
    PushD16
    jp Next
    :
    ld hl,0
    PushD16
    jp Next
    "
  ),

  # Tests if the first byte is greater than the second and pushes the result.
  ">" => ForthDef.new( # ( a b -- c )
    name: ">",
    label: "GREATER_FORTH",
    interpret: "
    PopD16
    PopD
    cp h
    jp c, :+
    PopD
    cp l
    jp c, :++
    jp z, :++
    ld hl, 1
    PushD16
    jp Next
    :
    PopD
    :
    ld hl, 0
    PushD16
    jp Next
    "
  ),

  # Tests if the first byte is less than the second and pushes the result.
  "<" => ForthDef.new( # ( a b -- c )
    name: "<",
    label: "LESS_FORTH",
    interpret: "
    PopD16
    PopD
    cp h
    jp c, :+
    PopD
    cp l
    jp c, :++
    ld hl, 0
    PushD16
    jp Next
    :
    PopD
    :
    ld hl, 1
    PushD16
    jp Next
    "
  ),

  # ANDs two boolean cells. This is a logical AND, not a bitwise AND.
  "AND" => ForthDef.new( # ( a b -- c )
    name: "AND",
    label: "AND_FORTH",
    interpret: "
    PopD
    or a
    jp nz, :+
    PopD
    or a
    jp nz, :++
    PopD16
    ld hl, 0
    PushD16
    jp Next
    :
    PopD
    :
    PopD
    or a
    jp nz, :+
    PopD
    or a
    jp nz, :++
    ld hl, 0
    PushD16
    jp Next
    :
    PopD
    :
    ld hl, 1
    PushD16
    jp Next
    "
  ),

  # ORs two boolean bytes. This is a logical OR, not a bitwise OR.
  "OR" => ForthDef.new( # ( a b -- c )
    name: "OR",
    label: "OR_FORTH",
    interpret: "
    PopD
    or a
    jp nz, :+
    PopD
    or a
    jp nz, :++
    PopD
    or a
    jp nz, :+++
    PopD
    or a
    jp nz, :++++
    ld hl, 0
    PushD16
    jp Next
    :
    PopD
    :
    PopD
    :
    PopD
    :
    ld hl, 1
    PushD16
    jp Next
    "
  ),

  # Performs a logical NOT on a boolean byte. This is a logical NOT, not a bitwise NOT.
  "NOT" => ForthDef.new( # ( a -- !a )
    name: "NOT",
    label: "NOT_FORTH",
    interpret: "
    PopD
    or a
    jp nz, :+
    PopD
    or a
    jp nz, :++
    ld hl, 1
    PushD16
    jp Next
    :
    PopD
    :
    ld hl, 0
    PushD16
    jp Next
    "
  ),
  
  # Takes two quotations and a boolean byte. Calls the first quotation if the byte is True,
  #   calls the second quotation if the byte is False.
  "IF" => ForthDef.new( # ( n &a &b -- ... )
    name: "IF",
    label: "IF_FORTH",
    interpret: "
    push de
    PopD16
    ld d,h
    ld e,l
    PopD16
    PopD
    or a
    jp nz, :+
    PopD
    or a
    jp nz, :++
    jp Next
    :
    PopD
    :
    ld d,h
    ld e,l
    jp Next
    "
  ),

  # Halts the system by creating an infinite loop.
  "PAUSE" => ForthDef.new(
    name: "PAUSE",
    interpret: "
    jp PAUSE
    "
  ),

  "INCLUDE\"" => ForthDef.new(
    name: "INCLUDE\"",
    compile: ->(state) {
      filename = ""
      while (c = state.next_char) != '"'
        filename += c
      end
      state.error("No such file at \"#{state.include_path + filename}\"") unless File.exist?(state.include_path + filename)
      include_file_code = File.open(state.include_path + filename, 'r').read
      state.code.insert(state.code_index, include_file_code)
    }
  ),

  "ASM\"" => ForthDef.new(
     name: "ASM",
     compile: ->(state) {
       assembly = ""
       while (c = state.next_char) != '"'
         assembly += c
       end
       start_asm_label = state.new_label
       end_asm_label = state.new_label
       state.output("DW BRANCH\nDW #{end_asm_label}\n#{start_asm_label}:\n#{assembly}\n#{end_asm_label}:\nDW #{start_asm_label}\n")
     }
  ),

  "RUBY{" => ForthDef.new(
    name: "RUBY",
    compile: ->(state) {
      ruby_code = ""
      while (c = state.next_char) != '}'
        ruby_code += c
      end
      eval(ruby_code)
    }
  ),
      
}

class CompilerState
  attr_accessor :comment, :string, :compile, :code, :code_index, :output_code, :definitions, :include_path
  def initialize(code, compile: false, definitions: {})
    @code = code.strip
    @output_code = ""
    @code_index = 0
    @compile = compile
    @definitions = definitions
    @stack = []
    @label_counter = 0
    @code_line = 1
    @include_path = "./"
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
end

def raw_compile!(compiler_state, end_token)
  output_index = compiler_state.output_code.length
  until compiler_state.end_of_buffer
    t = compiler_state.next_word
    break if t == end_token

    if compiler_state.definitions[t]
      compiler_state.definitions[t].execute_and_compile(compiler_state)
    elsif (num = parse_number(t)) != nil
      compiler_state.output("DW LIT2\nDW $#{num}\n")
    #elsif is_byte(t)
    #  compiler_state.output("DW LIT\nDB $#{t[1..2]}\n")
    #elsif is_short(t)
    #  compiler_state.output("DW LIT2\nDW $#{t[1..4]}\n")
    else
      compiler_state.error("'#{t}' is not defined.")
    end
  end
  compiler_state.output_code[output_index..-1]
end

def compile(code, def_table, include_path)
  state = CompilerState.new(code, compile: false, definitions: def_table)
  state.include_path = include_path
  asm_defs = ''
  state.output("\nMain:\n")
  raw_compile!(state, nil)
  def_table.each_value { |d| asm_defs += d.compile_definition if d.interpret }
  state.output("DW PAUSE\n")
  PREAMBLE + asm_defs + state.output_code
end

options = { include: "./" }
OptionParser.new do |opts|
  opts.banner = "Usage: gbforth.rb <source> [-I include_path]"

  opts.on("-o", "--output OUTPUT", "Specify the output assembly source file") do |o|
    options[:output] = o
  end

  opts.on("-I", "--include INCLUDE_PATH", "Specify the path to include source files from") do |i|
    options[:include] = i 
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

