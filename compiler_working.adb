--
-- Implementation of PL/0 Compiler written in Ada
-- by John Perry, 2018
--

-- for text and integer I/O
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

-- gives us some cool tools for manipulating characters
with Ada.Characters.Handling; use Ada.Characters.Handling;
-- gives us access to cool constants like CR, HT (carriage return, horizontal tab)
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

-- we use Ordered_Maps to help look up strings for keywords
with Ada.Containers.Ordered_Maps;
-- we use Ordered_Sets to keep track of which threads are in use
with Ada.Containers.Ordered_Sets;

package body Compiler is

-- @summary prints the p-codes stored in code between start and stop
-- @param code table of p-codes to show
-- @param start index of where to start showing p-codes
-- @param stop index of where to stop showing p-codes
procedure Show_PCode(code: in PCode_Table; start, stop: Table_Range) is
begin
  for i in start..stop loop
    Put(Positive(i), 4); Put(' ');
    Put(code(i).instruction'Image); Put(' ');
    Put(code(i).level, 4);
    Put(code(i).data, 4);
    Put("    ");
    Put(To_String(code(i).comment));
    New_Line(1);
  end loop;
end Show_PCode;

-- enumeration of errors; see Error_Messages for elaboration
type Errors is (
  SEMICOLON,
  IDENT,
  UNKNOWN,
  ASSIGN,
  ASSIGN_PROC,
  CALL_PROC,
  END_SYM,
  DO_SYM,
  THEN_SYM,
  VARIABLE,
  RPAREN,
  LPAREN,
  PROG_SIZE,
  NUMBER_EXPECTED,
  REL,
  NUMBER_IDENT,
  NOPROCEDURE,
  END_PROG,
  IS_PROCEDURE,
  BAD_SYMBOL,
  BAD_KIND,
  UNRECOGNIZED_PUNCTUATION,
  END_OF_FILE,
  EXPECT_UNTIL,
  TO_OR_DOWNTO,
  EXPECT_OF,
  COLON,
  CASE_NEEDS_CONSTANT,
  FOR_NEEDS_VARIABLE,
  TOO_MANY_THREADS,
  ARRAY_EXPECTS_SIZE,
  ARRAY_EXPECTS_INDEX,
  ARRAY_DECLARATION_REQUIRES_BRACKETS
);

-- String representation of error codes.
-- The strings must appear in correct order when compared to the interface.
-- If you add errors, add the string IN THE CORRECT PLACE!
Error_Messages: array(Errors) of Bounded_String := (
  To_Bounded_String("Semicolon Expected"),
  To_Bounded_String("Identifier Expected"),
  To_Bounded_String("Unknown Identifier"),
  To_Bounded_String("Assignment operator expected"),
  To_Bounded_String("Assignment to PROCEDURE not allowed"),
  To_Bounded_String("Can only call a procedure"),
  To_Bounded_String("END symbol Expected"),
  To_Bounded_String("DO symbol Expected"),
  To_Bounded_String("THEN symbol Expected"),
  To_Bounded_String("Variable or Expression Expected"),
  To_Bounded_String("RIGHT Parenthesis Expected"),
  To_Bounded_String("LEFT Parenthesis Expected"),
  To_Bounded_String("Program size is too large..."),
  To_Bounded_String("A number was Expected"),
  To_Bounded_String("Relational operator expected"),
  To_Bounded_String("number or ident expected"),
  To_Bounded_String("Procedure not accepted here"),
  To_Bounded_String("Premature end of program"),
  To_Bounded_String("Assignment not allowed here"),
  To_Bounded_String("Internal error: bad symbol encountered"),
  To_Bounded_String("Internal error: bad object kind encountered"),
  To_Bounded_String("Unrecognized punctuation"),
  To_Bounded_String("Unexpected end of file"),
  To_Bounded_String("UNTIL symbol expected"),
  To_Bounded_String("TO or DOWNTO symbol expected"),
  To_Bounded_String("OF symbol expected"),
  To_Bounded_String("Colon expected"),
  To_Bounded_String("Cases must be constants"),
  To_Bounded_String("For loops require a declared variable"),
  To_Bounded_String("Too many threads open already, cannot start a new one"),
  To_Bounded_String("Must specify a size for an array"),
  To_Bounded_String("Can only access arrays through indices"),
  To_Bounded_String(
      "Specify array size with a number in brackets [...] after name"
  )
);

-- @summary raises an informative exception
-- @description Program will halt!
-- @param num which error was selected
-- @exception Compile_Error the compiler has encountered a fatal error
procedure Error(num: Errors) is
  Compile_Error: exception;
begin
  New_Line(1);
  raise Compile_Error with To_String(error_messages(num));
end Error;

-- symbols/tokens the compiler understands
type Symbol is (
	VARSYM,
	CONSTSYM,
	BEGINSYM,
	ENDSYM,
	PERIOD,
	SEMICOLON,
	COLON,
	LPAREN,
	RPAREN,
	EQL,        -- keep
	NOTEQL,     -- these
	LSTHEN,     -- six
	GREQL,      -- symbols
	GRTHEN,     -- in
	LSEQL,      -- order
	ASSIGN,
	IFSYM,
	IDENT,
	NUM,
	PROCSYM,
	MINUS,
	PLUS,
	DIV,
	MULT,
	COMMA,
	ODDSYM,
	CALL,
	THENSYM,
	WHILESYM,
	DOSYM,
	-- added by John Perry
	ELSESYM,
	REPEATSYM,
	UNTILSYM,
	WRITESYM,
	WRITELNSYM,
	FORSYM,
	TOSYM,
	DOWNTOSYM,
	CASESYM,
	OFSYM,
	CENDSYM,
	ORSYM,
	ANDSYM,
	NOTSYM,
	COBEGINSYM,
	COENDSYM,
	THREADSYM,
	ARRAYSYM,
	LBRACKET,
	RBRACKET,
	-- this last one should probably remain
	MAX_SYMBOL
);

-- helps avoid if statements when processing OPR 0,?
subtype Relation_Symbol is Symbol range EQL..LSEQL;
-- these go with OPR 0,?
relation_operands: array(Relation_Symbol) of Integer := (8,  9, 10, 11, 12, 13);
-- comment strings for OPR 0,?
relation_comments: array(Relation_Symbol) of String(1..3) := (
  "=?=",
  "=/=",
  "<? ",
  ">=?",
  ">? ",
  "<=?"
);

-- @summary used to identify the keyword from a string
-- @description This package instantiates the Ordered_Maps generic package
-- to look up a Symbol using a Bounded_String
package Keyword_From_String is new Ada.Containers.Ordered_Maps(
  Key_Type     => Bounded_String,
  Element_Type => Symbol
);
use Keyword_From_String;

-- mapping strings to symbols without if statements!
keyword_string_map: Map;
-- mapping punctuation to symbols without if statements!
punctuation_map: Map;

-- Ada has some nice ways to initialize some things without loops
-- but I haven't yet figured out how to do that for a Map (if possible)
-- flag as to whether to initialize the keyword map
initialized_keyword_map: Boolean := false;

-- @summary initializes keyword_string_map and punctuation_map;
-- @description this needs to be run at least once, but only once
procedure Initialize_Keyword_Map is
begin

  Include(keyword_string_map, To_Bounded_String("AND"),       ANDSYM    );
  Include(keyword_string_map, To_Bounded_String("ARRAY"),     ARRAYSYM  );
  Include(keyword_string_map, To_Bounded_String("BEGIN"),     BEGINSYM  );
  Include(keyword_string_map, To_Bounded_String("CALL"),      CALL      );
  Include(keyword_string_map, To_Bounded_String("CASE"),      CASESYM   );
  Include(keyword_string_map, To_Bounded_String("CEND"),      CENDSYM   );
  Include(keyword_string_map, To_Bounded_String("COBEGIN"),   COBEGINSYM);
  Include(keyword_string_map, To_Bounded_String("COEND"),     COENDSYM  );
  Include(keyword_string_map, To_Bounded_String("CONST"),     CONSTSYM  );
  Include(keyword_string_map, To_Bounded_String("DO"),        DOSYM     );
  Include(keyword_string_map, To_Bounded_String("DOWNTO"),    DOWNTOSYM );
  Include(keyword_string_map, To_Bounded_String("ELSE"),      ELSESYM   );
  Include(keyword_string_map, To_Bounded_String("END"),       ENDSYM    );
  Include(keyword_string_map, To_Bounded_String("FOR"),       FORSYM    );
  Include(keyword_string_map, To_Bounded_String("IF"),        IFSYM     );
  Include(keyword_string_map, To_Bounded_String("NOT"),       NOTSYM    );
  Include(keyword_string_map, To_Bounded_String("ODD"),       ODDSYM    );
  Include(keyword_string_map, To_Bounded_String("OF"),        OFSYM     );
  Include(keyword_string_map, To_Bounded_String("OR"),        ORSYM     );
  Include(keyword_string_map, To_Bounded_String("PROCEDURE"), PROCSYM   );
  Include(keyword_string_map, To_Bounded_String("REPEAT"),    REPEATSYM );
  Include(keyword_string_map, To_Bounded_String("THEN"),      THENSYM   );
  Include(keyword_string_map, To_Bounded_String("THREAD"),    THREADSYM );
  Include(keyword_string_map, To_Bounded_String("TO"),        TOSYM     );
  Include(keyword_string_map, To_Bounded_String("UNTIL"),     UNTILSYM  );
  Include(keyword_string_map, To_Bounded_String("VAR"),       VARSYM    );
  Include(keyword_string_map, To_Bounded_String("WHILE"),     WHILESYM  );
  Include(keyword_string_map, To_Bounded_String("WRITE"),     WRITESYM  );
  Include(keyword_string_map, To_Bounded_String("WRITELN"),   WRITELNSYM);

  Include(punctuation_map, To_Bounded_String(":="), ASSIGN    );
  Include(punctuation_map, To_Bounded_String(":" ), COLON     );
  Include(punctuation_map, To_Bounded_String("," ), COMMA     );
  Include(punctuation_map, To_Bounded_String("." ), PERIOD    );
  Include(punctuation_map, To_Bounded_String("/" ), DIV       );
  Include(punctuation_map, To_Bounded_String("-" ), MINUS     );
  Include(punctuation_map, To_Bounded_String("*" ), MULT      );
  Include(punctuation_map, To_Bounded_String("+" ), PLUS      );
  Include(punctuation_map, To_Bounded_String("=" ), EQL       );
  Include(punctuation_map, To_Bounded_String("<>"), NOTEQL    );
  Include(punctuation_map, To_Bounded_String(">="), GREQL     );
  Include(punctuation_map, To_Bounded_String(">" ), GRTHEN    );
  Include(punctuation_map, To_Bounded_String("(" ), LPAREN    );
  Include(punctuation_map, To_Bounded_String("<="), LSEQL     );
  Include(punctuation_map, To_Bounded_String("<" ), LSTHEN    );
  Include(punctuation_map, To_Bounded_String(")" ), RPAREN    );
  Include(punctuation_map, To_Bounded_String(";" ), SEMICOLON );
  Include(punctuation_map, To_Bounded_String("[" ), LBRACKET  );
  Include(punctuation_map, To_Bounded_String("]" ), RBRACKET  );

  initialized_keyword_map := true;

end Initialize_Keyword_Map;

-- types of identifiers
type Identifier_Type is (
  NONE,   -- unrecognized (probably an error)
  CONST,  -- constant (but "constant" is a keyword and Ada is case-insensitive)
  VAR,    -- variable
  PROC,   -- procedure (but "procedure" is a keyword)
  ARR     -- array (but "array" is a keyword)
);

-- classification of characters read from input
type Input_Type is ( ALPHA, DIGIT, EOL, NONE, PUNCT, WHITESPACE );
-- things we recognize as characters
subtype Punct_Range is Character range Exclamation..Low_Line;

-- @summary determine the character type of ch
-- @param ch character read from input
-- @return what sort of character ch is
function Char_Type(ch: Character) return Input_Type is
  result: Input_Type := NONE;
begin
  if ch = LF or ch = CR then result := EOL;
  elsif Is_Letter(ch) then result := ALPHA;
  elsif Is_Digit(ch)  then result := DIGIT;
  elsif ch = Space or ch = HT then result := WHITESPACE;
  elsif ch in Punct_Range then result := PUNCT;
  end if;
  return result;
end Char_Type;

line:      Bounded_String; -- last word processed from input
punc:      Bounded_String; -- last puncuation processed from input
read_line: Bounded_String; -- full line read from input (please be <80 chars!)
number:    Integer;        -- last number processed from input

linelength: Length_Range := 0; -- length of line read
char_pos:   Length_Range := 1; -- position of next character to process (>= 1!)

-- @summary read a character from input
-- @description If we have already read in an entire line,
-- and have not exhausted it, reads the next unprocessed character in the line.
-- Otherwise, it attempts to read a new line from the input.
procedure Get_Char(ch: out Character) is
begin
  if char_pos > linelength then
    if End_Of_File then Error(END_OF_FILE); end if;
    read_line := To_Bounded_String(Get_Line);
    Append(read_line, ' ');
    linelength := Length(read_line);
    Put(To_String(read_line));
    New_Line(1);
    char_pos := 1;
  end if;
  ch := Element(read_line, char_pos);
  char_pos := char_pos + 1;
end Get_Char;

-- used for converting characters to digits
subtype Digit_Range is Natural range 0..9;

-- @summary Converts ch to the corresponding digit.
-- @description Makes use of Ada's attributes ('Pos), which are sort of weird.
-- I haven't seen them in other languages.
-- You'll see them elsewhere.
-- @param ch character from input
-- @return a digit corresponding to this character ('0'->0, '1'->1, etc.)
-- @exception Program_Error raised when ch is not a digit
function Ordinal(ch: Character) return Digit_Range is
  result: Digit_Range;
begin
  case ch is
    when '0'..'9' => result := Character'Pos(ch) - Character'Pos('0');
    when others => raise Program_Error;
  end case;
  return result;
end Ordinal;

-- @summary gets the next symbol from the input
-- @param sym where to store the symbol
procedure Get_Symbol(sym: out Symbol) is
  ch:       Character;    -- character to be read
  ch_type:  Input_Type;   -- what ch turns out to be
begin
  -- skip whitespace
  loop
    Get_Char(ch);
    ch_type := Char_Type(ch);
    exit when ch_type /= WHITESPACE and ch_type /= EOL;
  end loop;
  -- if it's an alphabetic character, finish reading a full word
  if Char_Type(ch) = ALPHA then
    line := Null_Bounded_String;
    loop
      Append(line, ch);
      Get_Char(ch);
      exit when (Char_Type(ch) /= ALPHA and Char_Type(ch) /= DIGIT and ch /= '_');
    end loop;
    if ch /= CR then char_pos := char_pos - 1; end if;
    -- determine if it's a keyword...
    if Contains(keyword_string_map, line) then
      -- ...choose the correct symbol...
      sym := Element(keyword_string_map, line);
    else
      -- ...consider it an identifier
      sym := IDENT;
    end if;
  -- if it's a digit, finish reading the full number
  elsif Char_Type(ch) = DIGIT then
    sym := NUM;
    number := 0;
    loop
      number := 10 * number + Ordinal(ch);
      Get_Char(ch);
      exit when Char_Type(ch) /= DIGIT;
    end loop;
    if ch /= CR then char_pos := char_pos - 1; end if;
  -- if it's a punctuation...
  elsif Char_Type(ch) = PUNCT then
    punc := Null_Bounded_String;
    Append(punc, ch);
    -- check if it's something that might get more than one character
    if ch = ':' or ch = '<' or ch = '>' then
      -- try reading if so
      Get_Char(ch);
      if Char_Type(ch) = PUNCT and (ch = '=' or ch = '>') then
        Append(punc, ch);
      else
        if ch /= CR then char_pos := char_pos - 1; end if;
      end if;
    end if;
    -- is it something we recognize?
    if Contains(punctuation_map, punc) then
      sym := Element(punctuation_map, punc);
    else
      Error(UNRECOGNIZED_PUNCTUATION);
    end if;
  end if;
end Get_Symbol;

-- entry into symbol table
type Symbol_Table_Entry is record
  name:   Bounded_String;  -- name of the variable/procedure/etc.
  kind:   Identifier_Type; -- whether it's a variable/procedure/etc.
  value:  Integer;         -- for constants
  level:  Integer;         -- relation to the current stack frame
  adr:    Table_Range;     -- where to find it
end record;

-- now set up the symbol table
max_symbols: constant := 499;
type Symbol_Table_Range is range 0..max_symbols;
symbol_table: array(Symbol_Table_Range) of Symbol_Table_Entry;

-- @summary enter an identifier into the symbol table
procedure Enter(
  kind:        Identifier_Type;           -- whether it's VAR, PROC, etc.
  name:        Bounded_String;            -- the identifier in the source code
  sym:         in out Symbol;             -- the current symbol;
                                          -- modified with value of next symbol
  varcount:    in out Table_Range;        -- number of variables in current frame
  level:       Integer;                   -- current level of the table
  table_index: in out Symbol_Table_Range; -- number of entries in the table
  size:        Table_Range := 0           -- (for arrays) size of object on stack
) is
begin
  table_index := table_index + 1;
  symbol_table(table_index).name := name;
  symbol_table(table_index).kind := kind;

  if kind = CONST then
    if sym /= IDENT then Error(IDENT); end if;
    Get_Symbol(sym);
    if sym /= EQL then Error(ASSIGN); end if;
    Get_Symbol(sym);
    if sym /= NUM then Error(NUMBER_EXPECTED); end if;
    symbol_table(table_index).value := number;
  elsif kind = VAR then
    if sym /= IDENT then Error(IDENT); end if;
    symbol_table(table_index).level := level;
    symbol_table(table_index).adr   := varcount;
    varcount := varcount + 1;
  elsif kind = ARR then
    symbol_table(table_index).level := level;
    symbol_table(table_index).adr   := varcount;
    varcount := varcount + size;
  elsif kind = PROC then
    symbol_table(table_index).level := level;
  end if;
  Get_Symbol(sym);
end Enter;

-- where we are in the code
code_index: Table_Range := 0;
-- where the code started
start_code_index: Table_Range := 0;

-- @summary generate a p-code instruction in the program
procedure Gen(
  code: in out PCode_Table; -- list of p-code instructions
  instruction: PCode;       -- new p-code instruction to add
  level: Integer;           -- level of data relative to current stack frame
  data: Integer;            -- data for the instruction
  comment: Bounded_String := Null_Bounded_String
                            -- optional comment for printing p-code
) is
begin
  if code_index > max_table_size then Error(PROG_SIZE); end if;
  code(code_index).instruction := instruction;
  code(code_index).level       := level;
  code(code_index).data        := data;
  code(code_index).comment     := comment;
  code_index := code_index + 1;
end Gen;

-- @summary find the position of package variable line in the symbol table
-- @param table_index where to start looking
-- @return zero if the string stored in line does not appear in the table;
-- otherwise, it returns the index where that string appears
function Position(table_index: Symbol_Table_Range) return Symbol_Table_Range is
  i: Symbol_Table_Range := table_index;
begin
  loop
    exit when i = 0 or line = symbol_table(i).name;
    i := i - 1;
  end loop;
  return i;
end Position;

-- @summary process a statement of the input file
-- @param code list of p-code instructions
-- @param sym the current symbol; modified with value of next symbol
-- @param level current stack frame
-- @param table_index number of identifiers currently in the stack
procedure Statement(
    code: in out PCode_Table;
    sym: in out Symbol;
    level: Integer;
    table_index: Symbol_Table_Range
);

-- @summary process an expression of the input file
-- @param code list of p-code instructions
-- @param sym the current symbol; modified with value of next symbol
-- @param level current stack frame
-- @param table_index number of identifiers currently in the stack
procedure Expression(
    code: in out PCode_Table;
    sym: in out Symbol;
    level: Integer;
    table_index: Symbol_Table_Range
);

-- @summary process a general expression in the input file
-- @param code list of p-code instructions
-- @param sym the current symbol; modified with value of next symbol
-- @param level current stack frame
-- @param table_index number of identifiers currently in the stack
procedure General_Expression(
  code: in out PCode_Table;
  sym: in out Symbol;
  level: Integer;
  table_index: Symbol_Table_Range
) is
  saved_sym: Symbol;
begin
  if sym = ODDSYM then
    Get_Symbol(sym);
    Expression(code, sym, level, table_index);
    Gen(code, OPR, 0, 6, To_Bounded_String("odd?"));
  else
    Expression(code, sym, level, table_index);
    if sym in Relation_Symbol then
      saved_sym := sym;
      Get_Symbol(sym);
      Expression(code, sym, level, table_index);
      Gen(
          code,
          OPR,
          0,
          relation_operands(saved_sym),
          To_Bounded_String(relation_comments(saved_sym))
      );
    end if;
  end if;
end General_Expression;

-- @summary process a factor in the input file
-- @param code list of p-code instructions
-- @param sym the current symbol; modified with value of next symbol
-- @param level current stack frame
-- @param table_index number of identifiers currently in the stack
procedure Factor(
  code: in out PCode_Table;
  sym: in out Symbol;
  level: Integer;
  table_index: Symbol_Table_Range
) is
  i: Symbol_Table_Range;
begin

  case sym is

    when THREADSYM =>

      Gen(code, THN, 0, 0, To_Bounded_String("thread number"));
      Get_Symbol(sym);

    when IDENT =>

      i := Position(table_index);
      if i = 0 then Error(UNKNOWN); end if;
      if symbol_table(i).kind = PROC then Error(IS_PROCEDURE); end if;

      case symbol_table(i).kind is

        when VAR =>

          Gen(
            code,
            LOD,
            level - symbol_table(i).level,
            symbol_table(i).adr,
            symbol_table(i).name
          );

        when CONST =>

          Gen(code, LIT, 0, symbol_table(i).value, symbol_table(i).name);

        when ARR =>

          Get_Symbol(sym);
          if sym /= LBRACKET then Error(ARRAY_EXPECTS_INDEX); end if;
          Get_Symbol(sym);
          Expression(code, sym, level, table_index);
          if sym /= RBRACKET then Error(ARRAY_EXPECTS_INDEX); end if;
          Gen(
            code,
            LDX,
            level - symbol_table(i).level,
            symbol_table(i).adr,
            symbol_table(i).name
          );

        when others => Error(BAD_KIND);

      end case;

      Get_Symbol(sym);

    when NUM =>

      Gen(code, LIT, 0, number);
      Get_Symbol(sym);

    when LPAREN =>

      Get_Symbol(sym);
      General_Expression(code, sym, level, table_index);
      if sym /= RPAREN then Error(RPAREN); end if;
      Get_Symbol(sym);

    when NOTSYM =>

      Get_Symbol(sym);
      Factor(code, sym, level, table_index);
      Gen(code, LIT, 0, 0, To_Bounded_String("not"));
      Gen(code, OPR, 0, 8, To_Bounded_String("eq?"));

    when others => Error(VARIABLE);

  end case;

end Factor;

-- @summary process a term in the input file
-- @param code list of p-code instructions
-- @param sym the current symbol; modified with value of next symbol
-- @param level current stack frame
-- @param table_index number of identifiers currently in the stack
procedure Term(
  code: in out PCode_Table;
  sym: in out Symbol;
  level: Integer;
  table_index: Symbol_Table_Range
) is
  prev_sym: Symbol;
begin
  Factor(code, sym, level, table_index);
  while sym = MULT or sym = DIV or sym = ANDSYM loop
    prev_sym := sym;
    Get_Symbol(sym);
    Factor(code, sym, level, table_index);
    if prev_sym = DIV then Gen(code, OPR, 0, 5, To_Bounded_String("div"));
    elsif prev_sym = MULT then Gen(code, OPR, 0, 4, To_Bounded_String("mul"));
    else gen(code, OPR, 0, 4, To_Bounded_String("and"));
    end if;
  end loop;
end Term;

procedure Expression(
  code: in out PCode_Table;
  sym: in out Symbol;
  level: Integer;
  table_index: Symbol_Table_Range
) is
  prev_sym: Symbol;
begin
  if sym = PLUS or sym = MINUS then
    prev_sym := sym;
    Get_Symbol(sym);
    Term(code, sym, level, table_index);
    if prev_sym = MINUS then
      Gen(code, OPR, 0, 1, To_Bounded_String("negate"));
    end if;
  else
    Term(code, sym, level, table_index);
  end if;
  while sym = PLUS or sym = MINUS or sym = ORSYM loop
    prev_sym := sym;
    Get_Symbol(sym);
    Term(code, sym, level, table_index);
    if prev_sym = MINUS then Gen(code, OPR, 0, 3, To_Bounded_String("sub"));
    elsif prev_sym = PLUS then Gen(code, OPR, 0, 2, To_Bounded_String("add"));
    else Gen(code, OPR, 0, 2, To_Bounded_String("or"));
    end if;
  end loop;
end Expression;

-- @summary process an if statement
-- @param code list of p-code instructions
-- @param sym the current symbol; modified with value of next symbol
-- @param level current stack frame
-- @param table_index number of identifiers currently in the stack
procedure Process_If(
  code: in out PCode_Table;
  sym: in out Symbol;
  level: Integer;
  table_index: Symbol_Table_Range
) is
  cx1, cx2: Table_Range;
begin
  Gen(code, OPR, 0, 7, To_Bounded_String("IF"));
  Get_Symbol(sym);
  General_Expression(code, sym, level, table_index);
  cx1 := code_index;
  Gen(code, JPC, 0, 0);
  if sym /= THENSYM then Error(THEN_SYM); end if;
  Get_Symbol(sym);
  Statement(code, sym, level, table_index);
  cx2 := code_index;
  Gen(code, JMP, 0, 0);
  code(cx1).data := Code_index;
  if sym = ELSESYM then
    Get_Symbol(sym);
    Statement(code, sym, level, table_index);
  end if;
  code(cx2).data := Code_index;
end Process_If;

-- @summary process a repeat statement
-- @param code list of p-code instructions
-- @param sym the current symbol; modified with value of next symbol
-- @param level current stack frame
-- @param table_index number of identifiers currently in the stack
procedure Process_Repeat(
  code: in out PCode_Table;
  sym: in out Symbol;
  level: Integer;
  table_index: Symbol_Table_Range
) is
  cx1: Table_Range;
begin
  cx1 := code_index;
  Gen(code, OPR, 0, 7, To_Bounded_String("REPEAT"));
  loop
    Get_Symbol(sym);
    Statement(code, sym, level, table_index);
    exit when sym /= SEMICOLON;
  end loop;
  if sym /= UNTILSYM then Error(EXPECT_UNTIL); end if;
  Gen(code, OPR, 0, 7, To_Bounded_String("UNTIL"));
  Get_Symbol(sym);
  General_Expression(code, sym, level, table_index);
  Gen(code, JPC, 0, Cx1);
end Process_Repeat;

-- @summary process a for statement
-- @param code list of p-code instructions
-- @param sym the current symbol; modified with value of next symbol
-- @param level current stack frame
-- @param table_index number of identifiers currently in the stack
procedure Process_For(
  code: in out PCode_Table;
  sym: in out Symbol;
  level: Integer;
  table_index: Symbol_Table_Range
) is
  i: Symbol_Table_Range;
  cx1, cx2: Table_Range;
  upwards: Boolean;
begin
  Gen(code, OPR, 0, 7, To_Bounded_String("FOR"));
  Get_Symbol(sym);
  if sym /= IDENT then Error(IDENT); end if;
  i := Position(table_index);
  if symbol_table(i).kind /= VAR then Error(FOR_NEEDS_VARIABLE); end if;
  Get_Symbol(sym);
  if sym /= ASSIGN then Error(ASSIGN); end if;
  Get_Symbol(sym);
  Expression(code, sym, level, table_index);
  Gen(
      code,
      STO,
      level - symbol_table(i).level,
      Symbol_table(i).adr,
      symbol_table(i).name
  );
  if sym /= TOSYM and sym /= DOWNTOSYM then Error(TO_OR_DOWNTO); end if;
  upwards := sym = TOSYM;
  Get_Symbol(sym);
  Expression(code, sym, level, table_index);
  cx1 := code_index;
  -- duplicate top of stack
  Gen(code, CTS, 0, 0);
  Gen(
      code,
      LOD,
      level - symbol_table(i).level,
      Symbol_table(i).adr,
      symbol_table(i).name
  );
  if upwards then Gen(code, OPR, 0, 11, To_Bounded_String("geq?"));
  else Gen(code, OPR, 0, 13, To_Bounded_String("leq?"));
  end if;
  cx2 := code_index;
  Gen(code, JPC, 0, 0);
  if sym /= DOSYM then Error(DO_SYM); end if;
  Get_Symbol(sym);
  Statement(code, sym, level, table_index);
  if upwards then Gen(code, LIT, 0, 1, To_Bounded_String("up"));
  else Gen(code, LIT, 0, -1, To_Bounded_String("down"));
  end if;
  Gen(
      code,
      LOD,
      level - symbol_table(i).level,
      Symbol_table(i).adr,
      symbol_table(i).name
  );
  Gen(code, OPR, 0, 2, To_Bounded_String("step"));
  Gen(
      code,
      STO,
      level - symbol_table(i).level,
      Symbol_table(i).adr,
      symbol_table(i).name
  );
  Gen(code, JMP, 0, Cx1);
  code(cx2).data := Code_index;
  Gen(code, INT, 0, -1);
end Process_For;

-- @summary process a case statement
-- @param code list of p-code instructions
-- @param sym the current symbol; modified with value of next symbol
-- @param level current stack frame
-- @param table_index number of identifiers currently in the stack
procedure Process_Case(
  code: in out PCode_Table;
  sym: in out Symbol;
  level: Integer;
  table_index: Symbol_Table_Range
) is
  i: Symbol_Table_Range;
  cx0, cx1: Table_Range;
begin
  Gen(code, JMP, 0, code_index + 2, To_Bounded_String("CASE"));
  cx0 := code_index;
  Gen(code, JMP, 0, 0);
  Get_Symbol(sym);
  Expression(code, sym, level, table_index);
  if sym /= OFSYM then Error(EXPECT_OF); end if;
  Get_Symbol(sym);
  while sym /= CENDSYM loop
    -- duplicate top of stack
    Gen(code, CTS, 0, 0);
    if sym /= IDENT and sym /= NUM then Error(NUMBER_IDENT); end if;
    if sym = IDENT then
      i := Position(table_index);
      if symbol_table(i).kind /= CONST then Error(CASE_NEEDS_CONSTANT); end if;
      Gen(
          code,
          LIT,
          0,
          symbol_table(i).value,
          To_Bounded_String("case " & To_String(symbol_table(i).name))
      );
    else
      Gen(
          code,
          LIT,
          0,
          number,
          To_Bounded_String("case " & Integer'Image(number))
      );
    end if;
    Gen(code, OPR, 0, 8, To_Bounded_String("equal?"));
    Get_Symbol(sym);
    if sym /= COLON then Error(COLON); end if;
    cx1 := code_index;
    Gen(code, JPC, 0, 0);
    Get_Symbol(sym);
    Statement(code, sym, level, table_index);
    code(cx1).data := code_index + 1;
    if sym /= SEMICOLON then Error(SEMICOLON); end if;
    Get_Symbol(sym);
    Gen(code, JMP, 0, Cx0);
  end loop;
  code(cx0).data := Code_index;
  Gen(code, INT, 0, -1, To_Bounded_String("CEND"));
  Get_Symbol(sym);
end Process_Case;

-- @summary process a cobegin statement
-- @param code list of p-code instructions
-- @param sym the current symbol; modified with value of next symbol
-- @param level current stack frame
-- @param table_index number of identifiers currently in the stack
procedure Process_Cobegin(
  code: in out PCode_Table;
  sym: in out Symbol;
  level: Integer;
  table_index: Symbol_Table_Range
) is
  num_to_spawn: Natural := 0;
  cx1: Table_Range;
begin
  Gen(code, OPR, 0, 7, To_Bounded_String("COBEGIN"));
  Get_Symbol(sym);
  while sym /= COENDSYM loop
    num_to_spawn := num_to_spawn + 1;
    cx1 := code_index;
    Gen(
        code,
        SNT,
        0,
        0,
        To_Bounded_String("spawn " & Integer'Image(num_to_spawn))
    );
    Statement(code, sym, level, table_index);
    Gen(
        code,
        RFT,
        0,
        0,
        To_Bounded_String("join " & Integer'Image(num_to_spawn))
    );
    code(cx1).data := code_index;
    if sym /= SEMICOLON then Error(SEMICOLON); end if;
    Get_Symbol(sym);
  end loop;
  Gen(code, WFT, 0, 0, To_Bounded_String("COEND"));
  Get_Symbol(sym);
end Process_Cobegin;

procedure Statement( -- documented above
  code: in out PCode_Table;
  sym: in out Symbol;
  level: Integer;
  table_index: Symbol_Table_Range
) is
  i: Symbol_Table_Range;
  cx1, cx2: Table_Range;
begin

  case sym is

    when IDENT =>

      i := Position(table_index);
      if i = 0 then Error(UNKNOWN); end if;

      case symbol_table(i).kind is

        when VAR =>

          Get_Symbol(sym);
          if sym /= ASSIGN then Error(ASSIGN); end if;
          Get_Symbol(sym);
          Expression(code, sym, level, table_index);
          Gen(
              code,
              STO,
              level - symbol_table(i).level,
              Symbol_table(i).adr,
              symbol_table(i).name
          );

        when ARR =>

          Get_Symbol(sym);
          if sym /= LBRACKET then Error(ARRAY_EXPECTS_INDEX); end if;
          Get_Symbol(sym);
          Expression(code, sym, level, table_index);
          if sym /= RBRACKET then Error(ARRAY_EXPECTS_INDEX); end if;
          Get_Symbol(sym);
          if sym /= ASSIGN then Error(ASSIGN); end if;
          Get_Symbol(sym);
          Expression(code, sym, level, table_index);
          Gen(
              code,
              STX,
              level - symbol_table(i).level,
              Symbol_table(i).adr,
              symbol_table(i).name
          );

        when others => Error(ASSIGN_PROC);

      end case;

    when CALL =>

      Get_Symbol(sym);
      if sym /= IDENT then Error(IDENT); end if;
      i := Position(table_index);
      if i = 0 then Error(UNKNOWN); end if;
      if symbol_table(i).kind /= PROC then Error(CALL_PROC); end if;
      Get_Symbol(sym);
      Gen(
          code,
          CAL,
          level - symbol_table(i).level,
          Symbol_table(i).adr,
          symbol_table(i).name
      );

    when BEGINSYM =>

      Get_Symbol(sym);
      Statement(code, sym, level, table_index);
      while sym = SEMICOLON loop
        Get_Symbol(sym);
        Statement(code, sym, level, table_index);
      end loop;
      if sym /= ENDSYM then Error(END_SYM); end if;
      Get_Symbol(sym);

    when WHILESYM =>

      Gen(code, OPR, 0, 7, To_Bounded_String("WHILE"));
      Get_Symbol(sym);
      cx1 := code_index;
      General_Expression(code, sym, level, table_index);
      cx2 := code_index;
      Gen(code, JPC, 0, 0);
      if sym /= DOSYM then Error(DO_SYM); end if;
      Get_Symbol(sym);
      Statement(code, sym, level, table_index);
      Gen(code, JMP, 0, Cx1);
      code(cx2).data := Code_index;

    when IFSYM => Process_If(code, sym, level, table_index);

    when WRITESYM | WRITELNSYM =>

      declare newln: Boolean := sym = WRITELNSYM;
      begin
        Get_Symbol(sym);
        if sym /= LPAREN then Error(LPAREN); end if;
        loop
          Get_Symbol(sym);
          Expression(code, sym, level, table_index);
          Gen(code, OPR, 0, 14, To_Bounded_String("WRITE"));
          exit when sym /= COMMA;
        end loop;
        if sym /= RPAREN then Error(RPAREN); end if;
        Get_Symbol(sym);
        if newln then Gen(code, OPR, 0, 15, To_Bounded_String("WRITELN")); end if;
      end;

    when REPEATSYM  => Process_Repeat(code, sym, level, table_index);

    when FORSYM     => Process_For(code, sym, level, table_index);

    when CASESYM    => Process_Case(code, sym, level, table_index);

    when COBEGINSYM => Process_Cobegin(code, sym, level, table_index);
 
    when others     => null;

  end case;

end Statement;

-- @summary process a block of the input file
-- @param code list of p-code instructions
-- @param sym the current symbol; modified with value of next symbol
-- @param level current stack frame
-- @param table_index number of identifiers currently in the stack
procedure Block(
  code: in out PCode_Table;
  sym: in out Symbol;
  level: Integer;
  table_index_initial: Symbol_Table_Range
) is
  varcount: Table_Range;
  size: Table_Range;
  start_table_index: Symbol_Table_Range;
  table_index: Symbol_Table_Range := table_index_initial;
begin
  varcount := 5;
  start_table_index := table_index;
  symbol_table(table_index).adr := code_index;
  Gen(code, JMP, 0, 0);
  
  while sym = CONSTSYM or sym = VARSYM or sym = PROCSYM or sym = ARRAYSYM loop
    if sym = CONSTSYM then
      Get_Symbol(sym);
      Enter(CONST, line, sym, varcount, level, table_index);
      while sym = COMMA loop
        Get_Symbol(sym);
        Enter(CONST, line, sym, varcount, level, table_index);
      end loop;
      if sym /= SEMICOLON then Error(SEMICOLON); end if;
      Get_Symbol(sym);
    elsif sym = VARSYM then
      Get_Symbol(sym);
      Enter(VAR, line, sym, varcount, level, table_index);
      while sym = COMMA loop
        Get_Symbol(sym);
        Enter(VAR, line, sym, varcount, level, table_index);
      end loop;
      if sym /= SEMICOLON then Error(SEMICOLON); end if;
      Get_Symbol(sym);
    end if;
    while sym = PROCSYM loop
      Get_Symbol(sym);
      if sym /= IDENT then Error(IDENT); end if;
      Enter(PROC, line, sym, varcount, level, table_index);
      Get_Symbol(sym);
      Block(code, sym, level + 1, table_index);
      if sym /= SEMICOLON then Error(SEMICOLON); end if;
      Get_Symbol(sym);
    end loop;
    while sym = ARRAYSYM loop
      loop
        Get_Symbol(sym);
        if sym /= IDENT then Error(IDENT); end if;
        Get_Symbol(sym);
        if sym /= LBRACKET then Error(ARRAY_DECLARATION_REQUIRES_BRACKETS); end if;
        Get_Symbol(sym);
        if sym /= NUM then Error(NUMBER_EXPECTED); end if;
        size := Table_Range(number);
        Get_Symbol(sym);
        if sym /= RBRACKET then Error(ARRAY_DECLARATION_REQUIRES_BRACKETS); end if;
        Enter(ARR, line, sym, varcount, level, table_index, size);
        exit when sym /= COMMA;
      end loop;
      if sym /= SEMICOLON then Error(SEMICOLON); end if;
      Get_Symbol(sym);
    end loop;
  end loop;
  
  code(symbol_table(start_table_index).adr).data := Code_index;
  symbol_table(start_table_index).adr:= code_index;
  start_code_index := code_index;
  Gen(code, INT, 0, varcount, To_Bounded_String("BEGIN"));
  Statement(code, sym, level, table_index);
  Gen(code, OPR, 0, 0, To_Bounded_String("END"));

  New_Line(1);
  Show_PCode(code, start_code_index, code_index - 1);
end Block;

-- @summary compile using standard input
-- @param code list of p-code instructions to write to
procedure Compile(code: out PCode_Table) is
  sym: Symbol := MAX_SYMBOL;
  level: Integer := 0;
  table_index: Symbol_Table_Range := 0;
begin
  Initialize_Keyword_Map;
  Get_Symbol(sym);
  Block(code, sym, level, table_index);
  New_Line(1);
  Put("Successful compilation!");
  New_Line(1);
end Compile;

-- stack constants and types
max_stack: constant := 1000;
type Stack_Type is array(Table_Range) of Integer;

-- for multitasking/multiprocessing:
-- maximum number of threads
max_threads: constant := 15;
-- fnumber/range of stacks
subtype Thread_Range is Natural range 0..max_threads;

-- our stack type, aliased so we can have access (pointers)
type All_Stacks is array(Thread_Range) of aliased Stack_Type;
stacks: All_Stacks;

-- program register for each stack/thread
progregs: array(Thread_Range) of Table_Range;
-- base register for each stack/thread
baseregs: array(Thread_Range) of Table_Range;
-- top of stack for each thread
tops:     array(Thread_Range) of Table_Range;

-- @summary instantiation of generic package for ordered sets
-- @description We use this to keep track of which threads are available for work
-- (idling, that is). Stacks not in this set should be currently executing.
package Thread_Set is new Ada.Containers.Ordered_Sets(Thread_Range);
use Thread_Set;
threads_avail: Set;

-- information on who called a procedure or function
type Caller_Data is record
  stack:    Thread_Range; -- which stack called it
  location: Table_Range;  -- where to return in the program code
end record;

-- @summary finds the "base" frame that contains "global" data for a procedure or function
-- @param level how far down in the stack to go, relative to current
--    (this may involve switching stacks, so see other parameters)
-- @param basereg current base register
-- @param which_stack stack belonging to the current thread
-- @param stacks all the stacks, needed sometimes to find global data
-- @return the stack that called this function,
--     and the next p-code location to execute
function Base(
  level: Integer;
  basereg: Table_Range;
  which_stack: Thread_Range;
  stacks: in out All_Stacks
) return Caller_Data is
  lev: Integer := level;
  new_stack: Thread_Range := which_stack;
  stack: access Stack_Type;
  new_base: Table_Range;
  base1: Table_Range := basereg;
  result: Caller_Data;
begin
  if (base1 = 0 and which_stack /= 1) then
    lev := lev + 1;
  end if;
  while lev > 0 loop
    stack := stacks(new_stack)'Access;
    new_base := Table_Range(stack.all(Table_Range(base1)));
    new_stack := Thread_Range(stack.all(base1 + 4));
    base1 := new_base;
    if base1 = 0 and new_stack /= 1 then
      lev := lev + 1;
    end if;
    lev := lev - 1;
  end loop;
  result := (new_stack, base1);
  return result;
end Base;

-- @summary runs the on the given p-codes
-- @description The stack's first 5 registers should already be set up in order:
-- (0) base register of global variables;
-- (1)current base register;
-- (2)program register to return to;
-- (3)stack calling this one;
-- (4)stack that called the calling stack.
-- @param codes table of p-codes to execute
-- @param which_stack which stack to use
procedure Run_Stack(codes: in PCode_Table; which_stack: Thread_Range := 1) is
  stack: access Stack_Type;
  current_stack: Thread_Range := which_stack;
  progreg, top, basereg: Table_Range;
  code: PCode_Entry;
  caller: Caller_Data;
begin
  New_Line(1);
  Put("Start PL/0 #"); Put(Positive(current_stack), 2);
  New_Line(1);

  stack   := stacks   (current_stack)'Access;
  progreg := progregs (current_stack);
  top     := tops     (current_stack);
  basereg := baseregs (current_stack);

  loop
    code := codes(progreg);
    progreg := progreg + 1;
    case code.instruction is
      when LIT =>
        top := top + 1;
        stack.all(top) := code.data;
      when OPR =>
        case code.data is
          when 0 =>
            top           := (if basereg = 0 then 0 else basereg - 1);
            basereg       := Table_Range(stack.all(top + 2));
            progreg       := Table_Range(stack.all(top + 3));
            current_stack := Thread_Range(stack.all(top + 4));
          when 1 => stack.all(top) := -stack.all(top);
          when 2 =>
            top := top - 1;
            stack.all(top) := stack.all(top) + stack.all(top + 1);
          when 3 =>
            top := top - 1;
            stack.all(top) := stack.all(top) - stack.all(top + 1);
          when 4 =>
            top := top - 1;
            stack.all(top) := stack.all(top) * stack.all(top + 1);
          when 5 =>
            top := top - 1;
            stack.all(top) := stack.all(top) / stack.all(top + 1);
          when 6 => stack.all(top) := stack.all(top) mod 2;
          when 8 =>
            top := top - 1;
            stack.all(top) := (
                if Stack.all(top) = Stack.all(top + 1) then 1 else 0
            );
          when 9 =>
            top := top - 1;
            stack.all(top) := (
                if Stack.all(top) /= Stack.all(top + 1) then 1 else 0
            );
          when 10 =>
            top := top - 1;
            stack.all(top) := ( if stack.all(top) < stack.all(top + 1) then 1 else 0 );
          when 11 =>
            top := top - 1;
            stack.all(top) := ( if stack.all(top) >= stack.all(top + 1) then 1 else 0 );
          when 12 =>
            top := top - 1;
            stack.all(top) := ( if stack.all(top) > stack.all(top + 1) then 1 else 0 );
          when 13 =>
            top := top - 1;
            stack.all(top) := ( if stack.all(top) <= stack.all(top + 1) then 1 else 0 );
          when 14 =>
            Put(stack.all(top), 0); Put(' ');
            top := top - 1;
          when 15 => New_Line(1);
          when others => stack.all(top) := stack.all(top);
        end case;
      when LOD =>
        top := top + 1;
        caller := Base(code.level, basereg, current_stack, stacks);
        stack.all(top) := stacks(caller.stack)(caller.location + Table_Range(code.data));
      when LDX =>
        caller := Base(code.level, basereg, current_stack, stacks);
        stack.all(top) := stacks(caller.stack)(caller.location + Table_Range(code.data) + Table_Range(Stack.all(top)));
      when STO =>
        caller := Base(code.level, basereg, current_stack, stacks);
        stacks(caller.stack)(caller.location + Table_Range(code.data)) := stack.all(top);
        top := top - 1;
      when STX =>
        caller := Base(code.level, basereg, current_stack, stacks);
        stacks(caller.stack)(caller.location + Table_Range(code.data) + Table_Range(Stack.all(top - 1))) := stack.all(top);
        top := top - 2;
      when CAL =>
        caller := Base(code.level, basereg, current_stack, stacks);
        stack.all(top + 1) := caller.location;
        stack.all(top + 2) := basereg;
        stack.all(top + 3) := progreg;
        stack.all(top + 4) := current_stack;
        stack.all(top + 5) := caller.stack;
        basereg := top + 1;
        progreg := Table_Range(code.data);
      when INT => top := Table_Range(Integer(top) + code.data);
      when JMP => progreg := Table_Range(code.data);
      when JPC =>
        if stack.all(top) = 0 then
          progreg := Table_Range(code.data);
        end if;
        top := top - 1;
      when CTS =>
        top := top + 1;
        stack.all(top) := stack.all(top - 1);
      when SNT =>
        stack.all(top) := stack.all(top); -- todo
      when RFT =>
        stack.all(top) := stack.all(top); -- todo
      when WFT =>
        stack.all(top) := stack.all(top); -- todo
      when THN =>
        top := top + 1;
        stack.all(top) := current_stack;
    end case;
    exit when progreg = 0;
  end loop;

  Insert(threads_avail, current_stack);
  Put("End PL/0 #"); Put(current_stack, 2);
  New_Line(1);
end Run_Stack;

-- @summary interpret the compiled program
-- @param codes table of p-codes
procedure Interpret(codes: in PCode_Table) is
  stack0: access Stack_Type;
begin
  for i in Thread_Range loop
    Insert(threads_avail, i);
    progregs(i) := 0;
    baseregs(i) := 0;
    tops(i) := 0;
  end loop;
  stack0 := stacks(0)'Access;
  stack0.all(0) := 0;
  stack0.all(1) := 0;
  stack0.all(2) := 0;
  stack0.all(3) := 0;
  stack0.all(4) := 1;
  stack0.all(5) := 1;
  tops(0) := 1;
  Delete(threads_avail, 0);
  Run_Stack(codes);
end Interpret;

end Compiler;