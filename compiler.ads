--
-- Interface for PL/0 Compiler written in Ada
-- by John Perry, 2018
--

-- import this package
with Ada.Strings.Bounded;

package Compiler is

-- we must pre-declare this type, but that doesn't mean we have to expose it
type PCode_Table is private;

-- @summary call this to compile from standard input
-- @description Compiled code will be placed in code,
-- which you can then Interpret().
-- Generated p-codes will automatically be shown during compilation.
-- ("out" means that the parameter will be modified)
procedure Compile(code: out PCode_Table);

-- @summary call this to interpret the generated p-codes
--
-- ("in" means the parameter will be read but not modified;
-- this is strictly enforced)
procedure Interpret(codes: PCode_Table);

private

-- enumeration of all pcodes
-- @value OPR some operation
-- @value CAL call a subroutine
-- @value INT increase the top of stack
-- @value JPC conditional jump
-- @value JMP unconditional jump
-- @value LIT place literal onto top of stack
-- @value LOD load onto top of stack value found elsewhere
-- @value STO store top of stack somewhere
-- @value CTS copy top of stack onto top of stack
-- @value SNT spawn new thread
-- @value RFT return from thread
-- @value WFT wait for thread
-- @value THN thread number
-- @value STX store at offset
-- @value LDX load from offset
type PCode is (
  OPR, CAL, INT, JPC, JMP, LIT, LOD, STO,
  CTS, SNT, RFT, WFT, THN, STX, LDX
);

-- We generally use bounded strings;
-- this was silly in hindsight but it made some sort of sense at the time
-- maximum string length
strlen: constant := 80;
-- Bounded_String is a generic package; we instantiate the *package*
package String_Max80 is new Ada.Strings.Bounded.Generic_Bounded_Length(strlen);
-- this next statement saves us from having to prefix
-- every Bounded_String statement as "String_Max80.whatever"
use String_Max80;

-- entry to the table of p-codes
type PCode_Entry is record
  instruction: PCode;
  level:       Integer;        -- instruction a appropriate p-code instruction
  data:        Integer;        -- level the relation of the data
                               -- with respect to the current stack frame
  comment:     Bounded_String; -- comment an optional string
                               -- explaining what is going on
end record;

-- How large the program is allowed to be (in terms of p-codes)
max_table_size: constant := 999;
-- defining this subtype helps the compiler check for errors
subtype Table_Range is Natural range 0..max_table_size;
-- the list of p-codes
type PCode_Table is array(Table_Range) of PCode_Entry;

end Compiler;