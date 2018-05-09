with Compiler; use Compiler;

procedure Compiler_Main is
  pcode: PCode_Table;
begin
  Compile(pcode);
  Interpret(pcode);
end Compiler_Main;