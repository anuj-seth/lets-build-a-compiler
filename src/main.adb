with Ada.Text_IO;
with Ada.Integer_Text_IO;

with Cradle;
with Parser;
with Interpreter;

procedure Main is
begin
   --  Parser.Init;
   --  Parser.Assignment;
   Interpreter.Run;
exception
   when Cradle.Halt_Exception =>
      null;
end Main;
