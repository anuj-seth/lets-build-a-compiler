with Ada.Command_Line;
with Ada.Text_IO;

with Cradle;
with Compiler;
with Interpreter;
with Calculator;

procedure Main is
   package CMD renames Ada.Command_Line;
begin
   if CMD.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("Mode required: "
                            & "interpreter or compiler or calculator");
      return;
   end if;

   declare
      Mode : constant String := CMD.Argument (1);
   begin
      if Mode = "compiler" then
         --  Parser.Assignment;
         Compiler.Program;
      elsif Mode = "interpreter" then
         Interpreter.Run;
      elsif Mode = "calculator" then
         Calculator.Run;
      else
         Ada.Text_IO.Put_Line ("Invalid mode argument."
                               & " Only compiler or interpreter "
                               & "or calculator supported");
      end if;
   end;
exception
   when Cradle.Halt_Exception =>
      null;
end Main;
