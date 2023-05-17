with Ada.Command_Line;
with Ada.Text_IO;

with Cradle;
with Parser;
with Interpreter;

procedure Main is
   package CMD renames Ada.Command_Line;
begin
   if CMD.Argument_Count < 1 then
      Ada.Text_IO.Put_Line ("Mode required: interpreter or compiler");
      return;
   end if;

   declare
      Mode : constant String := CMD.Argument (1);
   begin
      if Mode = "compiler" then
         Parser.Init;
         --  Parser.Assignment;
         Parser.Program;
      elsif Mode = "interpreter" then
         Interpreter.Run;
      else
         Ada.Text_IO.Put_Line ("Invalid mode argument." &
                               " Only compiler or interpreter supported");
      end if;
   end;
exception
   when Cradle.Halt_Exception =>
      null;
end Main;
