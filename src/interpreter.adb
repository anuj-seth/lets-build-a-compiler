with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Characters.Handling;

with Cradle;
with Reader;

package body Interpreter is
   package TIO renames Ada.Text_IO;
   package CH renames Ada.Characters.Handling;

   function Factor return Integer is
      Value : Integer;
   begin
      if Reader.Look = '(' then
         Reader.Match (X => '(');
         Value := Expression;
         Reader.Match (X => ')');
      elsif Cradle.Is_Alpha (X => Reader.Look) then
         Value := Table (Reader.Get_Name (1));
      else
         Value := Integer'Value (Reader.Get_Num);
      end if;
      return Value;
   end Factor;

   function Term return Integer is
      Value : Integer := Factor;
   begin
      Cradle.Enter_Fn (Fn_Name => "Term");
      while Cradle.Is_Mulop (X => Reader.Look) loop
         if Reader.Look = '*' then
            Reader.Match (X => '*');
            Value := Value * Factor;
         elsif Reader.Look = '/' then
            Reader.Match (X => '/');
            Value := Value / Factor;
         end if;
      end loop;
      Cradle.Exit_Fn (Fn_Name => "Term");
      return Value;
   end Term;

   function Expression return Integer is
      Value : Integer := 0;
   begin
      Cradle.Enter_Fn (Fn_Name => "Expression");
      if not Cradle.Is_Addop (X => Reader.Look) then
         Value := Term;
      end if;

      while Cradle.Is_Addop (X => Reader.Look) loop
         if Reader.Look = '+' then
            Reader.Match (X => '+');
            Value := Value + Term;
         elsif Reader.Look = '-' then
            Reader.Match (X => '-');
            Value := Value - Term;
         end if;
      end loop;

      Cradle.Exit_Fn (Fn_Name => "Expression");
      return Value;
   end Expression;

   procedure Assignment is
      Name : constant Character := Reader.Get_Name (1);
   begin
      Reader.Match (X => '=');
      Table (Name) := Expression;
   end Assignment;

   procedure Init is
   begin
      Reader.Get_Char;
      Reader.Skip_Whitespace;
   end Init;

   procedure Newline is
   begin
      Reader.Consume_New_Line;
      Init;
   end Newline;

   procedure Output is
      Var_Name : Character;
   begin
      Reader.Match (X => '!');
      Var_Name := Reader.Get_Name (1);
      Ada.Text_IO.Put (Var_Name & " is ");
      Ada.Integer_Text_IO.Put (Table (Var_Name));
      Ada.Text_IO.New_Line;
   end Output;

   procedure Input is
      Var_Name : Character;
   begin
      Reader.Match ('?');
      Var_Name := Reader.Get_Name (1);
      Ada.Text_IO.Put_Line ("var name is " & Var_Name);
   end Input;

   procedure Run is
   begin
      Init;
      Interpreter_Loop :
      loop
         if Reader.Look = '?' then
            Input;
         elsif Reader.Look = '!' then
            Output;
            Newline;
         else
            exit Interpreter_Loop when Reader.Look = '.';
            Assignment;
            Newline;
         end if;
      end loop Interpreter_Loop;
   end Run;
end Interpreter;
