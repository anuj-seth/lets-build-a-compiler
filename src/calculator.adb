with Ada.Text_IO;
with Ada.Integer_Text_IO;

with Reader;

package body Calculator is
   package IIO renames Ada.Integer_Text_IO;
   package TIO renames Ada.Text_IO;

   function Factor return Integer is
      Value : Integer;
   begin
      if Reader.Look = '(' then
         Reader.Match (X => '(');
         Value := Expression;
         Reader.Match (X => ')');
      else
         Value := Integer'Value (Reader.Get_Num);
      end if;
      return Value;
   end Factor;

   function Term return Integer is
      Value : Integer;
   begin
      Value := Factor;
      if Reader.Look = '*' then
         Reader.Match (X => '*');
         Value := Value * Term;
      elsif Reader.Look = '/' then
         Reader.Match (X => '/');
         Value := Value / Term;
      end if;
      return Value;
   end Term;

   function Expression return Integer is
      Value : Integer;
   begin
      Value := Term;
      Addops_Loop :
      while Reader.Look in '+' | '-' loop
         if Reader.Look = '+' then
            Reader.Match (X => '+');
            Value := Value + Term;
         elsif Reader.Look = '-' then
            Reader.Match (X => '-');
            Value := Value - Term;
         end if;
      end loop Addops_Loop;
      return Value;
   end Expression;

   procedure Init is
   begin
      Reader.Get_Char;
      Reader.Skip_Whitespace;
   end Init;

   procedure Run is
   begin
      Init;
      IIO.Put (Item => Expression);
      TIO.New_Line;
   end Run;
end Calculator;
