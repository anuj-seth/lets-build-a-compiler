with Ada.Text_IO;
with Ada.Characters.Handling;

with Cradle;

package body Parser is
   package TIO renames Ada.Text_IO;
   package CH renames Ada.Characters.Handling;

   procedure Get_Char is
   begin
      if not TIO.End_Of_Line then
         TIO.Get (Item => Look);
      end if;
   end Get_Char;

   procedure Match (X : Character) is
   begin
      if Look = X then
         Get_Char;
      else
         Cradle.Expected ("'" & X & "'");
      end if;
   end Match;

   function Get_Name return Character is
      Result : Character;
   begin
      if not Cradle.Is_Alpha (Look) then
         Cradle.Expected ("Name");
      end if;

      Result := CH.To_Upper (Look);
      Get_Char;
      return Result;
   end Get_Name;

   function Get_Num return Character is
      Result : Character;
   begin
      if not Cradle.Is_Digit (Look) then
         Cradle.Expected ("Integer");
      end if;

      Result := Look;
      Get_Char;
      return Result;
   end Get_Num;

   procedure Term is
   begin
      Cradle.Emit_Line ("Move #"
                        & Get_Num
                        & ",D0");
   end Term;

   procedure Add is
   begin
      Match ('+');
      Term;
      Cradle.Emit_Line ("ADD (SP)+, D0");
   end Add;

   procedure Subtract is
   begin
      Match ('-');
      Term;
      Cradle.Emit_Line ("SUB (SP)+, D0");
      Cradle.Emit_Line ("NEG D0");
   end Subtract;

   procedure Expression is
   begin
      Term;
      Operator_Loop :
      while Look = '+' or else Look = '-' loop
         Cradle.Emit_Line ("Move D0, -(SP)");
         case Look is
            when '+' =>
               Add;
            when '-' =>
               Subtract;
            when others =>
               Cradle.Expected ("Addop");
         end case;
      end loop Operator_Loop;
   end Expression;

   procedure Init is
   begin
      Get_Char;
   end Init;
end Parser;
