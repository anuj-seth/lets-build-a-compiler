with Ada.Text_IO;
with Ada.Characters.Handling;

with Cradle;

package body Parser is
   package TIO renames Ada.Text_IO;
   package CH renames Ada.Characters.Handling;

   function Is_End_Of_Line return Boolean is
      (TIO.End_Of_Line);

   procedure Get_Char is
   begin
      if Is_End_Of_Line then
         Look := Cradle.End_Of_Line_Character;
      else
         TIO.Get (Item => Look);
      end if;
   end Get_Char;

   procedure Match (X : Character) is
   begin
      Cradle.Enter_Fn (Fn_Name => "Match");
      if Look = X then
         Get_Char;
      else
         Cradle.Expected (S => "'" & X & "'");
      end if;
      Cradle.Exit_Fn (Fn_Name => "Match");
   end Match;

   function Get_Name return Character is
      Result : Character;
   begin
      if not Cradle.Is_Alpha (X => Look) then
         Cradle.Expected (S => "Name");
      end if;

      Result := CH.To_Upper (Item => Look);
      Get_Char;
      return Result;
   end Get_Name;

   function Get_Num return Character is
      Result : Character;
   begin
      if not Cradle.Is_Digit (X => Look) then
         Cradle.Expected (S => "Integer");
      end if;

      Result := Look;
      Get_Char;
      return Result;
   end Get_Num;

   procedure Identifier is
      Name : constant Character := Get_Name;
   begin
      if Look = '(' then
         Match (X => '(');
         Match (X => ')');
         Cradle.Emit_Line (S => "BSR " & Name);
      else
         Cradle.Emit_Line (S => "MOVE "
                                & Name
                                & "(PC), D0");
      end if;
   end Identifier;

   procedure Factor is
   begin
      Cradle.Enter_Fn (Fn_Name => "Factor");
      if Look = '(' then
         Match (X => '(');
         Expression;
         Match (X => ')');
      elsif Cradle.Is_Alpha (X => Look) then
         Identifier;
      else
         Cradle.Emit_Line (S => "MOVE #"
                                & Get_Num
                                & ", D0");
      end if;
      Cradle.Exit_Fn (Fn_Name => "Factor");
   end Factor;

   procedure Multiply is
   begin
      Cradle.Enter_Fn (Fn_Name => "Multiply");
      Match (X => '*');
      Factor;
      Cradle.Emit_Line (S => "MULS (SP)+, D0");
      Cradle.Enter_Fn (Fn_Name => "Multiply");
   end Multiply;

   procedure Divide is
   begin
      Match (X => '/');
      Factor;
      Cradle.Emit_Line (S => "DIVS D0, -(SP)");
   end Divide;

   procedure Term is
   begin
      Cradle.Enter_Fn (Fn_Name => "Term");
      Factor;
      Mulop_Loop :
      while Look = '*' or else Look = '/' loop
         Cradle.Emit_Line (S => "Move D0, -(SP)");
         if Look = '*' then
            Multiply;
         elsif Look = '/' then
            Divide;
         end if;
      end loop Mulop_Loop;
      Cradle.Exit_Fn (Fn_Name => "Term");
   end Term;

   procedure Add is
   begin
      Cradle.Enter_Fn (Fn_Name => "Add");
      Match (X => '+');
      Term;
      Cradle.Emit_Line (S => "ADD (SP)+, D0");
      Cradle.Exit_Fn (Fn_Name => "Add");
   end Add;

   procedure Subtract is
   begin
      Match (X => '-');
      Term;
      Cradle.Emit_Line (S => "SUB (SP)+, D0");
      Cradle.Emit_Line (S => "NEG D0");
   end Subtract;

   function Is_Addop (C : Character) return Boolean is
      (C = '+' or else C = '-');

   procedure Expression is
   begin
      Cradle.Enter_Fn (Fn_Name => "Expression");
      if Is_Addop (Look) then
         Cradle.Emit_Line ("CLR D0");
      else
         Term;
      end if;

      Addop_Loop :
      while Is_Addop (Look) loop
         Cradle.Emit_Line (S => "Move D0, -(SP)");
         if Look = '+' then
            Add;
         elsif Look = '-' then
            Subtract;
         end if;
      end loop Addop_Loop;

      if not Is_End_Of_Line then
         Cradle.Expected (S => "Newline");
      end if;
      Cradle.Exit_Fn (Fn_Name => "Expression");
   end Expression;

   procedure Init is
   begin
      Get_Char;
   end Init;
end Parser;
