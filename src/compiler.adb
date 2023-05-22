with Cradle;
with Reader;

package body Compiler is
   Label_Count : Natural := 0;

   function New_Label return String is
      Label : constant String := "L"
                                 & Cradle.Integer_To_String (I => Label_Count);
   begin
      Label_Count := Label_Count + 1;
      return Label;
   end New_Label;

   procedure Post_Label (L : String) is
   begin
      Cradle.Emit_Line (S => L & ":");
   end Post_Label;

   procedure Identifier is
      Name : constant String := Reader.Get_Name;
   begin
      if Reader.Look = '(' then
         Reader.Match (X => '(');
         Reader.Match (X => ')');
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
      if Reader.Look = '(' then
         Reader.Match (X => '(');
         Expression;
         Reader.Match (X => ')');
      elsif Cradle.Is_Alpha (X => Reader.Look) then
         Identifier;
      else
         Cradle.Emit_Line (S => "MOVE #"
                                & Reader.Get_Num
                                & ", D0");
      end if;
      Cradle.Exit_Fn (Fn_Name => "Factor");
   end Factor;

   procedure Multiply is
   begin
      Cradle.Enter_Fn (Fn_Name => "Multiply");
      Reader.Match (X => '*');
      Factor;
      Cradle.Emit_Line (S => "MULS (SP)+, D0");
      Cradle.Enter_Fn (Fn_Name => "Multiply");
   end Multiply;

   procedure Divide is
   begin
      Reader.Match (X => '/');
      Factor;
      Cradle.Emit_Line (S => "DIVS D0, -(SP)");
   end Divide;

   procedure Term is
   begin
      Cradle.Enter_Fn (Fn_Name => "Term");
      Factor;
      Mulop_Loop :
      while Reader.Look in '*' | '/' loop
         Cradle.Emit_Line (S => "MOVE D0, -(SP)");
         if Reader.Look = '*' then
            Multiply;
         elsif Reader.Look = '/' then
            Divide;
         end if;
      end loop Mulop_Loop;
      Cradle.Exit_Fn (Fn_Name => "Term");
   end Term;

   procedure Add is
   begin
      Cradle.Enter_Fn (Fn_Name => "Add");
      Reader.Match (X => '+');
      Term;
      Cradle.Emit_Line (S => "ADD (SP)+, D0");
      Cradle.Exit_Fn (Fn_Name => "Add");
   end Add;

   procedure Subtract is
   begin
      Reader.Match (X => '-');
      Term;
      Cradle.Emit_Line (S => "SUB (SP)+, D0");
      Cradle.Emit_Line (S => "NEG D0");
   end Subtract;

   procedure Expression is
   begin
      Cradle.Enter_Fn (Fn_Name => "Expression");
      if Cradle.Is_Addop (Reader.Look) then
         Cradle.Emit_Line ("CLR D0");
      else
         Term;
      end if;

      Addop_Loop :
      while Cradle.Is_Addop (Reader.Look) loop
         Cradle.Emit_Line (S => "MOVE D0, -(SP)");
         if Reader.Look = '+' then
            Add;
         elsif Reader.Look = '-' then
            Subtract;
         end if;
      end loop Addop_Loop;

      if not Reader.Is_End_Of_Line then
         Cradle.Expected (S => "Newline");
      end if;
      Cradle.Exit_Fn (Fn_Name => "Expression");
   end Expression;

   procedure Assignment is
      Name : constant String := Reader.Get_Name;
   begin
      Reader.Match (X => '=');
      Expression;
      Cradle.Emit_Line (S => "LEA "
                             & Name
                             & "(PC), A0");
      Cradle.Emit_Line (S => "MOVE D0, (A0)");
   end Assignment;

   procedure Other is
   begin
      Cradle.Enter_Fn (Fn_Name => "Other");
      Cradle.Emit_Line (S => Reader.Get_Name);
      Cradle.Exit_Fn (Fn_Name => "Other");
   end Other;

   procedure Condition is
   begin
      Cradle.Emit_Line (S => "<condition>");
   end Condition;

   procedure Do_If is
      L : constant String := New_Label;
   begin
      Cradle.Enter_Fn (Fn_Name => "Do_If");
      Reader.Match (X => 'i');
      Condition;
      Cradle.Emit_Line (S => "BEQ " & L);
      Block;
      Reader.Match (X => 'e');
      Post_Label (L => L);
      Cradle.Exit_Fn (Fn_Name => "Do_If");
   end Do_If;

   procedure Block is
   begin
      Cradle.Enter_Fn (Fn_Name => "Block");
      Block_Loop :
      loop
         exit Block_Loop when Reader.Look = 'e';
         --  Cradle.Emit_Line ("1 Look is " & Reader.Look);
         if Reader.Look = 'i' then
            Do_If;
         else
            Other;
         end if;
         --  Cradle.Emit_Line ("2 Look is " & Reader.Look);
         Reader.Consume_New_Line;
         --  Init;
         --  Cradle.Emit_Line ("3 Look is " & Reader.Look);
      end loop Block_Loop;
      Cradle.Exit_Fn (Fn_Name => "Block");
   end Block;

   procedure Program is
   begin
      Cradle.Enter_Fn (Fn_Name => "Program");
      Block;
      if Reader.Look /= 'e' then
         Cradle.Expected (S => "End");
      end if;
      Cradle.Emit_Line (S => "END");
      Cradle.Exit_Fn (Fn_Name => "Program");
   end Program;

   procedure Init is
   begin
      Cradle.Enter_Fn (Fn_Name => "Init");
      Reader.Get_Char;
      Reader.Skip_Whitespace;
      Cradle.Exit_Fn (Fn_Name => "Init");
   end Init;
end Compiler;
