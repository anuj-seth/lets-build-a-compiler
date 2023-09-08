with Cradle;
with Reader;

package body Compiler is

   overriding
   procedure Initialize (F : in out Frame) is
   begin
      Cradle.Emit_Line (S => "push {fp, lr}");
      Cradle.Emit_Line (S => "add fp, sp, #4");
   end Initialize;

   overriding
   procedure Finalize (F : in out Frame) is
   begin
      Cradle.Emit_Line (S => "sub sp, fp, #4");
      Cradle.Emit_Line (S => "pop {fp, lr}");
      Cradle.Emit_Line (S => "bx lr");
   end Finalize;

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

   procedure Equals (Current_Frame : in out Frame) is
   begin
      Reader.Match (X => '=');
      Expression (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "pop {r1}");
      Cradle.Emit_Line (S => "cmp r0, r1");
      Cradle.Emit_Line (S => "eor r0, r0");
      Cradle.Emit_Line (S => "moveq r0, #-1");
   end Equals;

   procedure Not_Equals (Current_Frame : in out Frame) is
   begin
      Reader.Match (X => '#');
      Expression (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "pop {r1}");
      Cradle.Emit_Line (S => "cmp r0, r1");
      Cradle.Emit_Line (S => "eor r0, r0");
      Cradle.Emit_Line (S => "movne r0, #-1");
   end Not_Equals;

   procedure Less (Current_Frame : in out Frame) is
   begin
      Reader.Match (X => '<');
      Expression (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "pop {r1}");
      Cradle.Emit_Line (S => "cmp r1, r0");
      Cradle.Emit_Line (S => "eor r0, r0");
      Cradle.Emit_Line (S => "movlt r0, #-1");
   end Less;

   procedure Greater (Current_Frame : in out Frame) is
   begin
      Reader.Match (X => '>');
      Expression (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "pop {r1}");
      Cradle.Emit_Line (S => "cmp r1, r0");
      Cradle.Emit_Line (S => "eor r0, r0");
      Cradle.Emit_Line (S => "movgt r0, #-1");
   end Greater;

   procedure Relation (Current_Frame : in out Frame) is
   begin
      Expression (Current_Frame => Current_Frame);
      if Cradle.Is_Relop (X => Reader.Look) then
         Cradle.Emit_Line (S => "push {r0}");
         if Reader.Look = '=' then
            Equals (Current_Frame => Current_Frame);
         elsif Reader.Look = '#' then
            Not_Equals (Current_Frame => Current_Frame);
         elsif Reader.Look = '>' then
            Greater (Current_Frame => Current_Frame);
         elsif Reader.Look = '<' then
            Less (Current_Frame => Current_Frame);
         end if;
         Cradle.Emit_Line (S => "tst r0, #0");
      end if;
   end Relation;

   procedure Identifier (Current_Frame : in out Frame) is
      Name : constant String := Reader.Get_Name;
   begin
      if Reader.Look = '(' then
         Reader.Match (X => '(');
         Reader.Match (X => ')');
         Cradle.Emit_Line (S => "bl " & Name);
      else
         declare
            Offset : constant Integer :=
               Current_Frame.Variable_Offsets (Name);
            Offset_String : constant String :=
               Cradle.Integer_To_String (I => Offset);
         begin
            Cradle.Emit_Line (S => "ldr r0, [fp, #-"
                                   & Offset_String
                                   & "]");
         exception
            when Constraint_Error =>
               Cradle.Halt (S => "Unknown variable name "
                                 & Name);
         end;
      end if;
   end Identifier;

   procedure Factor (Current_Frame : in out Frame) is
   begin
      if Reader.Look = '(' then
         Reader.Match (X => '(');
         Expression (Current_Frame => Current_Frame);
         Reader.Match (X => ')');
      elsif Cradle.Is_Alpha (X => Reader.Look) then
         Identifier (Current_Frame => Current_Frame);
      else
         Cradle.Emit_Line (S => "mov r0, #"
                                & Reader.Get_Num);
      end if;
   end Factor;

   procedure Signed_Factor (Current_Frame : in out Frame) is
   begin
      if Cradle.Is_Addop (Reader.Look) then
         Cradle.Emit_Line (S => "mov r0, #0"
                                & " @ leading zero for unary operators");
         Cradle.Emit_Line (S => "push {r0}");
         if Reader.Look = '+' then
            Add (Current_Frame => Current_Frame);
         else
            Subtract (Current_Frame =>  Current_Frame);
         end if;
      else
         Factor (Current_Frame => Current_Frame);
      end if;
   end Signed_Factor;

   procedure Multiply (Current_Frame : in out Frame) is
   begin
      Reader.Match (X => '*');
      Factor (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "pop {r1}");
      Cradle.Emit_Line (S => "mul r0, r1");
   end Multiply;

   procedure Divide (Current_Frame : in out Frame) is
   begin
      Reader.Match (X => '/');
      Factor (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "pop {r1}");
      Cradle.Emit_Line (S => "sdiv r0, r1, r0");
   end Divide;

   procedure Term (Current_Frame : in out Frame) is
   begin
      Signed_Factor (Current_Frame => Current_Frame);

      Mulop_Loop :
      while Cradle.Is_Mulop (Reader.Look) loop
         Cradle.Emit_Line (S => "push {r0}");
         if Reader.Look = '*' then
            Multiply (Current_Frame => Current_Frame);
         elsif Reader.Look = '/' then
            Divide (Current_Frame => Current_Frame);
         end if;
      end loop Mulop_Loop;
   end Term;

   procedure Add (Current_Frame : in out Frame) is
   begin
      Reader.Match  (X => '+');
      Term (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "pop {r1}");
      Cradle.Emit_Line (S => "add r0, r1");
   end Add;

   procedure Subtract (Current_Frame : in out Frame) is
   begin
      Reader.Match (X => '-');
      Term (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "pop {r1}");
      Cradle.Emit_Line (S => "sub r0, r1, r0");
   end Subtract;

   procedure Expression (Current_Frame : in out Frame) is
   begin
      Current_Frame.Variable_Offsets.Include ("b", 4);
      Term (Current_Frame => Current_Frame);

      Addop_Loop :
      while Cradle.Is_Addop (Reader.Look) loop
         Cradle.Emit_Line (S => "push {r0}");
         if Reader.Look = '+' then
            Add (Current_Frame => Current_Frame);
         elsif Reader.Look = '-' then
            Subtract (Current_Frame => Current_Frame);
         end if;
      end loop Addop_Loop;

      --  TODO uncomment once multiline code is supported ?
      --  if not Reader.Is_End_Of_Line then
      --   Cradle.Expected (S => "Newline");
      --  end if;
   end Expression;

   procedure Boolean_Factor (Current_Frame : in out Frame) is
   begin
      if not Cradle.Is_Boolean (X => Reader.Look) then
         Cradle.Expected (S => "Boolean literal");
      end if;
      if Cradle.Is_Boolean (X => Reader.Look) then
         if Reader.Get_Boolean then
            Cradle.Emit_Line (S => "mov r0, #-1");
         else
            Cradle.Emit_Line (S => "mov r0, #0");
         end if;
      else
         Relation (Current_Frame => Current_Frame);
      end if;
   end Boolean_Factor;

   procedure Not_Factor (Current_Frame : in out Frame) is
   begin
      if Reader.Look = '!' then
         Reader.Match (X => '!');
         Boolean_Factor (Current_Frame => Current_Frame);
         Cradle.Emit_Line (S => "mov r1, #-1");
         Cradle.Emit_Line (S => "eor r0, r1");
      else
         Boolean_Factor (Current_Frame => Current_Frame);
      end if;
   end Not_Factor;

   procedure Boolean_Term (Current_Frame : in out Frame) is
   begin
      Not_Factor (Current_Frame => Current_Frame);
      while Reader.Look = '&' loop
         Cradle.Emit_Line (S => "push {r0}");
         Reader.Match (X => '&');
         Not_Factor (Current_Frame => Current_Frame);
         Cradle.Emit_Line (S => "pop {r1}");
         Cradle.Emit_Line (S => "and r0, r1");
      end loop;
   end Boolean_Term;

   procedure Boolean_Or (Current_Frame : in out Frame) is
   begin
      Reader.Match (X => '|');
      Boolean_Term (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "pop {r1}");
      Cradle.Emit_Line (S => "orr r0, r1");
   end Boolean_Or;

   procedure Boolean_Xor (Current_Frame : in out Frame) is
   begin
      Reader.Match (X => '~');
      Boolean_Term (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "pop {r1}");
      Cradle.Emit_Line (S => "eor r0, r1");
   end Boolean_Xor;

   procedure Boolean_Expression (Current_Frame : in out Frame) is
   begin
      Boolean_Term (Current_Frame => Current_Frame);
      while Cradle.Is_Orop (X => Reader.Look) loop
         Cradle.Emit_Line (S => "push {r0}");
         if Reader.Look = '|' then
            Boolean_Or (Current_Frame => Current_Frame);
         elsif Reader.Look = '~' then
            Boolean_Xor (Current_Frame => Current_Frame);
         end if;
      end loop;
   end Boolean_Expression;

   procedure Assignment (Current_Frame : in out Frame) is
      Name : constant String := Reader.Get_Name;
      Offset : constant Natural :=
         8 + (Current_Frame.Variables_In_Stack * 4);
   begin
      Reader.Match (X => '=');
      --  create space on stack for this  variable
      --  in the final version I will need to create
      --  space for all variables before a function
      --  is compiled
      Cradle.Emit_Line (S => "sub sp, sp, #4");
      Current_Frame.Variable_Offsets.Include (Name, Offset);
      Current_Frame.Variables_In_Stack := Current_Frame.Variables_In_Stack + 1;
      Boolean_Expression (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "str r0, [fp, #-"
                             & Cradle.Integer_To_String (I => Offset)
                             & "]");
   end Assignment;

   procedure Header is
   begin
      Cradle.Emit_Line (S => ".cpu cortex-a15"
                             & "  @ this directive enables "
                             & " support for sdiv/udiv");
      Cradle.Emit_Line (S => ".text");
      Cradle.Emit_Line (S => ".global main");
      Cradle.Emit_Line (S => "main:");
   end Header;

   procedure Trailer is
   begin
      null;
      --  Cradle.Emit_Line (S => "@_zero_exit_code:");
      --  Cradle.Emit_Line (S => "@mov r0, #0");
      --  Cradle.Emit_Line (S => "@_exit:");
      --  Cradle.Emit_Line (S => "@mov r7, #1");
      --  Cradle.Emit_Line (S => "@svc 0");
   end Trailer;

   procedure Init is
   begin
      Reader.Get_Char;
      Reader.Skip_Whitespace;
   end Init;

   procedure Other (Current_Frame : in out Frame) is
   begin
      Cradle.Enter_Fn (Fn_Name => "Other");
      Cradle.Emit_Line (S => Reader.Get_Name);
      Cradle.Exit_Fn (Fn_Name => "Other");
   end Other;

   procedure Do_Do (Current_Frame : in out Frame) is
      L : constant String := New_Label;
      Break_To_This : constant String := New_Label;
   begin
      Reader.Match (X => 'd');
      Expression (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "sub r0, #1");
      Post_Label (L => L);
      Cradle.Emit_Line (S => "push {r0}");
      Block (Current_Frame => Current_Frame,
             Break_To_Label => Break_To_This);
      Cradle.Emit_Line (S => "pop {r0}");
      Cradle.Emit_Line (S => "sub r0, #1");
      Cradle.Emit_Line (S => "cmp r0, #0");
      Cradle.Emit_Line (S => "bne " & L);
      Cradle.Emit_Line (S => "sub sp, #4");
      Post_Label (L => Break_To_This);
      Cradle.Emit_Line (S => "add sp, #4");
   end Do_Do;

   procedure Do_For (Current_Frame : in out Frame) is
      L_1 : constant String := New_Label;
      L_2 : constant String := New_Label;
      Break_To_This : constant String := New_Label;
   begin
      Reader.Match (X => 'f');
      declare
         Name : constant String := Reader.Get_Name;
      begin
         --  the for loop should get it's own frame
         --  since the loop counters have scope only inside loop
         --  or maybe not coz we are not creating a new frame at all
         --  TODO  I need to allocate space for all variables including
         --  loop counters beforehand
         --  I will tackle the for loop later on
         Reader.Match (X => '=');
         Expression (Current_Frame => Current_Frame);
         Cradle.Emit_Line (S => "sub r0, #1   @ predecrement loop counter");
         Cradle.Emit_Line (S => "LEA " & Name & "(PC), A0");
         Cradle.Emit_Line (S => "MOVE D0, (A0)");
         Expression (Current_Frame => Current_Frame);
         Cradle.Emit_Line (S => "MOVE D0, -(SP)");
         Post_Label (L => L_1);
         Cradle.Emit_Line (S => "LEA " & Name & "(PC), A0");
         Cradle.Emit_Line (S => "MOVE (A0), D0");
         Cradle.Emit_Line (S => "ADDQ #1, D0");
         Cradle.Emit_Line (S => "MOVE D0, (A0)");
         Cradle.Emit_Line (S => "CMP (SP), D0");
         Cradle.Emit_Line (S => "BGT " & L_2);
         Block (Current_Frame => Current_Frame,
                Break_To_Label => Break_To_This);
         Reader.Match (X => 'e');
         Cradle.Emit_Line (S => "BRA " & L_1);
         Post_Label (L => L_2);
         Cradle.Emit_Line (S => "ADDQ #2, SP");
         Post_Label (L => Break_To_This);
      end;
   end Do_For;

   procedure Do_Repeat (Current_Frame : in out Frame) is
      Loop_Label : constant String := New_Label;
      Break_To_This : constant String := New_Label;
   begin
      Cradle.Enter_Fn (Fn_Name => "Do_Repeat");
      Reader.Match (X => 'r');
      Post_Label (L => Loop_Label);
      Block (Current_Frame => Current_Frame,
             Break_To_Label => Break_To_This);
      Reader.Match (X => 'u');
      Boolean_Expression (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "bne " & Loop_Label);
      Post_Label (L => Break_To_This);
      Cradle.Exit_Fn (Fn_Name => "Do_Repeat");
   end Do_Repeat;

   procedure Do_Loop (Current_Frame : in out Frame) is
      Loop_Label : constant String := New_Label;
      Break_Label : constant String := New_Label;
   begin
      Cradle.Enter_Fn (Fn_Name => "Do_Loop");
      Reader.Match (X => 'p');
      Post_Label (L => Loop_Label);
      Block (Current_Frame => Current_Frame,
             Break_To_Label => Break_Label);
      Reader.Match (X => 'e');
      Cradle.Emit_Line (S => "b " & Loop_Label);
      Post_Label (L => Break_Label);
      Cradle.Exit_Fn (Fn_Name => "Do_Loop");
   end Do_Loop;

   procedure Do_While (Current_Frame : in out Frame) is
      Loop_Label : constant String := New_Label;
      Skip_Label : constant String := New_Label;
   begin
      Cradle.Enter_Fn (Fn_Name => "Do_While");
      Reader.Match (X => 'w');
      Post_Label (L => Loop_Label);
      Boolean_Expression (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "bne " & Skip_Label);
      Block (Current_Frame => Current_Frame,
             Break_To_Label => Skip_Label);
      Reader.Match (X => 'e');
      Cradle.Emit_Line (S => "b " & Loop_Label);
      Post_Label (L => Skip_Label);
      Cradle.Exit_Fn (Fn_Name => "Do_While");
   end Do_While;

   procedure Do_If (Current_Frame : in out Frame;
      Break_To_Label : String) is
      Skip_Then_Label : constant String := New_Label;
      Skip_Else_Label : String := Skip_Then_Label;
   begin
      Cradle.Enter_Fn (Fn_Name => "Do_If");
      Reader.Match (X => 'i');
      Boolean_Expression (Current_Frame => Current_Frame);
      Cradle.Emit_Line (S => "bne " & Skip_Then_Label);
      Block (Current_Frame => Current_Frame,
             Break_To_Label => Break_To_Label);
      if Reader.Look = 'l' then
         Reader.Match (X => 'l');
         Skip_Else_Label := New_Label;
         Cradle.Emit_Line (S => "b "
                                & Skip_Else_Label);
         Post_Label (L => Skip_Then_Label);
         Block (Current_Frame => Current_Frame,
                Break_To_Label => Break_To_Label);
      end if;
      Reader.Match (X => 'e');
      Post_Label (L => Skip_Else_Label);
      Cradle.Exit_Fn (Fn_Name => "Do_If");
   end Do_If;

   procedure Do_Break (Label : String) is
   begin
      Reader.Match (X => 'b');
      if Label /= "" then
         Cradle.Emit_Line (S => "b " & Label);
      else
         Cradle.Halt (S => "No loop to break from");
      end if;
   end Do_Break;

   procedure Block (Current_Frame : in out Frame;
      Break_To_Label : String) is
   begin
      Block_Loop :
      loop
         if Reader.Look = 'i' then
            Do_If (Current_Frame => Current_Frame,
                   Break_To_Label => Break_To_Label);
         elsif Reader.Look = 'w' then
            Do_While (Current_Frame => Current_Frame);
         elsif Reader.Look = 'p' then
            Do_Loop (Current_Frame => Current_Frame);
         elsif Reader.Look = 'r' then
            Do_Repeat (Current_Frame => Current_Frame);
         elsif Reader.Look = 'd' then
            Do_Do (Current_Frame => Current_Frame);
         elsif Reader.Look = 'b' then
            Do_Break (Label => Break_To_Label);
         else
            Assignment (Current_Frame => Current_Frame);
         end if;
         exit Block_Loop when Reader.Look = 'e'
            or else Reader.Look = 'l'
            or else Reader.Look = 'u';
         --  Reader.Consume_New_Line;
         --  Reader.Get_Char;
      end loop Block_Loop;
   end Block;

   procedure Program is
   begin
      Init;
      Header;
      declare
         Main_Frame : Frame;
         Break_To_Main : constant String := "";

      begin
         Boolean_Expression (Current_Frame => Main_Frame);
         --  Assignment (Current_Frame => Main_Frame);
         --  Block (Current_Frame => Main_Frame,
         --       Break_To_Label => Break_To_Main);
      end;
      Trailer;
   end Program;

end Compiler;
