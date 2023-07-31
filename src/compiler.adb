with Cradle;
with Reader;

package body Compiler is

   procedure Factor (Current_Frame : in out Frame) is
   begin
      if Reader.Look = '(' then
         Reader.Match (X => '(');
         Expression (Current_Frame => Current_Frame);
         Reader.Match (X => ')');
      elsif Cradle.Is_Alpha (X => Reader.Look) then
         declare
            Variable_Name : constant String := Reader.Get_Name;
            Offset : constant Integer := Current_Frame.Variable_Offsets (Variable_Name);
         begin
            Cradle.Emit_Line (S => "ldr r0, [fp, -"
                                   & Integer'Image (Offset)
                                   & "]");
         end;
      else
         Cradle.Emit_Line (S => "mov r0, #"
                                & Reader.Get_Num);
      end if;
   end Factor;

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
      Factor (Current_Frame => Current_Frame);

      Mulop_Loop :
      while Cradle.Is_Mulop (Reader.Look) loop
         Cradle.Emit_Line (S => "push {r0}");
         if Reader.Look = '*' then
            Multiply (Current_Frame => Current_Frame);
         elsif Reader.Look = '/' then
            Divide (Current_Frame => Current_Frame);
         else
            Cradle.Expected (S => "Mulop");
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
      if Cradle.Is_Addop (Reader.Look) then
         Cradle.Emit_Line (S => "mov r0, #0"
                                & " @ leading zero for unary operators");
      else
         Term (Current_Frame => Current_Frame);
      end if;

      Addop_Loop :
      while Cradle.Is_Addop (Reader.Look) loop
         Cradle.Emit_Line (S => "push {r0}");
         if Reader.Look = '+' then
            Add (Current_Frame => Current_Frame);
         elsif Reader.Look = '-' then
            Subtract (Current_Frame => Current_Frame);
         else
            Cradle.Expected (S => "Addop");
         end if;
      end loop Addop_Loop;
   end Expression;

   procedure Header is
   begin
      Cradle.Emit_Line (S => ".cpu cortex-a15"
                             & "  @ this directive enables "
                             & " support for sdiv/udiv");
      Cradle.Emit_Line (S => ".text");
      Cradle.Emit_Line (S => ".global _start");
      Cradle.Emit_Line (S => "_start:");
   end Header;

   procedure Trailer is
   begin
      Cradle.Emit_Line (S => "_zero_exit_code:");
      Cradle.Emit_Line (S => "@mov r0, #0");
      Cradle.Emit_Line (S => "_exit:");
      Cradle.Emit_Line (S => "mov r7, #1");
      Cradle.Emit_Line (S => "svc 0");
   end Trailer;

   procedure Init is
   begin
      Reader.Get_Char;
   end Init;

end Compiler;
