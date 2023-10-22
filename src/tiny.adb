with Cradle;
with Reader;

package body Tiny is

   procedure Header is
   begin
      Cradle.Emit_Line (S => ".cpu cortex-a15"
                             & "  @ this directive enables "
                             & " support for sdiv/udiv");
   end Header;

   procedure Trailer is
   begin
      Cradle.Emit_Line (S => "bx lr");
      --  Cradle.Emit_Line (S => "@_zero_exit_code:");
      --  Cradle.Emit_Line (S => "@mov r0, #0");
      --  Cradle.Emit_Line (S => "@_exit:");
      --  Cradle.Emit_Line (S => "@mov r7, #1");
      --  Cradle.Emit_Line (S => "@svc 0");
   end Trailer;

   procedure Declaration is
   begin
      Cradle.Enter_Fn (Fn_Name => "Declaration");
      Reader.Match (X => 'v');
      declare
         Var_Name : constant String := Reader.Get_Name;
      Cradle.Emit_Line (S => "Character " & Reader.Look);
      Reader.Get_Char;
      Cradle.Enter_Fn (Fn_Name => "Declaration");
   end Declaration;

   procedure Top_Declarations is 
   begin
      Cradle.Enter_Fn (Fn_Name => "Top_Declarations");
      Cradle.Emit_Line (S => ".data");
      while Reader.Look /= 'b' loop
         if Reader.Look = 'v' then
            Declaration;
         else
            Cradle.Halt (S => "Unexpected keyword: " & Reader.Look);
         end if;
      end loop;
      Cradle.Exit_Fn (Fn_Name => "Top_Declarations");
   end Top_Declarations;

   procedure Main is
   begin
      Reader.Match (X => 'b');
      Cradle.Emit_Line (S => ".text");
      Cradle.Emit_Line (S => ".global main");
      Cradle.Emit_Line (S => "main:");
      Reader.Match (X => 'e');
      Trailer;
   end Main;

   procedure Init is
   begin
      Reader.Get_Char;
      Reader.Skip_Whitespace;
   end Init;

   procedure Program is
   begin
      Init;
      Header;
      Reader.Match (X => 'p');
      Top_Declarations;
      Main;
      Reader.Match (X => '.');
   end Program;
end Tiny;

