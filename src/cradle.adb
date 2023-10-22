with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Environment_Variables;

package body Cradle is
   package TIO renames Ada.Text_IO;
   package CL1 renames Ada.Characters.Latin_1;
   package CH renames Ada.Characters.Handling;
   package ENV renames Ada.Environment_Variables;

   DEBUG : Boolean;
   FUNCTION_DEPTH : Natural := 1;

   function End_Of_Line_Character return Character is
      (CL1.LF);

   function Is_End_Of_Line_Character (X : Character) return Boolean is
      (X = CL1.LF);

   function Is_Space (X : Character) return Boolean is
      (CH.Is_Space (Item => X) or else X = CL1.HT);

   function Is_Alpha (X : Character) return Boolean
      renames CH.Is_Letter;

   function Is_Digit (X : Character) return Boolean
      renames CH.Is_Digit;

   function Is_Alphanumeric (X : Character) return Boolean is
      (Is_Alpha (X => X) or else Is_Digit (X => X));

   function Is_Mulop (X : Character) return Boolean is
      (X in '*' | '/');

   function Is_Addop (X : Character) return Boolean is
      (X in '+' | '-');

   function Is_Orop (X : Character) return Boolean is
      (X in '|' | '~');

   function Is_Boolean (X : Character) return Boolean is
      (X in 'T' | 't' | 'F' | 'f');

   function Is_Op (X : Character) return Boolean is
      (X in '+' | '-' | '*' | '/' | '<' | '>' | ':' | '=');

   function Is_Relop (X : Character) return Boolean is
      (X in '=' | '#' | '<' | '>');

   procedure Enter_Fn (Fn_Name : String) is
   begin
      if not DEBUG then
         return;
      end if;

      FUNCTION_DEPTH := FUNCTION_DEPTH + 1;
      for I in 1 .. FUNCTION_DEPTH loop
         TIO.Put (Item => CL1.Space);
      end loop;
      TIO.Put_Line (Item => "->" & Fn_Name);
   end Enter_Fn;

   procedure Exit_Fn (Fn_Name : String) is
   begin
      if not DEBUG then
         return;
      end if;

      for I in 1 .. FUNCTION_DEPTH loop
         TIO.Put (Item => CL1.Space);
      end loop;
      TIO.Put_Line (Item => "<-" & Fn_Name);
      FUNCTION_DEPTH := FUNCTION_DEPTH - 1;
   end Exit_Fn;

   procedure Error (S : String) is
   begin
      TIO.New_Line;
      TIO.Put (Item => CL1.BEL);
      TIO.Put_Line (Item => "Error: " & S & ".");
   end Error;

   procedure Halt (S : String) is
   begin
      Error (S => S);
      raise Halt_Exception;
   end Halt;

   procedure Expected (S : String) is
   begin
      Halt (S => S & " Expected");
   end Expected;

   procedure Emit (S : String) is
   begin
      TIO.Put (Item => CL1.HT);
      TIO.Put (Item => S);
   end Emit;

   procedure Emit_Line (S : String) is
   begin
      Emit (S => S);
      TIO.New_Line;
   end Emit_Line;

   function Integer_To_String (I : Integer) return String is
      use Ada.Strings.Fixed;
      S : constant String := Trim (Integer'Image (I),
                                   Ada.Strings.Left);
   begin
      return S;
   end Integer_To_String;

begin
   declare
      Env_Var_Value : constant String
         := ENV.Value ("DEBUG", "NOT FOUND");
   begin
      if Env_Var_Value = "NOT FOUND" then
         DEBUG := False;
      elsif Env_Var_Value = "on" then
         DEBUG := True;
      else
         DEBUG := False;
      end if;
   end;
end Cradle;
