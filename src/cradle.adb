with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Characters.Handling;

package body Cradle is
   package TIO renames Ada.Text_IO;
   package CL1 renames Ada.Characters.Latin_1;
   package CH renames Ada.Characters.Handling;

   procedure Error (S : String) is
   begin
      TIO.New_Line;
      TIO.Put (CL1.BEL);
      TIO.Put_Line ("Error: " & S & ".");
   end Error;

   procedure Halt (S : String) is
   begin
      Error (S);
      raise Halt_Exception;
   end Halt;

   procedure Expected (S : String) is
   begin
      Halt (S & " Expected");
   end Expected;

   function Is_Alpha (X : Character) return Boolean
      renames CH.Is_Letter;

   function Is_Digit (X : Character) return Boolean
      renames CH.Is_Digit;

   procedure Emit (S : String) is
   begin
      TIO.Put (CL1.HT);
      TIO.Put (S);
   end Emit;

   procedure Emit_Line (S : String) is
   begin
      Emit (S);
      TIO.New_Line;
   end Emit_Line;

end Cradle;
