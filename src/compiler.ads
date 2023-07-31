with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;

package Compiler is
   type Frame is private;
   procedure Init;
   procedure Header;
   procedure Trailer;
   procedure Expression (Current_Frame : in out Frame);
   --  procedure Assignment;
   --  procedure Program;

private
   package String_To_Integer_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
         (Element_Type    => Integer,
          Key_Type        => String,
          Hash            => Ada.Strings.Hash_Case_Insensitive,
          Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   type Frame is record
      Top : Integer := 0;
      Variable_Offsets : String_To_Integer_Maps.Map;
   end record;

   procedure Factor (Current_Frame : in out Frame);
   procedure Multiply (Current_Frame : in out Frame);
   procedure Divide (Current_Frame : in out Frame);
   procedure Term (Current_Frame : in out Frame);
   procedure Add (Current_Frame : in out Frame);
   procedure Subtract (Current_Frame : in out Frame);
   --  procedure Condition;
   --  procedure Do_If (Break_To_Label : String);
   --  procedure Do_While;
   --  procedure Do_Loop;
   --  procedure Do_Repeat;
   --  procedure Do_For;
   --  procedure Do_Break (Label : String);
   --  procedure Block (Break_To_Label : String);
   --  function New_Label return String;
   --  procedure Post_Label (L : String);

end Compiler;
