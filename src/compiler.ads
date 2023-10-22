with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Finalization;

package Compiler is
   procedure Program;

private
   package String_To_Integer_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
         (Element_Type    => Integer,
          Key_Type        => String,
          Hash            => Ada.Strings.Hash_Case_Insensitive,
          Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   type Frame is new
      Ada.Finalization.Controlled with
      record
         Variables_In_Stack : Natural := 0;
         Variable_Offsets : String_To_Integer_Maps.Map;
      end record;

   overriding
   procedure Initialize (F : in out Frame);
   overriding
   procedure Finalize (F : in out Frame);

   procedure Init;
   procedure Header;
   procedure Trailer;
   procedure Assignment (Current_Frame : in out Frame; Var_Name : String);
   procedure Expression (Current_Frame : in out Frame);
   procedure Boolean_Term (Current_Frame : in out Frame);
   procedure Boolean_Or (Current_Frame : in out Frame);
   procedure Boolean_Xor (Current_Frame : in out Frame);
   procedure Boolean_Expression (Current_Frame : in out Frame);
   procedure Identifier (Current_Frame : in out Frame);
   procedure Factor (Current_Frame : in out Frame);
   procedure Multiply (Current_Frame : in out Frame);
   procedure Divide (Current_Frame : in out Frame);
   procedure Term (Current_Frame : in out Frame);
   procedure Add (Current_Frame : in out Frame);
   procedure Subtract (Current_Frame : in out Frame);
   --  procedure Condition;
   procedure Do_Do (Current_Frame : in out Frame);
   procedure Do_If (Current_Frame : in out Frame; Break_To_Label : String);
   procedure Do_While (Current_Frame : in out Frame);
   procedure Do_Loop (Current_Frame : in out Frame);
   procedure Do_Repeat (Current_Frame : in out Frame);
   --  procedure Do_For;
   --  procedure Do_Break (Label : String);
   procedure Block (Current_Frame : in out Frame;
      Break_To_Label : String);
   --  function New_Label return String;
   --  procedure Post_Label (L : String);

end Compiler;
