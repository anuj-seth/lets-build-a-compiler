package Cradle is
   Halt_Exception : exception;
   function End_Of_Line_Character return Character;
   procedure Error (S : String);
   procedure Halt (S : String);
   procedure Expected (S : String);
   procedure Emit (S : String);
   procedure Emit_Line (S : String);
   procedure Enter_Fn (Fn_Name : String);
   procedure Exit_Fn (Fn_Name : String);
   function Is_Space (X : Character) return Boolean;
   function Is_Alpha (X : Character) return Boolean;
   function Is_Digit (X : Character) return Boolean;
   function Is_Alphanumeric (X : Character) return Boolean;
   function Is_Addop (X : Character) return Boolean;
   function Is_Mulop (X : Character) return Boolean;
   function Integer_To_String (I : Integer) return String;
end Cradle;
