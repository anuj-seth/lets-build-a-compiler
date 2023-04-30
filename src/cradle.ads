package Cradle is
   Halt_Exception : exception;
   procedure Error (S : String);
   procedure Halt (S : String);
   procedure Expected (S : String);
   function Is_Alpha (X : Character) return Boolean;
   function Is_Digit (X : Character) return Boolean;
   procedure Emit (S : String);
   procedure Emit_Line (S : String);
end Cradle;
