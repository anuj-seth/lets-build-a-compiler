package Interpreter is
   procedure Run;
private
   subtype Variables is Character range 'A' .. 'Z';
   Table : array (Variables) of Integer
      := (others => 0);

   procedure Input;
   procedure Output;
   procedure Newline;
   function Term return Integer;
   function Factor return Integer;
   function Expression return Integer;
   procedure Assignment;
   procedure Init;
end Interpreter;
