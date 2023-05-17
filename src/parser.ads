package Parser is
   procedure Init;
   procedure Assignment;
   procedure Program;

private
   procedure Factor;
   procedure Multiply;
   procedure Divide;
   procedure Term;
   procedure Add;
   procedure Subtract;
   procedure Expression;
   procedure Block;
end Parser;
