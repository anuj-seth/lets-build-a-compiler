package Compiler is
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
   procedure Condition;
   procedure Do_If;
   procedure Block;
   function New_Label return String;
   procedure Post_Label (L : String);
end Compiler;
