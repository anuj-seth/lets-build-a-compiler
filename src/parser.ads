package Parser is
   procedure Assignment;
   procedure Init;

private
   Look : Character;
   function Is_End_Of_Line return Boolean;
   procedure Get_Char;
   procedure Match (X : Character);
   function Get_Name return Character;
   function Get_Num return Character;
   procedure Factor;
   procedure Multiply;
   procedure Divide;
   procedure Term;
   procedure Add;
   procedure Subtract;
   function Is_Addop (C : Character) return Boolean;
   procedure Expression;
end Parser;
