package Parser is
   procedure Expression;
   procedure Init;

private
   Look : Character;
   procedure Get_Char;
   procedure Match (X : Character);
   function Get_Name return Character;
   function Get_Num return Character;
   procedure Term;
   procedure Add;
   procedure Subtract;
end Parser;
