package Parser is
   procedure Assignment;
   procedure Init;

private
   Look : Character;
   function Is_End_Of_Line return Boolean;
   procedure Get_Char;
   type Lookahead_Predicate_Type is access function (X : Character)
      return Boolean;
   function Get_Char_While_True (Predicate : Lookahead_Predicate_Type;
                                 Till_Now : String) return String;
   procedure Skip_Whitespace;
   procedure Match (X : Character);
   function Get_Name return String;
   function Get_Num return String;
   procedure Factor;
   procedure Multiply;
   procedure Divide;
   procedure Term;
   procedure Add;
   procedure Subtract;
   procedure Expression;
end Parser;
