package Reader is
   function Look return Character;
   procedure Get_Char;
   type Lookahead_Predicate_Type is access function (X : Character)
      return Boolean;
   function Get_Char_While_True (Predicate : Lookahead_Predicate_Type;
                                 Till_Now : String) return String;
   procedure Skip_Whitespace;
   procedure Skip_Line;
   procedure Match (X : Character);
   function Get_Name return String;
   function Get_Num return String;
   function Get_Boolean return Boolean;
   function Is_End_Of_Line return Boolean;
   procedure Consume_New_Line;
private
   Look_Ahead : Character;
end Reader;
