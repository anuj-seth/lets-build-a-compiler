with Reader;
with Cradle;

package body Scanner is
   package SU renames Ada.Strings.Unbounded;

   function Handle_String return Token_Type is
      S : constant String := Reader.Get_Name;
   begin
      if Keyword_Table.Contains (Key => S) then
         declare
            Tp : constant Keyword_Type := Keyword_Table (S);
            Token : Token_Type (Kind => Tp);
         begin
            return Token;
         end;
      else
         return (Kind => Ident,
                 Value => SU.To_Unbounded_String (Source => S));
      end if;
   end Handle_String;

   function Scan return Token_Type is
   begin
      if Reader.Is_End_Of_Line then
         return (Kind => End_Of_Line);
      elsif Reader.Look = '.' then
         return (Kind => Terminator);
      elsif Cradle.Is_Alpha (Reader.Look) then
         return Handle_String;
      elsif Cradle.Is_Digit (X => Reader.Look) then
         return (Kind => Number,
                 Value => SU.To_Unbounded_String (Source => Reader.Get_Num));
      elsif Cradle.Is_Op (X => Reader.Look) then
         return (Kind => Operator,
                 Value => SU.To_Unbounded_String (Source => Reader.Get_Op));
      else
         return (Kind => Unknown,
                 Value => SU.To_Unbounded_String (Source => Reader.Look & ""));
      end if;
   end Scan;

begin
   Keyword_Table.Include (Key => "IF",
                          New_Item => If_Sym);
   Keyword_Table.Include (Key => "ELSE",
                          New_Item => Else_Sym);
   Keyword_Table.Include (Key => "END IF",
                          New_Item => EndIf_Sym);
   Keyword_Table.Include (Key => "WHILE",
                          New_Item => While_Sym);
   Keyword_Table.Include (Key => "LOOP",
                          New_Item => Loop_Sym);
   Keyword_Table.Include (Key => "REPEAT",
                          New_Item => Repeat_Sym);
   Keyword_Table.Include (Key => "DO",
                          New_Item => Do_Sym);
   Keyword_Table.Include (Key => "BREAK",
                          New_Item => Break_Sym);
   Keyword_Table.Include (Key => "End",
                          New_Item => End_Sym);
end Scanner;
