with Reader;
with Cradle;

package body Scanner is
   package SU renames Ada.Strings.Unbounded;

   function Scan return Token_Type is
   begin
      if Cradle.Is_Alpha (Reader.Look) then
         declare
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
         end;
      elsif Cradle.Is_Digit (X => Reader.Look) then
         return (Kind => Number,
                 Value => SU.To_Unbounded_String (Source => Reader.Get_Num));
      elsif Cradle.Is_Op (X => Reader.Look) then
         return (Kind => Operator,
                 Value => SU.To_Unbounded_String (Source => Reader.Get_Op));
      else
         declare
            Result : constant String := "" & Reader.Look;
         begin
            Reader.Get_Char;
            return (Kind => Ident,
                    Value => SU.To_Unbounded_String (Source => Result));
         end;
      end if;
   end Scan;

begin
   Keyword_Table.Include (Key => "IF",
                          New_Item => If_Sym);
   Keyword_Table.Include (Key => "ELSE",
                          New_Item => Else_Sym);
   Keyword_Table.Include (Key => "END IF",
                          New_Item => EndIf_Sym);
   Keyword_Table.Include (Key => "End",
                          New_Item => End_Sym);
end Scanner;
