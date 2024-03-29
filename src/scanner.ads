with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash;

package Scanner is
   type Symbol_Type is (If_Sym, Else_Sym, EndIf_Sym,
      While_Sym, Loop_Sym, Repeat_Sym, Do_Sym, Break_Sym,
      End_Sym, Ident, Number, Operator, End_Of_Line, Terminator, Unknown);

   subtype Keyword_Type is Symbol_Type range If_Sym .. End_Sym;

   type Token_Type (Kind : Symbol_Type) is
      record
         case Kind is
            when Keyword_Type | End_Of_Line | Terminator =>
               null;
            when others =>
               Value : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   function Scan return Token_Type;

   function Value (T : Token_Type) return String is
      (Ada.Strings.Unbounded.To_String (T.Value));

   function Kind (T : Token_Type) return Symbol_Type is
      (T.Kind);

private
   package Keyword_Map is
      new Ada.Containers.Indefinite_Hashed_Maps (Key_Type => String,
                                                 Element_Type => Keyword_Type,
                                                 Hash => Ada.Strings.Hash,
                                                 Equivalent_Keys => "=");
   Keyword_Table : Keyword_Map.Map;
end Scanner;
