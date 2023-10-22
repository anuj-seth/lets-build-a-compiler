with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Equal_Case_Insensitive;

package Tiny is
   procedure Program;

private
   type Var_Initial_Value is
      record

   package String_To_String_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
         (Element_Type    => String,
          Key_Type        => String,
          Hash            => Ada.Strings.Hash_Case_Insensitive,
          Equivalent_Keys => Ada.Strings.Equal_Case_Insensitive);

   Global_ST : String_To_String_Maps.Map;
end Tiny;
