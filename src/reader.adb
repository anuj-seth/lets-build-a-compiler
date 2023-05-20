with Ada.Text_IO;
with Ada.Characters.Handling;

with Cradle;

package body Reader is
   package TIO renames Ada.Text_IO;
   package CH renames Ada.Characters.Handling;

   function Is_End_Of_Line return Boolean
      renames TIO.End_Of_Line;

   procedure Skip_Line is
   begin
      TIO.Skip_Line (File => TIO.Standard_Input);
   end Skip_Line;

   procedure Consume_New_Line is
   begin
      if Is_End_Of_Line then
         Skip_Line;
      end if;
   end Consume_New_Line;

   function Look return Character is
      (Look_Ahead);

   procedure Match (X : Character) is
   begin
      Cradle.Enter_Fn (Fn_Name => "Match");
      if Look_Ahead = X then
         Get_Char;
         Skip_Whitespace;
      else
         Cradle.Expected (S => "'" & X & "'");
      end if;
      Cradle.Exit_Fn (Fn_Name => "Match");
   end Match;

   procedure Get_Char is
   begin
      if Is_End_Of_Line then
         Look_Ahead := Cradle.End_Of_Line_Character;
      else
         TIO.Get (Item => Look_Ahead);
      end if;
   end Get_Char;

   function Get_Char_While_True (Predicate : Lookahead_Predicate_Type;
                                 Till_Now : String) return String is
   begin
      if Predicate (X => Look_Ahead) then
         declare
            Name : String (1 .. Till_Now'Length + 1);
         begin
            Name (1 .. Name'Length - 1) := Till_Now;
            Name (Name'Length) := CH.To_Upper (Look_Ahead);
            Get_Char;
            return Get_Char_While_True (Predicate => Predicate,
                                        Till_Now => Name);
         end;
      else
         return Till_Now;
      end if;
   end Get_Char_While_True;

   procedure Skip_Whitespace is
   begin
      while Cradle.Is_Space (X => Look_Ahead) loop
         Get_Char;
      end loop;
   end Skip_Whitespace;

   function Get_Name return String is
   begin
      if not Cradle.Is_Alpha (X => Look_Ahead) then
         Cradle.Expected (S => "Name");
      end if;

      declare
         Name : constant String :=
            Get_Char_While_True (Predicate => Cradle.Is_Alphanumeric'Access,
                                 Till_Now => "");
      begin
         Skip_Whitespace;
         return Name;
      end;
   end Get_Name;

   function Get_Num return String is
   begin
      if not Cradle.Is_Digit (X => Look_Ahead) then
         Cradle.Expected (S => "Integer");
      end if;

      declare
         Num : constant String :=
            Get_Char_While_True (Predicate => Cradle.Is_Digit'Access,
                                 Till_Now => "");
      begin
         Skip_Whitespace;
         return Num;
      end;
   end Get_Num;

end Reader;
