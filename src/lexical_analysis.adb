with Text_Utils;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Text_IO;
package body Lexical_Analysis is

   File_Content : Ada.Strings.Unbounded.Unbounded_String;

   Current_Token : Token.Token_Record_Type;

   Next_Token : Token.Token_Record_Type;

   Index : Positive := 1;

   Line : Positive := 1;

   Last_Id_Value : Positive := 1;

   Association_Table : Token.Map.Map;


   function Is_Letter_Or_Underscore (c : Character) return boolean is (Ada.Characters.Handling.Is_Letter (c) or c = '_');

   function Is_Letter_Or_Digit_Or_Underscore (c : Character) return boolean is (Ada.Characters.Handling.Is_Alphanumeric (c) or c = '_');

   function Is_C_KeyWord (s : string) return boolean is
   begin
      return s = "if" or s = "int" or s = "else" or s = "for" or s = "while" or s = "do" or s = "continue" or s = "break" or s = "return";
   end Is_C_KeyWord;


   function Get_Token return Token.Token_Record_Type is
   begin
      while Ada.Strings.Unbounded.Element(File_Content, Index) = ' ' or
        Ada.Strings.Unbounded.Element(File_Content, Index) = Ada.Characters.Latin_1.HT or
        Ada.Strings.Unbounded.Element(File_Content, Index) = Ada.Characters.Latin_1.LF
      loop
         if Ada.Strings.Unbounded.Element(File_Content, Index) = Ada.Characters.Latin_1.LF then
            Line := Line + 1;
         end if;
         Index:=Index+1;
      end loop;
      declare
         char : constant Character := Ada.Strings.Unbounded.Element (File_Content, Index);
         use Ada.Characters.Handling;
      begin

         if Is_Letter_Or_Underscore (char) then
            declare
               buffer : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("" & char);
            begin
               while Is_Letter_Or_Digit_Or_Underscore (Ada.Strings.Unbounded.Element (File_Content, Index + 1)) loop
                  Ada.Strings.Unbounded.Append(Buffer, "" & Ada.Strings.Unbounded.Element(File_Content, Index + 1));
                  Index := Index + 1;
               end loop;
               Index := Index + 1;

               declare
                  Id : constant String := Ada.Strings.Unbounded.To_String (Buffer);
               begin

                  if Is_C_KeyWord (Id) then

                     if Id = "if" then
                        return (Has_Value => False,
                                Token_Type => Token.Tok_If,
                                Line => Line);
                     elsif Id = "int" then
                        return (Has_Value => False,
                                Token_Type => Token.Tok_Int,

                                Line => Line);
                     elsif Id = "else" then
                        return (Has_Value => False,
                                Token_Type => Token.Tok_Else,
                                Line => Line);
                     elsif Id = "for" then
                        return (Has_Value => False,
                                Token_Type => Token.Tok_For,
                                Line => Line);
                     elsif Id = "while" then
                        return (Has_Value => False,
                                Token_Type => Token.Tok_While,
                                Line => Line);
                     elsif Id = "do" then
                        return (Has_Value => False,
                                Token_Type => Token.Tok_Do,
                                Line => Line);
                     elsif Id = "continue" then
                        return (Has_Value => False,
                                Token_Type => Token.Tok_Continue,
                                Line => Line);
                     elsif Id = "break" then
                        return (Has_Value => False,
                                Token_Type => Token.Tok_Break,
                                Line => Line);
                     elsif Id = "return" then
                        return (Has_Value => False,
                                Token_Type => Token.Tok_Return,
                                Line => Line);
                     else
                        raise Constraint_Error; -- impossible
                     end if;
                  else
                     declare
                        Id_Value : Positive := Last_Id_Value;
                     begin
                        Last_Id_Value := Last_Id_Value +1;
                        Association_Table.Insert ( Id_Value, Id);

                        return (Has_Value => True,
                                Token_Type => Token.Tok_Id,
                                Value => Id_Value,
                                Line => Line);
                     end;

                  end if;
               end;
            end;

         elsif char = '+' then
            Index := Index + 1;
            return (Has_Value => False,
                    Token_Type => Token.Tok_Plus,
                    Line => Line);
         elsif char = '-' then
            Index := Index + 1;
            return (Has_Value => False,
                    Token_Type => Token.Tok_Minus,
                    Line => Line);
         elsif char = '*' then
            Index := Index + 1;
            return (Has_Value => False,
                    Token_Type => Token.Tok_Asterisk,
                    Line => Line);
         elsif char = '/' then
            Index := Index + 1;
            return (Has_Value => False,
                    Token_Type => Token.Tok_Slash,
                    Line => Line);
         elsif char = '%' then
            Index := Index + 1;
            return (Has_Value => False,
                    Token_Type => Token.Tok_Percent,
                    Line => Line);
         elsif char = '!' then
            if Ada.Strings.Unbounded.Element (File_Content, Index +1) = '=' then
               Index := Index + 2;
               return (Has_Value => False,
                       Token_Type => Token.Tok_Difference_Comparaison,
                       Line => Line);
            else
               Index := Index + 1;
               return (Has_Value => False,
                       Token_Type => Token.Tok_Plus,
                       Line => Line);
            end if;
         elsif char = '&' then
            if Ada.Strings.Unbounded.Element (File_Content, Index +1) = '&' then
               Index := Index + 2;
               return (Has_Value => False,
                       Token_Type => Token.Tok_And_Boolean,
                       Line => Line);
            else
               Index := Index + 1;
               return (Has_Value => False,
                       Token_Type => Token.Tok_Ampersand,
                       Line => Line);
            end if;
         elsif char = '=' then
            if Ada.Strings.Unbounded.Element (File_Content, Index +1) = '=' then
               Index := Index + 2;
               return (Has_Value => False,
                       Token_Type => Token.Tok_Equal_Comparaison,
                       Line => Line);
            else
               Index := Index + 1;
               return (Has_Value => False,
                       Token_Type => Token.Tok_Assignment,
                       Line => Line);
            end if;
         elsif char = '<' then
            if Ada.Strings.Unbounded.Element (File_Content, Index +1) = '=' then
               Index := Index + 2;
               return (Has_Value => False,
                       Token_Type => Token.Tok_Less_Or_Equal_Than,
                       Line => Line);
            else
               Index := Index + 1;
               return (Has_Value => False,
                       Token_Type => Token.Tok_Less_Than,
                       Line => Line);
            end if;

         elsif char = '>' then
            if Ada.Strings.Unbounded.Element (File_Content, Index +1) = '=' then
               Index := Index + 2;
               return (Has_Value => False,
                       Token_Type => Token.Tok_Greater_Or_Equal_Than,
                       Line => Line);
            else
               Index := Index + 1;
               return (Has_Value => False,
                       Token_Type => Token.Tok_Greater_Than,
                       Line => Line);
            end if;
         elsif char = '|' then
            if Ada.Strings.Unbounded.Element (File_Content, Index +1) = '|' then
               Index := Index + 2;
               return (Has_Value => False,
                       Token_Type => Token.Tok_Or_Boolean,
                       Line => Line);
            else
               Error (msg  => "a second '|' was expected",
                      Line => Line);
               raise Constraint_Error;
            end if;

         elsif char = ',' then
            Index := Index + 1;
            return (Has_Value => False,
                    Token_Type => Token.Tok_Comma,
                    Line => Line);

         elsif char = ';' then
            Index := Index + 1;
            return (Has_Value => False,
                    Token_Type => Token.Tok_Semi_Colon,
                    Line => Line);

         elsif char = '(' then
            Index := Index + 1;
            return (Has_Value => False,
                    Token_Type => Token.Tok_Left_Parenthesis,
                    Line => Line);
         elsif char = ')' then
            Index := Index + 1;
            return (Has_Value => False,
                    Token_Type => Token.Tok_Right_Parenthesis,
                    Line => Line);

         elsif char = '[' then
            Index := Index + 1;
            return (Has_Value => False,
                    Token_Type => Token.Tok_Left_Bracket,
                    Line => Line);
         elsif char = ']' then
            Index := Index + 1;
            return (Has_Value => False,
                    Token_Type => Token.Tok_Right_Bracket,
                    Line => Line);

         elsif char = '{' then
            Index := Index + 1;
            return (Has_Value => False,
                    Token_Type => Token.Tok_Left_Curly_Bracket,
                    Line => Line);
         elsif char = '}' then
            Index := Index + 1;
            return (Has_Value => False,
                    Token_Type => Token.Tok_Right_Curly_Bracket,
                    Line => Line);

         elsif Is_Digit (char) then
            declare
               buffer : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String ("" & char);
            begin
               while Is_Digit(Ada.Strings.Unbounded.Element(File_Content, Index + 1)) loop
                  Ada.Strings.Unbounded.Append (Buffer, "" &  Ada.Strings.Unbounded.Element(File_Content, Index + 1));
                  Index := Index + 1;
               end loop;
               Index := Index + 1;
               return (Has_Value => True, 
                       Token_Type => Token.Tok_Const,
                       Value => Natural'Value (Ada.Strings.Unbounded.To_String (buffer)),
                       Line => Line);
            end;
         elsif char = Ada.Characters.Latin_1.EOT then
            return (Has_Value => False,
                    Token_Type => Token.Tok_EOF,
                    Line => Line);
         else
            Error (msg  => "Unexpected Token",
                   Line => Line);
            raise Constraint_Error;
         end if;
      end;
   end Get_Token;


   procedure Load(FileName : String) is
   begin
      File_Content := Ada.Strings.Unbounded.To_Unbounded_String (Text_Utils.Get_File_Content(FileName));
      Current_Token := Get_Token;
      Next_Token := Get_Token;
   end Load;


   procedure Advance_Token is
   begin
      Token.Debug_Put_Line(Current_Token);
      Current_Token := Next_Token;
      Next_Token := Get_Token;
   end Advance_Token;

   function Check_Token(Token_Type : Token.Token_Type_Enum_Type) return Boolean is
      use type Token.Token_Type_Enum_Type;
   begin
      if Next_Token.Token_Type /= Token_Type then
         return false;
      else
         Advance_Token;
         return true;
      end if;
   end Check_Token;

   procedure Accept_Token(Token_Type : Token.Token_Type_Enum_Type) is
   begin
      if not Check_Token(Token_Type) then
         Error(Token_Type'Image & " was excpected here", Line);
      end if;
   end Accept_Token;

   procedure Error(msg : String; Line : Positive) is
   begin
      Ada.Text_IO.Put_Line ("[Error] : " & msg & " l:" & Line'Image);
   end Error;

   function EOF return Boolean is
      use type Token.Token_Type_Enum_Type;
   begin
      return Next_Token.Token_Type = Token.Tok_EOF;
   end EOF;
   
   function Get_Current_Token return Token.Token_Record_Type is
   begin
      return Current_Token;
   end Get_Current_Token;
   
   function Get_Next_Token return Token.Token_Record_Type is
   begin
      return Next_Token;
   end Get_Next_Token;

end Lexical_Analysis;
