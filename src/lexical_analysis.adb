with Text_Utils;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Error_Log; 
use Error_Log;
package body Lexical_Analysis is

   File_Content : Ada.Strings.Unbounded.Unbounded_String;

   Current_Token : Token.Token_Record_Type;

   Next_Token : Token.Token_Record_Type;

   Index : Positive := 1;

   Line : Positive := 1;

   Last_Id_Value : Positive := 1;

   Association_Table : Token.Map.Map;
  
   Association_Table_Vector : Vector_Association_Table.Vector;
   
   
   function Get_Str_From_Assoc_Table (Id : Positive) return String is (Association_Table_Vector(Id));

   function Is_Letter_Or_Underscore (c : Character) return boolean is (Ada.Characters.Handling.Is_Letter (c) or c = '_');

   function Is_Letter_Or_Digit_Or_Underscore (c : Character) return boolean is (Ada.Characters.Handling.Is_Alphanumeric (c) or c = '_');

   function Is_C_KeyWord (s : string) return boolean is
   begin
      return s = "if" or s = "int" or s = "else" or s = "for" or s = "while" or s = "do" or s = "continue" or s = "break" or s = "return";
   end Is_C_KeyWord;


   function Get_Token return Token.Token_Record_Type is
      Rem_Space : Boolean := True;
   begin      
  
      while Rem_Space loop
         Rem_Space := False;
         while Ada.Strings.Unbounded.Element(File_Content, Index) = ' ' or
         Ada.Strings.Unbounded.Element(File_Content, Index) = Ada.Characters.Latin_1.HT or
         Ada.Strings.Unbounded.Element(File_Content, Index) = Ada.Characters.Latin_1.LF
         loop
         
            if Ada.Strings.Unbounded.Element(File_Content, Index) = Ada.Characters.Latin_1.LF then
               Line := Line + 1;
            end if;
            
            Index := Index + 1;
            if Index >= Ada.Strings.Unbounded.Length (File_Content) then
               return (Has_Value => False,
                          Token_Type => Token.Tok_EOF,
                       Line => Line);
            end if;
         end loop;
      
               
         if Ada.Strings.Unbounded.Element (File_Content, Index) = '/' and ( (Index + 1 < Ada.Strings.Unbounded.Length (File_Content)) and then
                                                                          Ada.Strings.Unbounded.Element (File_Content, Index + 1) = '/') then
            Rem_Space := True;
            Index := Index + 2;
            if Index >= Ada.Strings.Unbounded.Length (File_Content) then
               return (Has_Value => False,
                          Token_Type => Token.Tok_EOF,
                          Line => Line);
            end if;
            while not (Ada.Strings.Unbounded.Element(File_Content, Index) = Ada.Characters.Latin_1.LF) loop
               Index := Index + 1;
               if Index >= Ada.Strings.Unbounded.Length (File_Content) then
                  return (Has_Value => False,
                          Token_Type => Token.Tok_EOF,
                          Line => Line);
               end if;
            end loop;
            Index := Index + 1;
            if Index >= Ada.Strings.Unbounded.Length (File_Content) then
               return (Has_Value => False,
                       Token_Type => Token.Tok_EOF,
                       Line => Line);
            end if;
            Line := Line + 1;
         end if;
      
         if Ada.Strings.Unbounded.Element (File_Content, Index) = '/' and ( (Index + 1 < Ada.Strings.Unbounded.Length (File_Content)) and then
                                                                          Ada.Strings.Unbounded.Element (File_Content, Index + 1) = '*') then
            Rem_Space := True;
            Index := Index + 2;
            while not (Ada.Strings.Unbounded.Element(File_Content, Index) = '*' and ((Index + 1 < Ada.Strings.Unbounded.Length (File_Content)) and then
                                                                                    Ada.Strings.Unbounded.Element (File_Content, Index + 1) = '/')) loop
               if Ada.Strings.Unbounded.Element(File_Content, Index) = Ada.Characters.Latin_1.LF then
                  Line := Line + 1;
               end if;
               Index := Index + 1;
               if Index >= Ada.Strings.Unbounded.Length (File_Content) then
                  return (Has_Value => False,
                          Token_Type => Token.Tok_EOF,
                          Line => Line);
               end if;
            end loop;
            Index := Index + 2;
            if Index >= Ada.Strings.Unbounded.Length (File_Content) then
               return (Has_Value => False,
                       Token_Type => Token.Tok_EOF,
                       Line => Line);
            end if;
         end if;
      
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
                        raise Program_Error; -- impossible
                     end if;
                  else
                     declare
                        Id_Value : Positive;
                     begin
                        if Association_Table.Contains(Id) then
                           Id_Value := Association_Table.Element(Id);
                        else
                           Id_Value :=Last_Id_Value;
                           Association_Table.Insert (Id, Id_Value);
                           Association_Table_Vector.Append(Id);
                           Last_Id_Value := Last_Id_Value + 1;
                        end if;
                        
                        if Is_Digit (Id(Id'First)) then
                           Error (Msg  => "Invalid variable name",
                                  Line => Line);
                           raise Input_Error;
                        end if;

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
                       Token_Type => Token.Tok_Exclamation_Mark,
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
               raise Input_Error;
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
               
               --hex value
               If char = '0' and To_Lower (Ada.Strings.Unbounded.Element(File_Content, Index + 1)) = 'x' then
                  Buffer :=  Ada.Strings.Unbounded.To_Unbounded_String ("16#");
                  Index := Index + 1;
                  
                  while Is_Digit(Ada.Strings.Unbounded.Element(File_Content, Index + 1)) or 
                  (Ada.Strings.Unbounded.Element(File_Content, Index + 1) >= 'a' and
                  Ada.Strings.Unbounded.Element(File_Content, Index + 1) <= 'f') loop
                     Ada.Strings.Unbounded.Append (Buffer, "" &  Ada.Strings.Unbounded.Element(File_Content, Index + 1));
                     Index := Index + 1;
                  end loop;
                  Ada.Strings.Unbounded.Append(buffer,"#");
                  
               -- bin value
               elsif char = '0' and To_Lower (Ada.Strings.Unbounded.Element(File_Content, Index + 1)) = 'b' then
                  Buffer :=  Ada.Strings.Unbounded.To_Unbounded_String ("2#");
                  Index := Index + 1;
                  
                  while Ada.Strings.Unbounded.Element(File_Content, Index + 1) = '0' or Ada.Strings.Unbounded.Element(File_Content, Index + 1) = '1'  loop
                     Ada.Strings.Unbounded.Append (Buffer, "" &  Ada.Strings.Unbounded.Element(File_Content, Index + 1));
                     Index := Index + 1;
                  end loop;
                  Ada.Strings.Unbounded.Append(buffer,"#");
                  
               -- base 8
               elsif char = '0' and (Ada.Strings.Unbounded.Element(File_Content, Index + 1) >= '0' and Ada.Strings.Unbounded.Element(File_Content, Index + 1) <= '7')   then
                  Buffer :=  Ada.Strings.Unbounded.To_Unbounded_String ("8#");
                  
                  while Ada.Strings.Unbounded.Element(File_Content, Index + 1) >= '0' and Ada.Strings.Unbounded.Element(File_Content, Index + 1) <= '7'  loop
                     Ada.Strings.Unbounded.Append (Buffer, "" &  Ada.Strings.Unbounded.Element(File_Content, Index + 1));
                     Index := Index + 1;
                  end loop;
                  Ada.Strings.Unbounded.Append(buffer,"#");
                  
               -- base 10 value   
               else
                  while Is_Digit(Ada.Strings.Unbounded.Element(File_Content, Index + 1))  loop
                     Ada.Strings.Unbounded.Append (Buffer, "" &  Ada.Strings.Unbounded.Element(File_Content, Index + 1));
                     Index := Index + 1;
                  end loop;
               end if;
               
            
               if Is_Letter_Or_Underscore (Ada.Strings.Unbounded.Element(File_Content, Index + 1)) then
                  Error (msg  => "invalid constant",
                         Line => Line);
                  raise Input_Error;
               end if;
               
               Index := Index + 1;
               declare
                  --handle constant overflow like gcc
                  Val_Read : Long_Long_Integer;
                  Val : Integer;
               begin
                  begin
                     Val_Read :=  Long_Long_Integer'Value (Ada.Strings.Unbounded.To_String (buffer));
                  exception
                     when others =>
                        Error(Msg  => "constant value is too big " & Ada.Strings.Unbounded.To_String(buffer),
                              Line => Line);
                        raise Input_Error;
                  end;
                  
                  
                  if Val_Read > 2**31 - 1 then -- C int max
                     Warning (Msg => "constant overflow",
                              Line => Line);
                     val := Integer ((Val_Read mod (2**31)) - 2**31);
                  else
                     val := Natural (Val_Read);
                  end if;
               
                  return (Has_Value => True, 
                          Token_Type => Token.Tok_Const,
                          Value => Val,
                          Line => Line);
               end;
            end;
         elsif char = Ada.Characters.Latin_1.EOT then
            return (Has_Value => False,
                    Token_Type => Token.Tok_EOF,
                    Line => Line);
         else
            Error (msg  => "Unexpected Token " & Ada.Strings.Unbounded.Element (File_Content, Index),
                   Line => Line);
            raise Input_Error;
         end if;
      end;
   end Get_Token;


   procedure Load(FileName : String) is
   begin
      Index := 1;
      Line := 1;
      File_Content := Ada.Strings.Unbounded.To_Unbounded_String (Text_Utils.Get_File_Content(FileName));
      -- null Token, usefull to set the Line to 1 if there is an error at the begging of the file
      -- This token is never read
      Current_Token := (Has_Value => False,
                        Token_Type => Token.Tok_EOF,
                        Line       => 1);
      Next_Token := Get_Token;

      Error_Log.Create_Token_File (FileName => FileName);
   end Load;


   procedure Advance_Token is
   begin
      Error_Log.Debug_Print_Token (Next_Token);
      Current_Token := Next_Token;
      Next_Token := Get_Token;
   end Advance_Token;

   function Check_Token(Token_Type : Token.Token_Type_Enum_Type) return Boolean is
      use type Token.Token_Type_Enum_Type;
   begin
      if Next_Token.Token_Type /= Token_Type then
         return False;
      else
         Advance_Token;
         return True;
      end if;
   end Check_Token;

   procedure Accept_Token(Token_Type : Token.Token_Type_Enum_Type) is
   begin
      if not Check_Token(Token_Type) then
         Error(Token_Type'Image & " was excpected here, found " & Get_Next_Token.Token_Type'Image, Get_Current_Token.Line);
         raise Error_Log.Input_Error;
      end if;
   end Accept_Token;

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
   
   
   
begin
   Association_Table_Vector.Append ("putchar");
   Association_Table_Vector.Append ("getchar");
   Association_Table_Vector.Append ("exit");
   Association_Table.Insert (Key      => "putchar",
                             New_Item => 1);
   Association_Table.Insert (Key      => "getchar",
                             New_Item => 2);
   Association_Table.Insert (Key      => "exit",
                             New_Item => 3);
   Last_Id_Value := 4;

end Lexical_Analysis;
