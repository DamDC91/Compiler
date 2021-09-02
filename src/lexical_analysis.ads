with Token;
package Lexical_Analysis is

   procedure Load(FileName : String);

   procedure Advance_Token;

   function Check_Token(Token_Type : Token.Token_Type_Enum_Type) return Boolean;

   procedure Accept_Token(Token_Type : Token.Token_Type_Enum_Type);

   procedure Error(msg : String; Line : Positive);

   function EOF return Boolean;

   function Get_Current_Token return Token.Token_Record_Type;

   function Get_Next_Token return Token.Token_Record_Type;


end Lexical_Analysis;
