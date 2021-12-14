with Token;
with Ada.Containers.Indefinite_Vectors;
package Lexical_Analysis is

   procedure Load(FileName : String);

   procedure Advance_Token;

   function Check_Token(Token_Type : Token.Token_Type_Enum_Type) return Boolean;

   procedure Accept_Token(Token_Type : Token.Token_Type_Enum_Type);

   function EOF return Boolean;

   function Get_Current_Token return Token.Token_Record_Type;

   function Get_Next_Token return Token.Token_Record_Type;

   procedure Close_Debug;

   package Vector_Association_Table is new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                                                              Element_Type => String);
   function Get_Str_From_Assoc_Table (Id : Positive) return String;


end Lexical_Analysis;
