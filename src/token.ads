with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Token is

   type Token_Type_Enum_Type is (Tok_Const,
                                 Tok_Id,
                                 Tok_Plus,
                                 Tok_Minus,
                                 Tok_Asterisk,
                                 Tok_Slash,
                                 Tok_Percent,
                                 Tok_Exclamation_Mark,
                                 Tok_Ampersand, -- &
                                 Tok_Assignment, -- =
                                 Tok_Equal_Comparaison, -- ==
                                 Tok_Difference_Comparaison,  -- !=
                                 Tok_Greater_than,   -- >
                                 Tok_Greater_Or_Equal_Than, -- >=
                                 Tok_Less_Than, -- <
                                 Tok_Less_Or_Equal_Than, -- <=
                                 Tok_And_Boolean, -- &&
                                 Tok_Or_Boolean, -- ||
                                 Tok_Comma,
                                 Tok_Semi_Colon,
                                 Tok_Left_Parenthesis, -- (
                                 Tok_Right_Parenthesis, -- )
                                 Tok_Left_Curly_Bracket, -- {
                                 Tok_Right_Curly_Bracket, -- }
                                 Tok_Left_Bracket,   -- [
                                 Tok_Right_Bracket,  -- ]
                                 Tok_Int,
                                 Tok_If,
                                 Tok_Else,
                                 Tok_For,
                                 Tok_While,
                                 Tok_Do,
                                 Tok_Continue,
                                 Tok_Break,
                                 Tok_Return,
                                 Tok_EOF);


   package Map is new Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                                             Element_Type    => Natural,
                                                             Hash            => Ada.Strings.hash,
                                                             Equivalent_Keys => "=");


   type Token_Record_Type(Has_Value : Boolean := False) is record
      Token_Type : Token_Type_Enum_Type;
      Line       : Positive;
      case Has_Value is
         when True => Value : Natural;
         when False => null;
      end case;
   end record;

   function Debug_Print (T : Token_Record_Type) return string;


end Token;
