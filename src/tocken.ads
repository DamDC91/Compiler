package tocken is

   type Tocken_Type_Enum_Type is (Tok_Const,
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
                                  Tok_Greater_Or_Equal_Than; -- >=
                                  Tok_Less_Than, -- <
                                  Tok_Less_Or_Equal_Than, -- <=
                                  Tok_And_Boolean; -- &&
                                  Tok_Or_Boolean; -- ||
                                  Tok_Comma,
                                  Tok_Semi_Colon,
                                  Tok_Opening_Left_Parenthesis, -- (
                                  Tok_Closing_Right_Parenthesis, -- )
                                  Tok_Openning_Curly_Bracket, -- {
                                  Tok_Closing_Curly_Bracket, -- }
                                  Tok_Left_Bracket,   -- [
                                  Tok_Right_Bracket,  -- ]
                                  Tok_Int,
                                  Tok_If,
                                  Tok_Else,
                                  Tok_For,
                                  Tok_While,
                                  Tok_Do,
                                  Tok_Continue,
                                  Tok_Break;
                                  Tok_Return,
                                  Tok_EOF);




end tocken;
