with Syntaxic_Analysis;
with Token;
package Error_Log is

   Input_Error : Exception;

   Internal_Error : Exception;

   procedure Error (Msg : String; Line : Natural);

   procedure Warning (Msg : String; Line : Natural);

   procedure Debug_Print_Tree (T : Syntaxic_Analysis.Tree.Tree);

   procedure Debug_Print_Tree_Graphviz (T : Syntaxic_Analysis.Tree.Tree);

   procedure Debug_Print_Token (T : Token.Token_Record_Type);

   procedure Create_Token_File (FileName : String);

   procedure Close_Token_File;

   procedure Set_Filename(str : string);

   procedure Set_Warning_On (W : Boolean);

   procedure Set_Debug_On (B : Boolean);

   procedure Set_Output_Dir (s : String);

end Error_Log;
