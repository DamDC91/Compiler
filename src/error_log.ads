with Syntaxic_Analysis;
package Error_Log is

   Compilation_Error : Exception;

   procedure Error (Msg : String; Line : Natural);

   procedure Warning (Msg : String; Line : Natural);

   procedure Debug_Print_Tree (T : Syntaxic_Analysis.Tree.Tree);

   procedure Debug_Print_Tree_Graphviz (T : Syntaxic_Analysis.Tree.Tree);

   procedure Set_Filename(str : string);

   procedure Set_Warning_On (W : Boolean);

   procedure Set_Debug_On (B : Boolean);

   function Get_Debug_On return Boolean;

end Error_Log;
