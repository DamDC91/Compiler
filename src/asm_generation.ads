with Syntaxic_Analysis;
package Asm_Generation is

   procedure Create_File (FileName : String);
   
   procedure Close_File;
   
   procedure Generate_Asm (C : Syntaxic_Analysis.Tree.Cursor);

end Asm_Generation;