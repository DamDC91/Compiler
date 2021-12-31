with Syntaxic_Analysis;
package Asm_Generation is

   procedure Create_File (FileName : String);
   
   procedure Add_Runtime (Runtime : String);
   
   procedure Close_File;
  
   procedure Generate_Asm (C : Syntaxic_Analysis.Tree.Cursor);
   
   procedure Add_Start (Start_Filename : String);

end Asm_Generation;
