with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Fixed;
package body Asm_Generation is
   
   File : File_Type;
   
   
   procedure Create_File (FileName : String) is
   begin      
      Create (File => File,
              Mode => Out_File,
              Name => FileName);
      Put_Line (File, ".start");
   end Create_File;
   
   procedure Close_File is
   begin
      Put_Line (File, "dbg"); -- print top of the stck
      Put_Line (File, "halt");
      Close (File);
   end Close_File;
   
   Function Image (Nb : Integer) return string is (Ada.Strings.Fixed.Trim (Nb'Image, Ada.Strings.Both));
   
   procedure Generate_Asm (C : Syntaxic_Analysis.Tree.Cursor) is
      Node : constant Syntaxic_Analysis.Node := Syntaxic_Analysis.Tree.Element (C);
   begin
      case Node.Node_Type is
         when Syntaxic_Analysis.Node_Constant =>
            Put_Line (File, "push " & Image(Node.Value));
         when Syntaxic_Analysis.Node_Minus_U =>
            Put_Line (File, "push 0");
            Generate_Asm (Syntaxic_Analysis.Tree.First_Child(C));
            Put_Line (File, "sub");
         when Syntaxic_Analysis.Node_Not =>
            Generate_Asm (Syntaxic_Analysis.Tree.First_Child(C));
            Put_Line (File, "not");
         when Syntaxic_Analysis.Node_Address =>
            null; --TODO
         when Syntaxic_Analysis.Node_Dereference =>
            null; --TODO
      end case;
   end Generate_Asm;
   

end Asm_Generation;
