with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Containers;
with Semantic_Analysis;
package body Asm_Generation is
   
   File : File_Type;
   
   procedure Create_File (FileName : String) is
   begin      
      Create (File => File,
              Mode => Out_File,
              Name => FileName);
      Put_Line (File, ".start");
      Put_Line (File , "resn " & Semantic_Analysis.Get_Nb_Var'Image);
   end Create_File;
   
   procedure Close_File is
   begin
      --Put_Line (File, "dbg"); -- print top of the stck
      Put_Line (File, "halt");
      Close (File);
   end Close_File;
   
   Function Image (Nb : Integer) return string is (Ada.Strings.Fixed.Trim (Nb'Image, Ada.Strings.Both));
   
   function Get_Asm_Instruction (N : Syntaxic_Analysis.Operation_Node_Enum_Type) return String 
   is
      use Syntaxic_Analysis;
   begin
      case N is
         when Node_Op_Or_Boolean => return "or";
         when Node_Op_And_Boolean => return "and";
         when Node_Op_Equal_Comparaison => return "cmpeq";
         when Node_Op_Difference_Comparaison => return "cmpne";
         when Node_Op_Greater_than => return "cmpgt";
         when Node_Op_Greater_Or_Equal_Than => return "cmpge";
         when Node_Op_Less_Than => return "cmplt";
         when Node_Op_Less_Or_Equal_Than => return "cmple";
         when Node_Op_Add => return "add";
         when Node_Op_Sub => return "sub";
         when Node_Op_Mult => return "mul";
         when Node_Op_Division => return "div";
         when Node_Op_Modulo => return "mod";
      end case;
   end Get_Asm_Instruction;
   
   procedure Generate_Asm (C : Syntaxic_Analysis.Tree.Cursor) is
      Node : constant Syntaxic_Analysis.Node_Variant_Type := Syntaxic_Analysis.Tree.Element (C);
      use type Ada.Containers.Count_Type;
   begin
      case Node.Node_Type is
         when Syntaxic_Analysis.Node_Constant =>
            Put_Line (File, "push " & Image(Node.Value));
            
         when Syntaxic_Analysis.Node_Minus_U =>
            Put_Line (File, "push 0");
              if Syntaxic_Analysis.Tree.Child_Count (C) /= 1 then
                 raise Constraint_Error with "AST invalid";
              end if;
            Generate_Asm (Syntaxic_Analysis.Tree.First_Child(C));
            Put_Line (File, "sub");
            
         when Syntaxic_Analysis.Node_Not =>
              if Syntaxic_Analysis.Tree.Child_Count (C) /= 1 then
                 raise Constraint_Error with "AST invalid";
              end if;
            Generate_Asm (Syntaxic_Analysis.Tree.First_Child(C));
            Put_Line (File, "not");
            
         when Syntaxic_Analysis.Node_Address =>
            null; --TODO
         when Syntaxic_Analysis.Node_Dereference =>
            null; --TODO
            
         when Syntaxic_Analysis.Operation_Node_Enum_Type =>
              if Syntaxic_Analysis.Tree.Child_Count (C) /= 2 then
                 raise Constraint_Error with "AST invalid";
              end if;
            Generate_Asm (Syntaxic_Analysis.Tree.First_Child(C));
            Generate_Asm (Syntaxic_Analysis.Tree.Last_Child(C));
            Put_Line (File, Get_Asm_Instruction (Node.Node_Type));      
         when Syntaxic_Analysis.Node_Drop =>
              if Syntaxic_Analysis.Tree.Child_Count (C) /= 1 then
                 raise Constraint_Error with "AST invalid";
              end if;
            Generate_Asm (Syntaxic_Analysis.Tree.First_Child(C));
            Put_Line(File, "drop");
         when Syntaxic_Analysis.Node_Instruction_Block =>

            Syntaxic_Analysis.Tree.Iterate_Children (Parent  => C,
                                                     Process => Generate_Asm'Access);
            
         when Syntaxic_Analysis.Node_Debug =>
              if Syntaxic_Analysis.Tree.Child_Count (C) /= 1 then
                 raise Constraint_Error with "AST invalid";
              end if;
            Generate_Asm (Syntaxic_Analysis.Tree.First_Child(C));
            Put_Line (File, "dbg");
            
         when Syntaxic_Analysis.Node_Var_Decl => 
            null; --nothing to do
            
         when Syntaxic_Analysis.Node_Var_Ref => 
            Put_Line (File, "get "& Node.Var_Stack_Index'Image);
            
         when Syntaxic_Analysis.Node_Op_Assignment =>
            declare
               First_Child : constant Syntaxic_Analysis.Tree.Cursor := Syntaxic_Analysis.Tree.First_Child(C);
               First_Child_Node : constant Syntaxic_Analysis.Node_Variant_Type := Syntaxic_Analysis.Tree.Element (First_Child);
               use type Syntaxic_Analysis.Node_Type_Enum_Type;
            begin
               if First_Child_Node.Node_Type /= Syntaxic_Analysis.Node_Var_Ref then
                  raise Constraint_Error with "Left operand is not a variable";
               end if;
               Generate_Asm (c => Syntaxic_Analysis.Tree.Last_Child(C));
               Put_Line (File, "dup");
               Put_Line (File, "set " & First_Child_Node.Var_Stack_Index'Image);
            end;
            
               
         when Syntaxic_Analysis.Node_Seq => 
                        Syntaxic_Analysis.Tree.Iterate_Children (Parent  => C,
                                                     Process => Generate_Asm'Access);
      end case;
   end Generate_Asm;
   

end Asm_Generation;
