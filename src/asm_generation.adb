with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Containers;
with Error_Log;
use Error_Log;
with Lexical_Analysis;
with Text_Utils;
package body Asm_Generation is
   
   File : File_Type;
   Tab : constant string := "    ";
   
   procedure Create_File (FileName : String) is
   begin      
      Create (File => File,
              Mode => Ada.Text_IO.Out_File,
              Name => FileName);

   end Create_File;
   
   procedure Add_Runtime (Runtime : String) is
      str : constant String := Text_Utils.Get_File_Content (Runtime);
   begin
      Put_Line (File, str(str'First..str'Last-1));      
   end Add_Runtime;
   
   procedure Close_File is
   begin
      Close (File);
   end Close_File;
   
   procedure Delete_File is
   begin
      Delete (File);
   end Delete_File;
   
   procedure Add_Start (Start_Filename : String) is
      str : constant String := Text_Utils.Get_File_Content (Start_Filename);
   begin
      Put_Line (File, str(str'First..str'Last-1)); 
   end Add_Start;
   
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
      
      
      procedure Call_Generate_Asm (Cu : Syntaxic_Analysis.Tree.Cursor) is
      begin
         Generate_Asm (C => Cu);
      end Call_Generate_Asm;
   begin
      case Node.Node_Type is
         when Syntaxic_Analysis.Node_Constant =>
            Put_Line (File, Tab & "push " & Image(Node.Value));
            
         when Syntaxic_Analysis.Node_Minus_U =>
            Put_Line (File, Tab & "push 0");
            if Syntaxic_Analysis.Tree.Child_Count (C) /= 1 then
               raise Constraint_Error with "AST invalid";
            end if;
            Generate_Asm (C => Syntaxic_Analysis.Tree.First_Child(C));
            Put_Line (File, Tab & "sub");
            
         when Syntaxic_Analysis.Node_Not =>
            if Syntaxic_Analysis.Tree.Child_Count (C) /= 1 then
               raise Constraint_Error with "AST invalid";
            end if;
            Generate_Asm (C => Syntaxic_Analysis.Tree.First_Child(C));
            Put_Line (File, Tab & "not");
            
         when Syntaxic_Analysis.Node_Address =>
            declare
               Child : constant Syntaxic_Analysis.Node_Variant_Type := Syntaxic_Analysis.Tree.First_Child_Element (C);
               Index : constant Natural := (Child.Var_Stack_Index + 1);
            begin
               Put_Line (File, Tab & "prep start2");
               Put_Line (File, Tab & "add");
               Put_Line (File, Tab & "prep start2");
               Put_Line (File, Tab & "drop");
               Put_Line (File, Tab & "sub");
               Put_Line (File, Tab & "push " & Index'Image);
               Put_Line (File, Tab & "sub");
            end;
         when Syntaxic_Analysis.Node_Dereference =>
            Generate_Asm (C => Syntaxic_Analysis.Tree.First_Child (C));
            declare
               use type Syntaxic_Analysis.Node_Type_Enum_Type;
            begin
               if Syntaxic_Analysis.Tree.Has_Element (Syntaxic_Analysis.Tree.Next_Sibling (C)) and
               Syntaxic_Analysis.Tree.Element (Syntaxic_Analysis.Tree.Parent (C)).Node_Type = Syntaxic_Analysis.Node_Op_Assignment then -- L value
                  Put_Line (File, Tab & "write");
               else -- R value
                  Put_Line (File, Tab & "read");
               end if;
            end;
            
         when Syntaxic_Analysis.Operation_Node_Enum_Type =>
            if Syntaxic_Analysis.Tree.Child_Count (C) /= 2 then
               raise Compilation_Error with "AST invalid";
            end if;
            Generate_Asm (C => Syntaxic_Analysis.Tree.First_Child(C));
            Generate_Asm (C => Syntaxic_Analysis.Tree.Last_Child(C));
            Put_Line (File, Tab &  Get_Asm_Instruction (Node.Node_Type));      
         when Syntaxic_Analysis.Node_Drop =>
            if Syntaxic_Analysis.Tree.Child_Count (C) /= 1 then
               raise Compilation_Error with "AST invalid";
            end if;
            Generate_Asm (C => Syntaxic_Analysis.Tree.First_Child(C));
            Put_Line(File, Tab & "drop");
         when Syntaxic_Analysis.Node_Instruction_Block =>

            Syntaxic_Analysis.Tree.Iterate_Children (Parent  => C,
                                                     Process => Call_Generate_Asm'Access);
            
         when Syntaxic_Analysis.Node_Debug =>
            if Syntaxic_Analysis.Tree.Child_Count (C) /= 1 then
               raise Compilation_Error with "AST invalid";
            end if;
            Generate_Asm (C => Syntaxic_Analysis.Tree.First_Child(C));
            Put_Line (File, Tab & "dbg");
            
         when Syntaxic_Analysis.Node_Var_Decl => 
            if Syntaxic_Analysis.Tree.Child_Count (C) = 1 then
               Generate_Asm (C => Syntaxic_Analysis.Tree.First_Child(C));
            end if;
            
         when Syntaxic_Analysis.Node_Var_Ref => 
            Put_Line (File, Tab & "get "& Node.Var_Stack_Index'Image & " ; " & Lexical_Analysis.Get_Str_From_Assoc_Table (Node.Ref_Var_Key));
            
         when Syntaxic_Analysis.Node_Op_Assignment =>
            declare
               First_Child : constant Syntaxic_Analysis.Tree.Cursor := Syntaxic_Analysis.Tree.First_Child(C);
               First_Child_Node : constant Syntaxic_Analysis.Node_Variant_Type := Syntaxic_Analysis.Tree.Element (First_Child);
               use type Syntaxic_Analysis.Node_Type_Enum_Type;
            begin
               if First_Child_Node.Node_Type /= Syntaxic_Analysis.Node_Var_Ref and First_Child_Node.Node_Type /= Syntaxic_Analysis.Node_Dereference then
                  raise Compilation_Error with "Left operand is not a variable";
               end if;
               if First_Child_Node.Node_Type = Syntaxic_Analysis.Node_Var_Ref then
                  Generate_Asm (c => Syntaxic_Analysis.Tree.Last_Child(C));
                  Put_Line (File, Tab & "dup");
                  Put_Line (File, Tab & "set " & First_Child_Node.Var_Stack_Index'Image & " ; " & Lexical_Analysis.Get_Str_From_Assoc_Table (First_Child_Node.Ref_Var_Key));
               else
                  Generate_Asm (C => Syntaxic_Analysis.Tree.Last_Child(C));
                  Put_Line (File, Tab & "dup");
                  Generate_Asm (C => First_Child);
                  --Put_Line (File, Tab & "write");
               end if;
            end;
            
               
         when Syntaxic_Analysis.Node_Seq => 
            Syntaxic_Analysis.Tree.Iterate_Children (Parent  => C,
                                                     Process => Call_Generate_Asm'Access);
         when Syntaxic_Analysis.Node_Cond => 
            if Syntaxic_Analysis.Tree.Child_Count (C) /= 2 and Syntaxic_Analysis.Tree.Child_Count (C) /= 3 then
               raise Compilation_Error with "AST invalid";
            end if;
            declare
               Cond_Nb : constant Positive := Syntaxic_Analysis.Tree.Element(C).Cond_Count;
               First_Child : constant Syntaxic_Analysis.Tree.Cursor := Syntaxic_Analysis.Tree.First_Child(C);
            
               Second_Child : constant Syntaxic_Analysis.Tree.Cursor := Syntaxic_Analysis.Tree.Next_Sibling (First_Child);
            
               Third_Child : constant Syntaxic_Analysis.Tree.Cursor := Syntaxic_Analysis.Tree.Next_Sibling(Second_Child);
            begin
               if Syntaxic_Analysis.Tree.Child_Count (C) = 3 then
                  Generate_Asm (C => First_Child);
                  Put_Line (File, Tab & "jumpf cond_else_" & Image (Cond_Nb));
                  Generate_Asm (C => Second_Child);
                  Put_Line (File, Tab & "jump cond_end_" & Image (Cond_Nb));
                  Put_Line (File, ".cond_else_" & Image (Cond_Nb));
                  Generate_Asm(C => Third_Child);
                  Put_Line (File, ".cond_end_" & Image (Cond_Nb));
               else
                  Generate_Asm (C => First_Child);
                  Put_Line (File, Tab & "jumpf cond_end_" & Image (Cond_Nb));
                  Generate_Asm (C => Second_Child);
                  Put_Line (File, ".cond_end_" & Image (Cond_Nb));
               end if;
            end;
         when Syntaxic_Analysis.Node_Break => 
            Put_Line (File, Tab & "jump end_loop_" & Image(Syntaxic_Analysis.Tree.Element (C).Loop_Count));
            
         when Syntaxic_Analysis.Node_Continue =>
            Put_Line (File, Tab & "jump cont_loop_" & Image(Syntaxic_Analysis.Tree.Element (C).Loop_Count));
            
         when Syntaxic_Analysis.Node_Label =>
            Put_Line (File, ".cont_loop_" & Image(Syntaxic_Analysis.Tree.Element (C).Loop_Count));
            Syntaxic_Analysis.Tree.Iterate_Children (Parent  => C,
                                                     Process => Call_Generate_Asm'Access);
         when Syntaxic_Analysis.Node_Loop =>
            declare
               Current_Loop_Nb : Constant Positive := Syntaxic_Analysis.Tree.Element (C).Loop_Count;
            begin
               if Syntaxic_Analysis.Tree.Child_Count(C) = 1 or Syntaxic_Analysis.Tree.Child_Count (C) = 3 then
                  Put_Line (File, ".start_loop_" & Image (Current_Loop_Nb));
                  
                  Syntaxic_Analysis.Tree.Iterate_Children (Parent  => C,
                                                           Process => Call_Generate_Asm'Access);
                  Put_Line (File, Tab & "jump start_loop_" & Image (Current_Loop_Nb));
                  Put_Line (File, ".end_loop_" & Image (Current_Loop_Nb));               
               else
                  Raise Compilation_Error with "AST Invalid";
               end if;
            end;
         when Syntaxic_Analysis.Node_Call =>
            Put_Line (File, Tab & "prep " & Lexical_Analysis.Get_Str_From_Assoc_Table (Node.Ref_Func_Key));
            Syntaxic_Analysis.Tree.Iterate_Children (Parent  => C,
                                                     Process => Call_Generate_Asm'Access);
            Put_Line (File, Tab & "call " & Integer (Syntaxic_Analysis.Tree.Child_Count (C))'Image);
         when Syntaxic_Analysis.Node_Body_Func =>
            New_Line (File); 
            Put_Line (File, "." & Lexical_Analysis.Get_Str_From_Assoc_Table (Node.Name_Key));
            Put_Line (File, Tab & "resn " & Node.Nb_Var'Image);
            Generate_Asm (C => Syntaxic_Analysis.Tree.Last_Child (C));
            Put_Line (File, Tab & "push 0");
            Put_Line (File, Tab & "ret");
            
         when Syntaxic_Analysis.Node_Return =>
            Generate_Asm (C => Syntaxic_Analysis.Tree.First_Child (C));
            Put_Line (File, Tab & "ret");
            
      end case;
   end Generate_Asm;
   

end Asm_Generation;
