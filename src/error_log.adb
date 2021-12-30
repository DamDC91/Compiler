with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Strings.Fixed;
with Tree_Graphviz;
package body Error_Log is

   Current_FileName : Unbounded_String;

   Warning_On : Boolean := False;

   Debug_On : Boolean := False;

   procedure Set_Warning_On (W : Boolean) is
   begin
      Warning_On := W;
   end Set_Warning_On;

   procedure Set_Filename(str : string) is
   begin
      Current_FileName := To_Unbounded_String (Str);
   end Set_Filename;

   procedure Error (Msg : String; Line : Natural) is
      FileName : constant string := To_String (Current_FileName);
      File : File_Type;
   begin
      Put_Line (Standard_Error, FileName & ":" & Line'Image & ": error " & Msg);

      Open(File, In_File, FileName);
      if Line > 1 then
         Skip_Line (File => File,
                    Spacing => Ada.Text_IO.Count (Line-1));
      end if;
      Put_Line(Standard_Error, "   " & Line'Image & " | " & Get_Line(File));

      Close(File);
   end Error;


   procedure Warning (Msg : String; Line : Natural) is
      FileName : constant string := To_String (Current_FileName);
      File : File_Type;
   begin
      If Warning_On then
         Put_Line (Standard_Error, FileName & ":" & Line'Image & ": Warning " & Msg);

         Open(File, In_File, FileName);
         if Line > 1 then
            Skip_Line (File => File,
                       Spacing => Ada.Text_IO.Count (Line-1));
         end if;
         Put_Line(Standard_Error, "   " & Line'Image & " | " & Get_Line(File));

         Close(File);
      end if;
   end Warning;

   procedure Set_Debug_On (B : Boolean) is
   begin
      Debug_On := B;
   end Set_Debug_On;

   function Get_Debug_On return Boolean is
   begin
      return Debug_On;
   end Get_Debug_On;


   function Debug_Print (N : Syntaxic_Analysis.Node_Variant_Type) return string is
   begin
      case N.Node_Type is
         when Syntaxic_Analysis.Node_Constant =>
            return "(" & N.Node_Type'Image & ", Val:" & N.Value'Image & ", l:" & N.Line'Image & ")";
         when Syntaxic_Analysis.Node_Var_Decl =>
            return "(" & N.Node_Type'Image & ", Var_Key:" & N.Var_Key'Image & ", l:" & N.Line'Image & ")";
         when Syntaxic_Analysis.Node_Var_Ref =>
            return "(" & N.Node_Type'Image & ", Ref_Var_Key:" & N.Ref_Var_Key'Image & ", Var_Stack_Idx:" & N.Var_Stack_Index'Image & " l:" & N.Line'Image & ")";
         when others =>
            return "(" & N.Node_Type'Image & ", l:" & N.Line'Image & ")";
      end case;
   end Debug_Print;

   procedure Debug_Print_Tree (T : Syntaxic_Analysis.Tree.Tree) is
      Debug_File_Tree : Ada.Text_IO.File_Type;

      procedure Print_Tree (C : Syntaxic_Analysis.Tree.Cursor) is
         use Ada.Strings.Fixed;
         N : constant Syntaxic_Analysis.Node_Variant_Type := Syntaxic_Analysis.Tree.Element (C);
         P : constant Natural := Natural (Syntaxic_Analysis.Tree.Depth (C))-2;
         str : constant string := P * "----";
      begin
         Ada.Text_IO.Put (Debug_File_Tree, str);
         Ada.Text_IO.Put_Line (Debug_File_Tree, Debug_Print (N));
      end Print_Tree;

      FileName : constant String := "tree_" & Ada.Directories.Base_Name (To_String (Current_FileName)) & ".txt";
      use type Ada.Directories.File_Kind;
   begin
      if Ada.Directories.Exists (FileName) and then Ada.Directories.Kind (FileName) /= Ada.Directories.Ordinary_File then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "error : '" & FileName & "' cannot be created");
         raise Error_Log.Compilation_Error;
      end if;
      Ada.Text_IO.Create (File => Debug_File_Tree,
                          Mode => Ada.Text_IO.Out_File,
                          Name => FileName);
      Syntaxic_Analysis.Tree.Iterate (T, Print_Tree'Access);
      Ada.Text_IO.Close (Debug_File_Tree);
   end Debug_Print_Tree;




   procedure Debug_Print_Tree_Graphviz (T : Syntaxic_Analysis.Tree.Tree) is
      function Get_label (C : Syntaxic_Analysis.Tree.Cursor) return string is
         N : constant Syntaxic_Analysis.Node_Variant_Type := Syntaxic_Analysis.Tree.Element (C);
         Img : constant String := N.Node_Type'Image;
         Index : constant Integer := Ada.Strings.Fixed.Index (Img, "_");
         Node_Img : constant String := Img (Index + 1..Img'Last);
      begin
         case N.Node_Type is
         when Syntaxic_Analysis.Node_Constant =>
            return Node_Img & "(Val:" & N.Value'Image &")";
         when Syntaxic_Analysis.Node_Var_Decl =>
            return Node_Img & "(Var_Key:" & N.Var_Key'Image & ")";
         when Syntaxic_Analysis.Node_Var_Ref =>
            return Node_Img & "(Ref_Var_Key:" & N.Ref_Var_Key'Image & ", Var_Stack_Idx:" & N.Var_Stack_Index'Image & ")";
         when others =>
            return Node_Img;
         end case;
      end Get_label;
      function Get_arrow (C : Syntaxic_Analysis.Tree.Cursor) return string is
         pragma Unreferenced(C);
      begin
         return "";
      end Get_arrow;
      package Graphviz is new Tree_Graphviz (Cursor          => Syntaxic_Analysis.Tree.Cursor,
                                             Depth           => Syntaxic_Analysis.Tree.Depth,
                                             Get_Node_Label  => Get_label,
                                             Get_Arrow_Label => Get_arrow);
      F : File_Type;
      FileName : constant String := "tree_" & Ada.Directories.Base_Name (To_String (Current_FileName)) & ".gv";
      use type Ada.Directories.File_Kind;
   begin
      if Ada.Directories.Exists (FileName) and then Ada.Directories.Kind (FileName) /= Ada.Directories.Ordinary_File then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "error : '" & FileName & "' cannot be created");
         raise Error_Log.Compilation_Error;
      end if;
      Create (File => F,
              Mode => Out_File,
              Name => FileName);
      Put_Line (F, "diGraph Tree {");
      For C in T.Iterate loop
         Graphviz.Put (F, C);
      end loop;
      Put_Line (F, "}");
      close (F);
   end Debug_Print_Tree_Graphviz;


end Error_Log;
