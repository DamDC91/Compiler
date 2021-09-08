with Ada.Containers;
with Ada.Text_IO;

generic 
   type Cursor is private; 
   with function Depth (C : Cursor) return Ada.Containers.Count_Type;
   with function Get_Node_Label (C : Cursor) return String;
   with function Get_Arrow_Label (C : Cursor) return String;

package Tree_Graphviz is

   procedure Put (File : Ada.Text_IO.File_Type; C : Cursor);
      
      
end Tree_Graphviz;
