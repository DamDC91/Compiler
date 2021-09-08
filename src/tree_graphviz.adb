with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
package body Tree_Graphviz is

   package Str_Stack is new Ada.Containers.Indefinite_Vectors (Index_Type   => Natural,
                                                               Element_Type => String,
                                                               "="          => "=");

   Stack         : Str_Stack.Vector;
   Last_Depth    : Ada.Containers.Count_Type := 0;
   Index         : Natural := 1;
   
   procedure Put (File : Ada.Text_IO.File_Type; C : Cursor)
   is 
      Node_Label : constant String := Get_Node_Label (C);
      Arrow_Label : constant String := Get_Arrow_Label (C);
      Node : constant String := "node" & Ada.Strings.Fixed.Trim (Index'Image, Ada.Strings.Both);
      Current_Depth : constant Ada.Containers.Count_Type := Depth (C);

      use Ada.Text_IO;
      use type Ada.Containers.Count_Type;
   begin
      
      Put_Line(File, Node & "[label= """ & Node_Label & """ ];");

      if Last_Depth >= Current_Depth then
         for I in 1 .. Last_Depth - Current_Depth + 1 loop
            Stack.Delete_Last;
         end loop;
      end if;

      if not Stack.Is_Empty then
         Put_Line (File, Stack.Last_Element & " -> " & Node & "[label=""" & Arrow_Label & """];");
      end if;

      Stack.Append (Node);
      Last_Depth := Current_Depth;
      Index := Index + 1;
   end Put;

end Tree_Graphviz;
