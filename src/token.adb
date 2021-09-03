with Ada.Text_IO;
package body Token is

   procedure Debug_Put_Line(T : Token_Record_Type) is
   begin
      if T.Has_Value then
         Ada.Text_IO.Put_Line("(" & T.Token_Type'Image & ", Val:" & T.Value'Image & ", l:" &T.Line'Image & ")");
      else
         Ada.Text_IO.Put_Line("(" & T.Token_Type'Image & ", l:" & T.Line'Image & ")");
      end if;
   end Debug_Put_Line;

end Token;
