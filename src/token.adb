package body Token is

   function Debug_Print (T : Token_Record_Type) return String is
   begin
      if T.Has_Value then
         return  "(" & T.Token_Type'Image & ", Val:" & T.Value'Image & ", l:" &T.Line'Image & ")";
      else
         return "(" & T.Token_Type'Image & ", l:" & T.Line'Image & ")";
      end if;
   end Debug_Print;

end Token;
