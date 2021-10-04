package Error_Log is

   Compilation_Error : Exception;

   procedure Error (Msg : String; Line : Natural);

   procedure Warning (Msg : String; Line : Natural);

   procedure Set_Filename(str : string);
end Error_Log;
