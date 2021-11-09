with Lexical_Analysis;
with Syntaxic_Analysis;
with Ada.Strings.Fixed;
with Asm_Generation;
with GNATCOLL.Opt_Parse;
with Ada.Strings.Unbounded;
with Ada.Directories;
with Semantic_Analysis;
with Error_Log;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Calendar;
with Ada.Exceptions;
procedure main is

   package Args is
      use GNATCOLL.Opt_Parse;
      Parser : Argument_Parser := Create_Argument_Parser (Help => "reduce C Compiler for msm");

      package Files is new Parse_Positional_Arg_List (Parser      => Parser,
                                                      Name        => "files",
                                                      Help        => "Files to compile",
                                                      Allow_Empty => False,
                                                      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String);

      package debug is new Parse_Flag (Parser  => Parser,
                                       Short   => "-d",
                                       Long    => "--debug",
                                       Help    => "Print debug infos");

   end Args;

   Files_Dir : constant String := Ada.Directories.Current_Directory;
   Command : constant String := Ada.Directories.Full_Name (Ada.Command_Line.Command_Name);
   Asm_Filename : constant String := Files_Dir & "/out.s";
   Id : constant Natural := Ada.Strings.Fixed.Index (Source  => Command,
                                                     Pattern => "/",
                                                     From    => Command'Last,
                                                     Going   => Ada.Strings.Backward);

   Runtime_Dir : constant String := (if Id /= 0 then Command(Command'First .. Id) & "../runtime/" else "../runtime/");
   Runtime_Source_File : constant String := Runtime_Dir & "runtime.c";
   Runtime_Asm_File : constant String := Runtime_Dir & "runtime.s";
   Start_File : constant string := Runtime_Dir & "start.s";
   use type Ada.Calendar.Time;
begin


   Syntaxic_Analysis.Init;


   -- Compiling runtime only if the code as been touch since the last compilation
   Lexical_Analysis.Load(Runtime_Source_File, False);
   Error_Log.Set_Filename  (Runtime_Source_File);
   declare
      T : Syntaxic_Analysis.Tree.Tree := Syntaxic_Analysis.G;
   begin
      Semantic_Analysis.AST_Analyse (T);


      Syntaxic_Analysis.Debug_Print_Tree (T);
      Syntaxic_Analysis.Debug_Print_Tree_Graphviz (T);
      Lexical_Analysis.Close_Debug;


      -- only run syntaxic and semantic analysis for setting loup and cond counter and the symbol table
      if Ada.Directories.Exists (Runtime_Asm_File) and then Ada.Directories.Modification_Time (Runtime_Asm_File) < Ada.Directories.Modification_Time (Runtime_Source_File) then
         Asm_Generation.Create_File(Filename => Runtime_Asm_File);
         Asm_Generation.Generate_Asm (Syntaxic_Analysis.Tree.First_Child (T.Root));
         Asm_Generation.Close_File;
         Ada.Text_IO.Put_Line ("runtime compilation is ok");
      else
         Ada.Text_IO.Put_Line ("runtime is not recompile");
      end if;

   exception
      when e : others =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "rutime compilation failed");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message(e));
         if Ada.Directories.Exists (Runtime_Asm_File) then
            Asm_Generation.Close_File;
            Asm_Generation.Delete_File;
         end if;
         raise;
   end;


   Asm_Generation.Create_File (Asm_Filename);

   Asm_Generation.Add_Runtime (Runtime  => Runtime_Asm_File);


   Ada.Directories.Set_Directory (Files_Dir);



   if Args.Parser.Parse then
      declare
         Is_Debug_Mode : constant Boolean := Args.debug.Get;
         Files_Array   : constant Args.Files.Result_Array := Args.Files.Get;

      begin

         for i in Files_Array'First ..Files_Array'Last loop

            declare
               FileName : constant String := Ada.Strings.Unbounded.To_String (Files_Array (i));
            begin
               Error_Log.Set_Filename  (FileName);
               Lexical_Analysis.Load(FileName, Is_Debug_Mode);


               declare
                  T : Syntaxic_Analysis.Tree.Tree := Syntaxic_Analysis.G;
               begin
                  Semantic_Analysis.AST_Analyse (T);

                  if Is_Debug_Mode then
                     Syntaxic_Analysis.Debug_Print_Tree (T);
                     Syntaxic_Analysis.Debug_Print_Tree_Graphviz (T);
                  end if;

                  Asm_Generation.Generate_Asm (Syntaxic_Analysis.Tree.First_Child (T.Root));

               end;

            end;
            Lexical_Analysis.Close_Debug;

         end loop;


      end;

      Asm_Generation.Add_Start (Start_Filename => Start_File);

      Asm_Generation.Close_File;
   end if;



end main;
