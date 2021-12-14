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
with GNAT.OS_Lib;

procedure main is

   package Args is
      use GNATCOLL.Opt_Parse;
      Parser : Argument_Parser := Create_Argument_Parser (Help => "reduce C Compiler for msm");

      package Files is new Parse_Positional_Arg_List (Parser      => Parser,
                                                      Name        => "files",
                                                      Help        => "Files to compile",
                                                      Allow_Empty => False,
                                                      Arg_Type    => Ada.Strings.Unbounded.Unbounded_String);

      package warning is new Parse_Flag (Parser  => Parser,
                                         Short   => "-w",
                                         Long    => "--warning",
                                         Help    => "Print warning");

      package debug is new Parse_Flag (Parser  => Parser,
                                       Short   => "-d",
                                       Long    => "--debug",
                                       Help    => "Print debug infos");

      package Output_Filename is new Parse_Option (Parser      => Parser,
                                                   Short       => "-o",
                                                   Long        => "--output",
                                                   Help        => "Output file name",
                                                   Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
                                                   Default_Val => Ada.Strings.Unbounded.To_Unbounded_String ("out.s"));

      package Compile_Runtime is new Parse_Flag (Parser  => Parser,
                                                 Short   => "-r",
                                                 Long    => "--compile_runtime",
                                                 Help    => "force runtime compilation");

   end Args;

begin

   if Args.Parser.Parse then

      declare
         Files_Dir : constant String := Ada.Directories.Current_Directory;
         Command : constant String := Ada.Directories.Full_Name (Ada.Command_Line.Command_Name);
         Asm_Filename : constant String := Files_Dir & "/" & Ada.Strings.Unbounded.To_String (Args.Output_Filename.Get);
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
         Error_Log.Set_Debug_On (Args.Compile_Runtime.Get and Args.debug.Get);
         Lexical_Analysis.Load (Runtime_Source_File);
         Error_Log.Set_Filename (Runtime_Source_File);
         declare
            T : Syntaxic_Analysis.Tree.Tree := Syntaxic_Analysis.G;
         begin
            Semantic_Analysis.AST_Analyse (T);

            -- only run syntaxic and semantic analysis for setting loup and cond counter and the symbol table
            if Args.Compile_Runtime.Get then
               Asm_Generation.Create_File(Filename => Runtime_Asm_File);
               Asm_Generation.Generate_Asm (Syntaxic_Analysis.Tree.First_Child (T.Root));
               Asm_Generation.Close_File;
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output, "runtime compilation succeed");

            elsif Ada.Directories.Exists (Runtime_Asm_File) and then Ada.Directories.Modification_Time (Runtime_Asm_File) < Ada.Directories.Modification_Time (Runtime_Source_File) then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Output, "runtime has been modified, should be compile");

            elsif not Ada.Directories.Exists (Runtime_Asm_File) then
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Runtime need to be compile first, please add -r option");
               raise Error_Log.Compilation_Error;
            end if;

         exception
            when others =>
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "rutime compilation failed");
               if Ada.Directories.Exists (Runtime_Asm_File) then
                  Asm_Generation.Close_File;
                  Asm_Generation.Delete_File;
               end if;
               GNAT.OS_Lib.OS_Exit(1);
         end;

         Asm_Generation.Create_File (Asm_Filename);
         Asm_Generation.Add_Runtime (Runtime  => Runtime_Asm_File);

         Ada.Directories.Set_Directory (Files_Dir);

         declare
            Files_Array   : constant Args.Files.Result_Array := Args.Files.Get;
         begin
            Error_Log.Set_Warning_On (Args.warning.Get);
            for i in Files_Array'First ..Files_Array'Last loop

               declare
                  FileName : constant String := Ada.Strings.Unbounded.To_String (Files_Array (i));
               begin

                  if not Ada.Directories.Exists (FileName) then
                     Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "error : '" & FileName & "': not such file");
                     raise Error_Log.Compilation_Error;
                  end if;
                  Error_Log.Set_Filename  (FileName);
                  Error_Log.Set_Debug_On (Args.debug.Get);
                  Lexical_Analysis.Load(FileName);
                  declare
                     T : Syntaxic_Analysis.Tree.Tree := Syntaxic_Analysis.G;
                  begin
                     Semantic_Analysis.AST_Analyse (T);
                     Asm_Generation.Generate_Asm (Syntaxic_Analysis.Tree.First_Child (T.Root));
                  end;
               end;
            end loop;
         end;

         Asm_Generation.Add_Start (Start_Filename => Start_File);

         Asm_Generation.Close_File;
      end;
      Ada.Text_IO.Put_Line ("Compilation succeed");
   end if;

exception
   when Error_Log.Compilation_Error =>
      Asm_Generation.Close_File;
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Compilation failed");
end main;
