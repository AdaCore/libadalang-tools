-------------------------------------------------------------------------------
-- Procedure Normalize                                                       --
-- (C) Copyright 1997 ADALOG                                                 --
-- Author: J-P. Rosen                                                        --
--                                                                           --
-- Normalizes an Ada source file to allow "semantic" comparison of files     --
--    All unnecessary separators are removed                                 --
--    Tabs are treated as a single space                                     --
--    Comments are removed                                                   --
--    All identifiers are converted to upper case                            --
--    String and character litterals are left untouched                      --
--    Empty lines (after processing) are removed                             --
-- Usage:                                                                    --
--    normalize [<file-in> [<file-out>]]                                     --
--    with one argument, outputs to Standard_Output                          --
--    with no argument, inputs from Standard_Input and outputs               --
--    to Standard_Output                                                     --
--    This should allow for easy pipe-lining !                               --
-- Exit_Status:                                                              --
--    0 : OK                                                                 --
--    1 : Error (file not found or syntax error)                             --
-- Known bugs:                                                               --
--    "mod" (A,B) is not recognized as a special construct, and thus         --
--    casing of "mod" will be preserved                                      --
--                                                                           --
-- Rights to use, distribute or modify this package in any way is hereby     --
-- granted, provided this header is kept unchanged in all versions and the   --
-- associated documentation file is distributed unchanged. Additionnal       --
-- headers or documentation may be added.                                    --
-- All modifications must be properly marked as not originating from Adalog. --
-- If you make a valuable addition, please keep us informed by sending a     --
-- message to rosen.adalog@wanadoo.fr                                        --
--                                                                           --
-- ADALOG is providing training, consultancy and expertise in Ada and        --
-- related software engineering techniques. For more info about our services:--
-- ADALOG                   Tel: +33 1 41 24 31 40                           --
-- 19-21 rue du 8 mai 1945  Fax: +33 1 41 24 07 36                           --
-- 94110 ARCUEIL            E-m: info@adalog.fr                              --
-- FRANCE                   URL: http://www.adalog.fr/                       --
--                                                                           --
-- This procedure is distributed in the hope that it will be useful, but     --
-- WITHOUT ANY WARRANTY;  without even the  implied warranty of              --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                      --
-------------------------------------------------------------------------------
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Characters.Handling;
with Ada.Exceptions;
procedure Normalize is
   File_In   : File_Type;
   Buffer_In : String(1..254);  -- 250 required by RM + 4 guard characters
   -- Inspecting up to Buffer_In(Index_In+4) is safe
   Last_In     : Natural;
   Index_In    : Positive;

   File_Out   : File_Type;
   Buffer_Out : String (Buffer_In'range);
   Index_Out  : Natural;

   Separator_Required : Boolean;
   -- True if a separator has to be added if the next element is an
   -- identifier or a numeric litteral

   procedure Add_Buff (Ch : Character) is
   begin
      Index_Out              := Index_Out + 1;
      Buffer_Out (Index_Out) := Ch;
   end Add_Buff;

   use Ada.Characters.Handling;

begin
   case Argument_Count is
      when 0 =>
         null;
      when 1 =>
         Open (File_In,  In_File,  Argument(1));
         Set_Input (File_In);
      when 2 =>
         Open   (File_In,  In_File,  Argument(1));
         Create (File_Out, Out_File, Argument(2));
         Set_Input  (File_In);
         Set_Output (File_Out);
      when others =>
         Put_Line (Standard_Error, "Usage: normalize [<file-in> [<file-out>]]");
         Set_Exit_Status (1);
         return;
   end case;

   while not End_Of_File loop
      Get_Line (Buffer_In, Last_In);
      if Last_In > 250 then  -- Input line too long
         Put (Standard_Error, "Input line too long :");
         Put (Standard_Error, Count'Image (Line (Current_Input)));
         New_Line;
         Set_Exit_Status (1);
         return;
      end if;

      Buffer_In (Last_In + 1 .. Last_In + 4) := "    ";
      Index_In  := 1;
      Index_Out := 0;
      Separator_Required := False;

      while Index_In <= Last_In loop
         case Buffer_In (Index_In) is

            when '-' =>
               if Buffer_In (Index_In + 1) = '-' then
                  -- Start of comment
                  exit;
               else
                  Add_Buff ('-');
                  Index_In := Index_In + 1;
               end if;
               Separator_Required := False;

            when '"' =>
               Add_Buff ('"');
               Index_In := Index_In + 1;
               loop
                  -- Process string litteral
                  if Index_In > Last_In then
                     Put_Line (Standard_Error, "Unclosed string litteral");
                     Set_Exit_Status (1);
                     return;
                  end if;
                  Add_Buff (Buffer_In (Index_In));
                  Index_In := Index_In+1;
                  if Buffer_In (Index_In-1) = '"' then
                     if Buffer_In (Index_In) = '"' then
                        Add_Buff ('"');
                        Index_In := Index_In+1;
                     else
                        exit;
                     end if;
                  end if;
               end loop;
               Separator_Required := False;

            when ''' =>
               if Buffer_In (Index_In + 2) = ''' and then -- Character litteral
                     not (Is_Letter(Buffer_In (Index_In + 3)) and
                     Buffer_In (Index_In + 4) = ''')
                 -- Special kludge for Character'('a'), but beware of 'a'+'b' !
                 -- Note that if the 3rd character is not a letter, we don't care to
                 -- parse it wrongly, since it will be left unchanged.
               then
                  Add_Buff (''');
                  Add_Buff (Buffer_In (Index_In + 1));
                  Add_Buff (''');
                  Index_In := Index_In + 3;
               else
                  Add_Buff (''');
                  Index_In := Index_In + 1;
               end if;
               Separator_Required := False;

            when 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' =>
               if Separator_Required then
                  Add_Buff (' ');
               end if;
               while Is_Alphanumeric (Buffer_In (Index_In)) or
                     Buffer_In (Index_In) = '_'
               loop
                  -- Process identifier (or keyword) or numeric litteral
                  Add_Buff (To_Upper (Buffer_In (Index_In)));
                  Index_In := Index_In + 1;
               end loop;
               Separator_Required := True;

            when ' ' | Ascii.HT =>
               Index_In := Index_In + 1;

            when others =>
               Add_Buff (Buffer_In (Index_In));
               Index_In := Index_In + 1;
               Separator_Required := False;
         end case;
      end loop;

      if Index_Out > 0 then
         Put_Line (Buffer_Out (1..Index_Out));
      end if;
   end loop;

   Set_Exit_Status (0);

exception
   when Name_Error =>
      Put (Standard_Error, "Unable to open input file """);
      Put (Standard_Error, Argument(1));
      Put_Line ("""");
      Set_Exit_Status (1);
   when Occur : others =>
      Put_Line (Standard_Error, "Unexpected exception raised : " &
         Ada.Exceptions.Exception_Name (Occur));
      Set_Exit_Status (1);
end Normalize;
