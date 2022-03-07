------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2022, AdaCore                      --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Command_Lines.Common;        use Utils.Command_Lines.Common;
package Pp.Command_Lines is

   package Freeze_Common is new Freeze_Descriptor (Common_Descriptor);

   Descriptor : aliased Command_Line_Descriptor :=
     Copy_Descriptor (Common_Descriptor);

   use Common_Flag_Switches;

   package Pp_Disable is new Disable_Switches
     (Descriptor, [1 => To_All (Rep_Clauses)]);

   type Pp_Flags is
     (Obsolete_A0,
      Obsolete_A1,
      Obsolete_A2,
      Obsolete_A3,
      Obsolete_A4,
      Obsolete_A5,
      Comments_Only,
      Failure_Message,
      Pipe,
      Replace_Backup,
      Replace_Force_Backup,
      Replace,
      Separate_Loop,
      Separate_Then,
      Separate_Loop_Then,
      No_Separate_Loop,
      No_Separate_Then,
      No_Separate_Loop_Then,
      No_End_Labels, -- Not documented
      Xml_Help, -- Not documented
      L1, L2, L3, -- Not documented
      No_Tab, -- Not documented
      Separate_Label, -- Not documented
      Separate_Stmt_Name, -- Not documented
      Test, -- Not documented
      Warnings); -- Not documented

   package Pp_Flag_Switches is new Flag_Switches
     (Descriptor,
      Pp_Flags);

   package Pp_Flag_Shorthands is new Pp_Flag_Switches.Set_Shorthands
     ([Obsolete_A0 => +"-A0",
       Obsolete_A1 => +"-A1",
       Obsolete_A2 => +"-A2",
       Obsolete_A3 => +"-A3",
       Obsolete_A4 => +"-A4",
       Obsolete_A5 => +"-A5",
       Comments_Only => null,
       Failure_Message => null,
       Pipe => +"-pipe",
       Replace_Backup => +"-r",
       Replace_Force_Backup => +"-rf",
       Replace => +"-rnb",
       Separate_Loop => null,
       Separate_Then => null,
       Separate_Loop_Then => null,
       No_Separate_Loop => null,
       No_Separate_Then => null,
       No_Separate_Loop_Then => null,
       No_End_Labels => +"-e", -- Not documented
       Xml_Help => +"-hx", -- Not documented
       L1 => +"-l1", L2 => +"-l2", L3 => +"-l3", -- Not documented
       No_Tab => +"-notab", -- Not documented
       Separate_Label => null, -- Not documented
       Separate_Stmt_Name => null, -- Not documented
       Test => null, -- Not documented
       Warnings => +"-w"]); -- Not documented

   package Pp_Flag_Shorthands_N is new Pp_Flag_Switches.Set_Shorthands
     ([No_Tab => +"-N", -- Not documented
       others => null]);

   type Attribute_Casing is
     (Attribute_Mixed_Case,
      Attribute_Lower_Case,
      Attribute_Upper_Case);

   package Attribute_Casing_Switches is new Enum_Switches
     (Descriptor,
      Attribute_Casing);

   package Attribute_Casing_Shorthands is
      new Attribute_Casing_Switches.Set_Shorthands
     ([Attribute_Lower_Case => +"-aL",
       Attribute_Upper_Case => +"-aU",
       Attribute_Mixed_Case => +"-aM"]);

   type Keyword_Casing is
     (Keyword_Lower_Case,
      Keyword_Upper_Case);

   package Keyword_Casing_Switches is new Enum_Switches
     (Descriptor,
      Keyword_Casing);

   package Keyword_Casing_Shorthands is
      new Keyword_Casing_Switches.Set_Shorthands
     ([Keyword_Lower_Case => +"-kL",
       Keyword_Upper_Case => +"-kU"]);

   type Name_Casing is
     (Name_Case_As_Declared,
      Name_Lower_Case,
      Name_Upper_Case,
      Name_Mixed_Case);

   package Name_Casing_Switches is new Enum_Switches
     (Descriptor,
      Name_Casing);

   package Name_Casing_Shorthands is
      new Name_Casing_Switches.Set_Shorthands
     ([Name_Case_As_Declared => +"-nD",
       Name_Lower_Case => +"-nL",
       Name_Upper_Case => +"-nU",
       Name_Mixed_Case => +"-nM"]);

   type Enum_Casing is
     (Enum_Case_As_Declared,
      Enum_Lower_Case,
      Enum_Upper_Case,
      Enum_Mixed_Case);

   package Enum_Casing_Switches is new Enum_Switches
     (Descriptor,
      Enum_Casing);

   package Enum_Casing_Shorthands is
      new Enum_Casing_Switches.Set_Shorthands
     ([Enum_Case_As_Declared => +"-neD",
       Enum_Lower_Case => +"-neL",
       Enum_Upper_Case => +"-neU",
       Enum_Mixed_Case => +"-neM"]);

   type Type_Casing is
     (Type_Case_As_Declared,
      Type_Lower_Case,
      Type_Upper_Case,
      Type_Mixed_Case);

   package Type_Casing_Switches is new Enum_Switches
     (Descriptor,
      Type_Casing);

   package Type_Casing_Shorthands is
      new Type_Casing_Switches.Set_Shorthands
     ([Type_Case_As_Declared => +"-ntD",
       Type_Lower_Case => +"-ntL",
       Type_Upper_Case => +"-ntU",
       Type_Mixed_Case => +"-ntM"]);

   type Number_Casing is
     (Number_Case_As_Constant,
      Number_Case_As_Declared,
      Number_Lower_Case,
      Number_Upper_Case,
      Number_Mixed_Case);

   package Number_Casing_Switches is new Enum_Switches
     (Descriptor,
      Number_Casing);

   package Number_Casing_Shorthands is
      new Number_Casing_Switches.Set_Shorthands
     ([Number_Case_As_Constant => +"-nnC",
       Number_Case_As_Declared => +"-nnD",
       Number_Lower_Case => +"-nnL",
       Number_Upper_Case => +"-nnU",
       Number_Mixed_Case => +"-nnM"]);

   type Pragma_Casing is
     (Pragma_Mixed_Case,
      Pragma_Lower_Case,
      Pragma_Upper_Case);

   package Pragma_Casing_Switches is new Enum_Switches
     (Descriptor,
      Pragma_Casing);

   package Pragma_Casing_Shorthands is
      new Pragma_Casing_Switches.Set_Shorthands
     ([Pragma_Lower_Case => +"-pL",
       Pragma_Upper_Case => +"-pU",
       Pragma_Mixed_Case => +"-pM"]);

   type Constant_Casing is
     (Constant_Case_As_Non_Constant,
      Constant_Case_As_Declared,
      Constant_Mixed_Case,
      Constant_Lower_Case,
      Constant_Upper_Case);

   package Constant_Casing_Switches is new Enum_Switches
     (Descriptor,
      Constant_Casing);

   package Constant_Casing_Shorthands is
      new Constant_Casing_Switches.Set_Shorthands
       ([Constant_Case_As_Non_Constant => +"-cN",
         Constant_Case_As_Declared => +"-cD",
         Constant_Lower_Case => +"-cL",
         Constant_Upper_Case => +"-cU",
         Constant_Mixed_Case => +"-cM"]);

   type Pp_Booleans is
     (Alignment,
      Align_Modes,
      Comments_Unchanged,
      Comments_Gnat_Indentation,
      Comments_Standard_Indentation, -- Not documented
      Comments_Gnat_Beginning,
      Comments_Fill,
      Comments_Special,
      End_Id,
      Separate_Is,
      Separate_Return,
      Use_On_New_Line,
      Split_Line_Before_Op,
      Split_Line_Before_Record,
      Indent_Named_Statements,
      RM_Style_Spacing,
      Insert_Blank_Lines,
      Preserve_Blank_Lines,
      Source_Line_Breaks,
      Spaces_Only,
      Preserve_Line_Breaks,
      Ff_After_Pragma_Page,
      Compact,
      Vertical_Enum_Types,
      Vertical_Array_Types,
      Vertical_Named_Aggregates,
      Vertical_Case_Alternatives);
   --  --source-line-breaks means to keep the line breaks from the source;
   --  do not insert or delete any line breaks. --spaces-only means to insert
   --  and delete spaces as appropriate, but not make any other transformations
   --  (such as changing "end;" to "end X;").

   package Pp_Boolean_Switches is new Boolean_Switches
     (Descriptor,
      Pp_Booleans);

   package Pp_Boolean_Shorthands is new Pp_Boolean_Switches.Set_Shorthands
     ([Alignment => null,
       Align_Modes => null,
       Comments_Unchanged => +"-c0",
       Comments_Gnat_Indentation => +"-c1",
       Comments_Standard_Indentation => +"-c2", -- Not documented
       Comments_Gnat_Beginning => +"-c3",
       Comments_Fill => +"-c4",
       Comments_Special => +"-c5",
       End_Id => null,
       Separate_Is => null,
       Separate_Return => null,
       Use_On_New_Line => null,
       Split_Line_Before_Op => null,
       Split_Line_Before_Record => null,
       Indent_Named_Statements => null,
       RM_Style_Spacing => +"--RM-style-spacing",
       Insert_Blank_Lines => null,
       Preserve_Blank_Lines => null,
       Source_Line_Breaks => null,
       Spaces_Only => null,
       Preserve_Line_Breaks => null,
       Ff_After_Pragma_Page => +"-ff",
       Compact => null,
       Vertical_Enum_Types => null,
       Vertical_Array_Types => null,
       Vertical_Named_Aggregates => null,
       Vertical_Case_Alternatives => null]);

   package Pp_Boolean_Defaults is new
     Pp_Boolean_Switches.Set_Defaults
       ([Alignment => True,
         Align_Modes => True,
         Comments_Unchanged => False,
         Comments_Gnat_Indentation => False,
         Comments_Standard_Indentation => False,
         Comments_Gnat_Beginning => False,
         Comments_Fill => False,
         Comments_Special => False,
         End_Id => True,
         Separate_Is => True,
         Separate_Return => True,
         Use_On_New_Line => False,
         Split_Line_Before_Op => False,
         Split_Line_Before_Record => False,
         Indent_Named_Statements => False,
         RM_Style_Spacing => False,
         Insert_Blank_Lines => False,
         Preserve_Blank_Lines => False,
         Source_Line_Breaks => False,
         Spaces_Only => False,
         Preserve_Line_Breaks => False,
         Ff_After_Pragma_Page => False,
         Compact => True,
         Vertical_Enum_Types => False,
         Vertical_Array_Types => False,
         Vertical_Named_Aggregates => False,
         Vertical_Case_Alternatives => False]);

   type Pp_Strings is
     (File_Name_File, -- Not documented
      Output,
      Output_Force,
      End_Of_Line,
      Pp_Off,
      Pp_On,
      Templates);

   package Pp_String_Switches is new String_Switches
     (Descriptor,
      Pp_Strings);

   package Pp_String_Syntax is new Pp_String_Switches.Set_Syntax
     ([File_Name_File => '=',
       Output => '=',
       Output_Force => '=',
       End_Of_Line => '=',
       Pp_Off => '=',
       Pp_On => '=',
       Templates => '=']);

   package Pp_String_Shorthands is new Pp_String_Switches
     .Set_Shorthands
     ([File_Name_File => null,
       Output => +"-o",
       Output_Force => +"-of",
       End_Of_Line => +"--eol",
       Pp_Off => null,
       Pp_On => null,
       Templates => null]);

   type Pp_String_Seqs is (Dictionary);

   package Pp_String_Seq_Switches is new String_Seq_Switches
     (Descriptor,
      Pp_String_Seqs);

   package Pp_String_Seq_Syntax is new Pp_String_Seq_Switches
     .Set_Syntax
     ([Dictionary => ':']);

   package Pp_String_Seq_Shorthands is new Pp_String_Seq_Switches
     .Set_Shorthands
     ([Dictionary => +"-D"]);

   type Pp_Nats is
     (Max_Line_Length,
      Indentation,
      Indent_Continuation,
      Initial_Indentation, -- Not documented; used by gnatstub
      Decimal_Grouping,
      Based_Grouping,
      Call_Threshold,
      Par_Threshold,
      Case_Threshold); -- Not documented

   package Pp_Nat_Switches is new Other_Switches
     (Descriptor,
      Pp_Nats,
      Natural,
      Natural'Image,
      Natural'Value);

   package Pp_Nat_Syntax is new Pp_Nat_Switches.Set_Syntax
     ([Max_Line_Length => '!',
       Indentation => '!',
       Indent_Continuation => '!',
       Initial_Indentation => '=',
       Decimal_Grouping => '=',
       Based_Grouping => '=',
       Call_Threshold => '=',
       Par_Threshold => '=',
       Case_Threshold => '!']); -- Not documented

   package Pp_Nat_Defaults is new Pp_Nat_Switches.Set_Defaults
     ([Max_Line_Length => 79,
       Indentation => 3,
       Indent_Continuation => 0,
   --  Default for Indent_Continuation is one less than Indentation, or 1.
       Initial_Indentation => 0,
       Decimal_Grouping => 3,
       Based_Grouping => 4,
       Call_Threshold => Natural'Last,
       Par_Threshold => Natural'Last,
       Case_Threshold => 10]); -- Not documented

   package Pp_Nat_Shorthands is new Pp_Nat_Switches.Set_Shorthands
     ([Max_Line_Length => +"-M",
       Indentation => +"-i",
       Indent_Continuation => +"-cl",
       Initial_Indentation => null,
       Decimal_Grouping => null,
       Based_Grouping => null,
       Call_Threshold => null,
       Par_Threshold => null,
       Case_Threshold => +"-T"]); -- Not documented

   package Freeze is new Freeze_Descriptor (Descriptor);

   use Pp_Flag_Switches,
     Pp_Boolean_Switches,
     Attribute_Casing_Switches,
     Keyword_Casing_Switches,
     Name_Casing_Switches,
     Enum_Casing_Switches,
     Type_Casing_Switches,
     Number_Casing_Switches,
     Pragma_Casing_Switches,
     Constant_Casing_Switches,
     Pp_Nat_Switches;

   function Obsolete_Alignment_Switch (Cmd : Command_Line) return Boolean is
              (Arg (Cmd, Obsolete_A1)
       or else Arg (Cmd, Obsolete_A2)
       or else Arg (Cmd, Obsolete_A3)
       or else Arg (Cmd, Obsolete_A4)
       or else Arg (Cmd, Obsolete_A5));

   function Alignment_Enabled (Cmd : Command_Line) return Boolean is
     (if Arg (Cmd, RM_Style_Spacing) then Explicit (Cmd, Alignment)
      elsif Arg (Cmd, Obsolete_A0) then Obsolete_Alignment_Switch (Cmd)
      else Arg (Cmd, Alignment) or Obsolete_Alignment_Switch (Cmd));
   --  The old gnatpp had the ability to individually enable different kinds of
   --  alignment with -A1, -A2, -A3, -A4, and -A5. In addition, -A0 turned off
   --  all 5 of those. The new gnatpp does not support that complexity, because
   --  we believe users either like alignment for all cases, or don't like it.
   --  For compatibility, we recognize the obsolete switches, and turn on
   --  alignment if any of -A1..5 are specified (regardless of -A0). The -A0
   --  switch turns off alignment if no other -A switches are given.
   --
   --  Alignment is on by default, except when --rm-style-spacing is given; in
   --  that case, only an explicit --alignment turns on alignment. The ability
   --  to have both alignment and RM-style spacing is newer, so we don't need
   --  the horsing around with -A switches in that case.

   function Comment_Filling_Enabled (Cmd : Command_Line) return Boolean is
     (not Arg (Cmd, Comments_Unchanged) and Arg (Cmd, Comments_Fill));

   function Insert_Blank_Lines (Cmd : Command_Line) return Boolean is
     (Arg (Cmd, Insert_Blank_Lines) and not Arg (Cmd, Preserve_Blank_Lines));

   function Preserve_Blank_Lines (Cmd : Command_Line) return Boolean is
     (not Arg (Cmd, Insert_Blank_Lines) and Arg (Cmd, Preserve_Blank_Lines));

   type PP_Casing is
     --  Defines the casing of identifiers and keywords generated by gnatpp
     (Lower_Case,
      --  All letters are lowercase
      Upper_Case,
      --  All letters are uppercase
      Mixed,
      --  For both defining and usage occurrences of identifiers, the first
      --  letter and each letter which immediately follows the underscore are
      --  uppercase, and all the other letters are lowercase.
      As_Declared
      --  All the usage occurrences of identifiers have the same casing as
      --  defining occurrences, defining occurrences have the same casing as
      --  source code.
     );

   subtype Lower_Upper_PP_Casing is PP_Casing with
     Predicate => Lower_Upper_PP_Casing in Lower_Case | Upper_Case;

   subtype Lower_Upper_Mixed_PP_Casing is PP_Casing with
     Predicate => Lower_Upper_Mixed_PP_Casing
       in Lower_Case | Upper_Case | Mixed;

   subtype Cmd_Line is Command_Line;

   function PP_Attribute_Casing
     (Cmd : Cmd_Line) return Lower_Upper_Mixed_PP_Casing is
      (case Attribute_Casing'(Arg (Cmd)) is
         when Attribute_Mixed_Case => Mixed,
         when Attribute_Lower_Case => Lower_Case,
         when Attribute_Upper_Case => Upper_Case);

   function PP_Keyword_Casing
     (Cmd : Cmd_Line) return Lower_Upper_PP_Casing is
      (case Keyword_Casing'(Arg (Cmd)) is
         when Keyword_Lower_Case => Lower_Case,
         when Keyword_Upper_Case => Upper_Case);

   function PP_Pragma_Casing
     (Cmd : Cmd_Line) return Lower_Upper_Mixed_PP_Casing is
      (case Pragma_Casing'(Arg (Cmd)) is
         when Pragma_Mixed_Case => Mixed,
         when Pragma_Lower_Case => Lower_Case,
         when Pragma_Upper_Case => Upper_Case);
   --  Specifies the casing of pragma names and identifiers specific to
   --  pragmas

   function PP_Name_Casing
     (Cmd : Cmd_Line) return PP_Casing is
      (case Name_Casing'(Arg (Cmd)) is
         when Name_Case_As_Declared => As_Declared,
         when Name_Mixed_Case => Mixed,
         when Name_Lower_Case => Lower_Case,
         when Name_Upper_Case => Upper_Case);
   --  Defines the casing for both defining and usage occurrences of the
   --  names

   function PP_Enum_Casing
     (Cmd : Cmd_Line) return PP_Casing is
      (case Enum_Casing'(Arg (Cmd)) is
         when Enum_Case_As_Declared => PP_Name_Casing (Cmd),
         when Enum_Mixed_Case => Mixed,
         when Enum_Lower_Case => Lower_Case,
         when Enum_Upper_Case => Upper_Case);
   --  Defines the casing for both defining and usage occurrences of the
   --  enumeration literals.

   function PP_Type_Casing
     (Cmd : Cmd_Line) return PP_Casing is
      (case Type_Casing'(Arg (Cmd)) is
         when Type_Case_As_Declared => PP_Name_Casing (Cmd),
         when Type_Mixed_Case => Mixed,
         when Type_Lower_Case => Lower_Case,
         when Type_Upper_Case => Upper_Case);
   --  Defines the casing for both defining and usage occurrences of the
   --  type (and subtype???) names.

   function PP_Constant_Casing
     (Cmd : Cmd_Line) return PP_Casing is
      (case Constant_Casing'(Arg (Cmd)) is
         when Constant_Case_As_Non_Constant => PP_Name_Casing (Cmd),
         when Constant_Case_As_Declared => As_Declared,
         when Constant_Mixed_Case => Mixed,
         when Constant_Lower_Case => Lower_Case,
         when Constant_Upper_Case => Upper_Case);
   --  Defines the casing for both defining and usage occurrences of constant
   --  object declarations.

   function PP_Number_Casing
     (Cmd : Cmd_Line) return PP_Casing is
      (case Number_Casing'(Arg (Cmd)) is
         when Number_Case_As_Constant => PP_Constant_Casing (Cmd),
         when Number_Case_As_Declared => As_Declared,
         when Number_Mixed_Case => Mixed,
         when Number_Lower_Case => Lower_Case,
         when Number_Upper_Case => Upper_Case);
   --  Defines the casing for both defining and usage occurrences of the
   --  named numbers names.

   function PP_Indentation (Cmd : Cmd_Line) return Positive is
     (Arg (Cmd, Indentation));

   function PP_Indent_Continuation (Cmd : Cmd_Line) return Positive is
     (if Arg (Cmd, Indent_Continuation) = 0
        then (if PP_Indentation (Cmd) > 1
                then PP_Indentation (Cmd) - 1
                else PP_Indentation (Cmd))
        else Arg (Cmd, Indent_Continuation));

end Pp.Command_Lines;
