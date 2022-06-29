with Utils.Command_Lines;        use Utils.Command_Lines;
with Utils.Command_Lines.Common; use Utils.Command_Lines.Common;
package Pp.Command_Lines is

   package Freeze_Common is new Freeze_Descriptor (Common_Descriptor);

   Descriptor : aliased Command_Line_Descriptor :=
     Copy_Descriptor (Common_Descriptor);

   pragma Warnings (Off);
   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;
   pragma Warnings (On);

   package Pp_Disable is new Disable_Switches (Descriptor,
      (1 => To_All (Rep_Clauses)));

   type Pp_Flags is
     (No_Alignment,
      Alignment,
      Obsolete_A2,
      Obsolete_A3,
      Obsolete_A4,
      Obsolete_A5,
      Comments_Unchanged,
      Comments_Gnat_Indentation,
      Comments_Standard_Indentation, -- Not documented

      Comments_Gnat_Beginning,
      Comments_Fill,
      Comments_Special,
      Comments_Only,
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

      L1,
      L2,
      L3, -- Not documented

      No_Tab, -- Not documented

      Separate_Label, -- Not documented

      Separate_Stmt_Name, -- Not documented

      Test, -- Not documented

      Warnings); -- Not documented

   package Pp_Flag_Switches is new Flag_Switches (Descriptor, Pp_Flags);

   package Pp_Flag_Shorthands is new Pp_Flag_Switches.Set_Shorthands
     (
      (No_Alignment                  => +"-A0",
       Alignment                     => +"-A1",
       Obsolete_A2                   => +"-A2",
       Obsolete_A3                   => +"-A3",
       Obsolete_A4                   => +"-A4",
       Obsolete_A5                   => +"-A5",
       Comments_Unchanged            => +"-c0",
       Comments_Gnat_Indentation     => +"-c1",
       Comments_Standard_Indentation => +"-c2", -- Not documented

       Comments_Gnat_Beginning => +"-c3",
       Comments_Fill           => +"-c4",
       Comments_Special        => +"-c5",
       Comments_Only           => null,
       Pipe                    => +"-pipe",
       Replace_Backup          => +"-r",
       Replace_Force_Backup    => +"-rf",
       Replace                 => +"-rnb",
       Separate_Loop           => null,
       Separate_Then           => null,
       Separate_Loop_Then      => null,
       No_Separate_Loop        => null,
       No_Separate_Then        => null,
       No_Separate_Loop_Then   => null,
       No_End_Labels           => +"-e", -- Not documented

       Xml_Help => +"-hx", -- Not documented

       L1 => +"-l1",
       L2 => +"-l2",
       L3 => +"-l3", -- Not documented

       No_Tab => +"-notab", -- Not documented

       Separate_Label => null, -- Not documented

       Separate_Stmt_Name => null, -- Not documented

       Test => null, -- Not documented

       Warnings => +"-w")); -- Not documented

   package Pp_Flag_Shorthands_N is new Pp_Flag_Switches.Set_Shorthands
     (
      (No_Tab => +"-N", -- Not documented

       others => null));

   type Attribute_Casing is
     (Attribute_Mixed_Case, Attribute_Lower_Case, Attribute_Upper_Case);

   package Attribute_Casing_Switches is new Enum_Switches (Descriptor,
      Attribute_Casing);

   package Attribute_Casing_Shorthands is new Attribute_Casing_Switches
     .Set_Shorthands
     ((Attribute_Lower_Case => +"-aL", Attribute_Upper_Case => +"-aU",
       Attribute_Mixed_Case => +"-aM"));

   type Keyword_Casing is (Keyword_Lower_Case, Keyword_Upper_Case);

   package Keyword_Casing_Switches is new Enum_Switches (Descriptor,
      Keyword_Casing);

   package Keyword_Casing_Shorthands is new Keyword_Casing_Switches
     .Set_Shorthands
     ((Keyword_Lower_Case => +"-kL", Keyword_Upper_Case => +"-kU"));

   type Name_Casing is
     (Name_Case_As_Declared, Name_Lower_Case, Name_Upper_Case,
      Name_Mixed_Case);

   package Name_Casing_Switches is new Enum_Switches (Descriptor, Name_Casing);

   package Name_Casing_Shorthands is new Name_Casing_Switches.Set_Shorthands
     ((Name_Case_As_Declared => +"-nD", Name_Lower_Case => +"-nL",
       Name_Upper_Case       => +"-nU", Name_Mixed_Case => +"-nM"));

   type Enum_Casing is
     (Enum_Case_As_Declared, Enum_Lower_Case, Enum_Upper_Case,
      Enum_Mixed_Case);

   package Enum_Casing_Switches is new Enum_Switches (Descriptor, Enum_Casing);

   package Enum_Casing_Shorthands is new Enum_Casing_Switches.Set_Shorthands
     ((Enum_Case_As_Declared => +"-neD", Enum_Lower_Case => +"-neL",
       Enum_Upper_Case       => +"-neU", Enum_Mixed_Case => +"-neM"));

   type Type_Casing is
     (Type_Case_As_Declared, Type_Lower_Case, Type_Upper_Case,
      Type_Mixed_Case);

   package Type_Casing_Switches is new Enum_Switches (Descriptor, Type_Casing);

   package Type_Casing_Shorthands is new Type_Casing_Switches.Set_Shorthands
     ((Type_Case_As_Declared => +"-ntD", Type_Lower_Case => +"-ntL",
       Type_Upper_Case       => +"-ntU", Type_Mixed_Case => +"-ntM"));

   type Number_Casing is
     (Number_Case_As_Declared, Number_Lower_Case, Number_Upper_Case,
      Number_Mixed_Case);

   package Number_Casing_Switches is new Enum_Switches (Descriptor,
      Number_Casing);

   package Number_Casing_Shorthands is new Number_Casing_Switches
     .Set_Shorthands
     ((Number_Case_As_Declared => +"-nnD", Number_Lower_Case => +"-nnL",
       Number_Upper_Case       => +"-nnU", Number_Mixed_Case => +"-nnM"));

   type Pragma_Casing is
     (Pragma_Mixed_Case, Pragma_Lower_Case, Pragma_Upper_Case);

   package Pragma_Casing_Switches is new Enum_Switches (Descriptor,
      Pragma_Casing);

   package Pragma_Casing_Shorthands is new Pragma_Casing_Switches
     .Set_Shorthands
     ((Pragma_Lower_Case => +"-pL", Pragma_Upper_Case => +"-pU",
       Pragma_Mixed_Case => +"-pM"));

   type Pp_Booleans is
     (Align_Modes, End_Id, Separate_Is, Use_On_New_Line, Split_Line_Before_Op,
      Split_Line_Before_Record, Indent_Named_Statements, Rm_Style_Spacing,
      Insert_Blank_Lines, Preserve_Blank_Lines, Source_Line_Breaks,
      Spaces_Only, Preserve_Line_Breaks, Ff_After_Pragma_Page,
      Vertical_Enum_Types, Vertical_Array_Types, Vertical_Named_Aggregates,
      Vertical_Case_Alternatives);
   --  ???Source_Line_Breaks and Spaces_Only are currently undocumented.
   --  --source-line-breaks means to keep the line breaks from the source;
   --  do not insert or delete any line breaks. --spaces-only means to insert
   --  and delete spaces as appropriate, but not make any other transformations
   --  (such as changing "end;" to "end X;"). Neither of these work yet.

   package Pp_Boolean_Switches is new Boolean_Switches (Descriptor,
      Pp_Booleans);

   package Pp_Boolean_Shorthands is new Pp_Boolean_Switches.Set_Shorthands
     ((Align_Modes                => null, End_Id => null, Separate_Is => null,
       Use_On_New_Line            => null, Split_Line_Before_Op => null,
       Split_Line_Before_Record   => null, Indent_Named_Statements => null,
       Rm_Style_Spacing => +"--RM-style-spacing", Insert_Blank_Lines => null,
       Preserve_Blank_Lines       => null, Source_Line_Breaks => null,
       Spaces_Only                => null, Preserve_Line_Breaks => null,
       Ff_After_Pragma_Page       => +"-ff", Vertical_Enum_Types => null,
       Vertical_Array_Types       => null, Vertical_Named_Aggregates => null,
       Vertical_Case_Alternatives => null));

   package Pp_Boolean_Defaults is new Pp_Boolean_Switches.Set_Defaults
     (
      (Align_Modes              => True,
       End_Id                   => True,
       Separate_Is              => True,
       Use_On_New_Line          => False,
       Split_Line_Before_Op     => False,
       Split_Line_Before_Record => False,
       Indent_Named_Statements  => False,
       Rm_Style_Spacing         => False,
       Insert_Blank_Lines       => False,
       Preserve_Blank_Lines     => False,
       Source_Line_Breaks       => False, -- ????????????????Should be False

       Spaces_Only          => False,
       Preserve_Line_Breaks => False, -- ????????????????Should be False

       Ff_After_Pragma_Page       => False, Vertical_Enum_Types => False,
       Vertical_Array_Types       => False, Vertical_Named_Aggregates => False,
       Vertical_Case_Alternatives => False));

   type Pp_Strings is
     (File_Name_File, -- Not documented

      Output, Output_Force, End_Of_Line, Pp_Off, Pp_On);

   package Pp_String_Switches is new String_Switches (Descriptor, Pp_Strings);

   package Pp_String_Syntax is new Pp_String_Switches.Set_Syntax
     ((File_Name_File => '=', Output => '=', Output_Force => '=',
       End_Of_Line    => '=', Pp_Off => '=', Pp_On => '='));

   package Pp_String_Shorthands is new Pp_String_Switches.Set_Shorthands
     ((File_Name_File => null, Output => +"-o", Output_Force => +"-of",
       End_Of_Line    => +"--eol", Pp_Off => null, Pp_On => null));

   type Pp_String_Seqs is (Dictionary);

   package Pp_String_Seq_Switches is new String_Seq_Switches (Descriptor,
      Pp_String_Seqs);

   package Pp_String_Seq_Syntax is new Pp_String_Seq_Switches.Set_Syntax
     ((Dictionary => ':'));

   package Pp_String_Seq_Shorthands is new Pp_String_Seq_Switches
     .Set_Shorthands
     ((Dictionary => +"-D"));

   type Pp_Nats is
     (Max_Line_Length,
      Indentation,
      Indent_Continuation,
      Initial_Indentation, -- Not documented; used by gnatstub

      Decimal_Grouping, Based_Grouping, Call_Threshold, Par_Threshold,
      Case_Threshold); -- Not documented

   package Pp_Nat_Switches is new Other_Switches (Descriptor, Pp_Nats, Natural,
      Natural'Image, Natural'Value);

   package Pp_Nat_Syntax is new Pp_Nat_Switches.Set_Syntax
     ((Max_Line_Length => '!', Indentation => '!', Indent_Continuation => '!',
       Initial_Indentation => '=', Decimal_Grouping => '=',
       Based_Grouping      => '=', Call_Threshold => '=', Par_Threshold => '=',
       Case_Threshold      => '!')); -- Not documented

   package Pp_Nat_Defaults is new Pp_Nat_Switches.Set_Defaults
     ((Max_Line_Length => 79, Indentation => 3, Indent_Continuation => 0,
       --  Default for Indent_Continuation is one less than Indentation, or 1.

       Initial_Indentation => 0, Decimal_Grouping => 0, Based_Grouping => 0,
       Call_Threshold      => Natural'Last, Par_Threshold => Natural'Last,
       Case_Threshold      => 10)); -- Not documented

   package Pp_Nat_Shorthands is new Pp_Nat_Switches.Set_Shorthands
     ((Max_Line_Length     => +"-M", Indentation => +"-i",
       Indent_Continuation => +"-cl", Initial_Indentation => null,
       Decimal_Grouping    => null, Based_Grouping => null,
       Call_Threshold      => null, Par_Threshold => null,
       Case_Threshold      => +"-T")); -- Not documented

   package Freeze is new Freeze_Descriptor (Descriptor);

   pragma Warnings (Off);
   use Pp_Flag_Switches, Pp_Boolean_Switches, Attribute_Casing_Switches,
     Keyword_Casing_Switches, Name_Casing_Switches, Enum_Casing_Switches,
     Type_Casing_Switches, Number_Casing_Switches, Pragma_Casing_Switches,
     Pp_String_Switches, Pp_Nat_Switches, Pp_String_Seq_Switches;
   pragma Warnings (On);

   function Alignment_Enabled (Cmd : Command_Line) return Boolean is
     ((not Arg (Cmd, Rm_Style_Spacing))
      and then
      (not Arg (Cmd, No_Alignment) or else Arg (Cmd, Obsolete_A2)
       or else Arg (Cmd, Obsolete_A3) or else Arg (Cmd, Obsolete_A4)
       or else Arg (Cmd, Obsolete_A5)));
   --  The old gnatpp had the ability to individually enable different kinds of
   --  alignment; the new gnatpp does not. Instead, we align if ANY alignment
   --  option is enabled; if all alignment is turned off, we don't align.

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
      --  For both defining and usage occurrences of identifiers The first letter
      --  and each letter which immediately follows the underscore are uppercase,
      --  and all the other letters are lowercase
      As_Declared);
   --  All the usage occurrences of identifiers have the same casing as
   --  defining occurrences, defining occurrences have the same casing as
   --  the corresponding defining occurrences in the argument source.

   subtype Lower_Upper_PP_Casing is PP_Casing with
       Predicate => Lower_Upper_PP_Casing in Lower_Case | Upper_Case;

   subtype Lower_Upper_Mixed_PP_Casing is PP_Casing with
       Predicate => Lower_Upper_Mixed_PP_Casing in Lower_Case | Upper_Case |
            Mixed;

   subtype Cmd_Line is Command_Line;

   function PP_Attribute_Casing
     (Cmd : Cmd_Line) return Lower_Upper_Mixed_PP_Casing is
     (case Attribute_Casing'(Arg (Cmd)) is when Attribute_Mixed_Case => Mixed,
        when Attribute_Lower_Case => Lower_Case,
        when Attribute_Upper_Case => Upper_Case);

   function PP_Keyword_Casing (Cmd : Cmd_Line) return Lower_Upper_PP_Casing is
     (case Keyword_Casing'(Arg (Cmd)) is when Keyword_Lower_Case => Lower_Case,
        when Keyword_Upper_Case => Upper_Case);

   function PP_Pragma_Casing
     (Cmd : Cmd_Line) return Lower_Upper_Mixed_PP_Casing is
     (case Pragma_Casing'(Arg (Cmd)) is when Pragma_Mixed_Case => Mixed,
        when Pragma_Lower_Case                                 => Lower_Case,
        when Pragma_Upper_Case                                 => Upper_Case);
   --  Specifies the casing of pragma names and identifiers specific to
   --  pragmas

   function PP_Name_Casing (Cmd : Cmd_Line) return PP_Casing is
     (case Name_Casing'(Arg (Cmd)) is
        when Name_Case_As_Declared => As_Declared,
        when Name_Mixed_Case => Mixed, when Name_Lower_Case => Lower_Case,
        when Name_Upper_Case       => Upper_Case);
   --  Defines the casing for both defining and usage occurrences of the
   --  names

   function PP_Enum_Casing (Cmd : Cmd_Line) return PP_Casing is
     (case Enum_Casing'(Arg (Cmd)) is
        when Enum_Case_As_Declared => PP_Name_Casing (Cmd),
        when Enum_Mixed_Case => Mixed, when Enum_Lower_Case => Lower_Case,
        when Enum_Upper_Case       => Upper_Case);
   --  Defines the casing for both defining and usage occurrences of the
   --  enumeration literals.

   function PP_Type_Casing (Cmd : Cmd_Line) return PP_Casing is
     (case Type_Casing'(Arg (Cmd)) is
        when Type_Case_As_Declared => PP_Name_Casing (Cmd),
        when Type_Mixed_Case => Mixed, when Type_Lower_Case => Lower_Case,
        when Type_Upper_Case       => Upper_Case);
   --  Defines the casing for both defining and usage occurrences of the
   --  type (and subtype???) names.

   function PP_Number_Casing (Cmd : Cmd_Line) return PP_Casing is
     (case Number_Casing'(Arg (Cmd)) is
        when Number_Case_As_Declared => PP_Name_Casing (Cmd),
        when Number_Mixed_Case => Mixed, when Number_Lower_Case => Lower_Case,
        when Number_Upper_Case       => Upper_Case);
   --  Defines the casing for both defining and usage occurrences of the
   --  named numbers names.

   function PP_Indentation (Cmd : Cmd_Line) return Positive is
     (Arg (Cmd, Indentation));

   function PP_Indent_Continuation (Cmd : Cmd_Line) return Positive is
     (if Arg (Cmd, Indent_Continuation) = 0 then
        (if PP_Indentation (Cmd) > 1 then PP_Indentation (Cmd) - 1
         else PP_Indentation (Cmd))
      else Arg (Cmd, Indent_Continuation));

end Pp.Command_Lines;
