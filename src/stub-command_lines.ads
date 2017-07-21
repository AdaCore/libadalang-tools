with Utils.Command_Lines; use Utils.Command_Lines;
with Utils.Command_Lines.Common;        use Utils.Command_Lines.Common;
package Stub.Command_Lines is

   package Freeze_Common is new Freeze_Descriptor (Common_Descriptor);

   Descriptor : aliased Command_Line_Descriptor :=
     Copy_Descriptor (Common_Descriptor);

   use Common_Flag_Switches, Common_String_Switches,
     Common_String_Seq_Switches, Common_Nat_Switches;

   package Stub_Disable is new Disable_Switches
     (Descriptor, (To_All (Rep_Clauses), To_All (Compute_Timing)));

   type Stub_Flags is
     (Subunits,
      Force,
      Alphabetical_Order,
      Comment_Header_Sample,
      Comment_Header_Spec,
      Keep_Tree_File, -- ignored
      No_Exception,
      No_Local_Header,
      Reuse_Tree_File, -- error
      Overwrite_Tree_File); -- ignored

   package Stub_Flag_Switches is new Flag_Switches
     (Descriptor,
      Stub_Flags);

   package Stub_Flag_Shorthands is new Stub_Flag_Switches.Set_Shorthands
     ((Subunits => null,
       Force => +"-f",
       Alphabetical_Order => +"-gnatyo",
       Comment_Header_Sample => +"-hg",
       Comment_Header_Spec => +"-hs",
       Keep_Tree_File => +"-k",
       No_Exception => null,
       No_Local_Header => null,
       Reuse_Tree_File => +"-r",
       Overwrite_Tree_File => +"-t"));

   type Stub_Strings is
     (Header_File,
      Output);

   package Stub_String_Switches is new String_Switches
     (Descriptor,
      Stub_Strings);

   package Stub_String_Syntax is new Stub_String_Switches.Set_Syntax
     ((Header_File => '=',
       Output => '='));

   package Stub_String_Shorthands is new Stub_String_Switches
     .Set_Shorthands
     ((Header_File => null,
       Output => +"-o"));

   --  ????????????????Perhaps Max_Line_Length, Indentation should be moved to
   --  Common, and gnatpp and gnatstub shorthands unified. Output is also
   --  shared between gnatpp and gnatstub.

   type Stub_Nats is
     (Max_Line_Length,
      Indentation);

   package Stub_Nat_Switches is new Other_Switches
     (Descriptor,
      Stub_Nats,
      Natural,
      Natural'Image,
      Natural'Value);

   package Stub_Nat_Syntax is new Stub_Nat_Switches.Set_Syntax
     ((Max_Line_Length => '!',
       Indentation => '!'));

   package Stub_Nat_Defaults is new Stub_Nat_Switches.Set_Defaults
     ((Max_Line_Length => 79,
       Indentation => 3));

   package Stub_Nat_Shorthands is new Stub_Nat_Switches.Set_Shorthands
     ((Max_Line_Length => +"-gnatyM",
       Indentation => +"-gnaty"));

   package Stub_Nat_Shorthands_2 is new Stub_Nat_Switches.Set_Shorthands
     ((Max_Line_Length => +"-l",
       Indentation => +"-i"));

   package Freeze is new Freeze_Descriptor (Descriptor);

   use Stub_Flag_Switches,
     Stub_String_Switches,
     Stub_Nat_Switches;

   subtype Cmd_Line is Utils.Command_Lines.Command_Line;

end Stub.Command_Lines;
