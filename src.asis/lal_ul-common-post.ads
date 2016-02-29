package LAL_UL.Common.Post is

   procedure Postprocess_Common (Cmd : Command_Line);
   --  This copies information from Cmd into various global variables, for
   --  compatibility with tools that don't use LAL_UL. ????????????????Get rid
   --  of?

end LAL_UL.Common.Post;
