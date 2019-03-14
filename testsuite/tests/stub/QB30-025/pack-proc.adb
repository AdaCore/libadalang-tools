pragma Ada_2012;
separate (Pack)
procedure Proc (I : Integer; B : Boolean) is
begin
   --  Generated stub: replace with real body!
   pragma Compile_Time_Warning (Standard.True, "Proc unimplemented");
   raise Program_Error with "Unimplemented procedure Proc";
end Proc;
