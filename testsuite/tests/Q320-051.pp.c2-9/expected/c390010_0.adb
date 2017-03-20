-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- C390010_0

with Report;
with Tctouch;
package body C390010_0 is

   procedure Dispatching_Op (Dto : in out Discr_Tag_Record) is
   begin
      Tctouch.Touch
        ('1');  --------------------------------------------------- 1
      if Dto.Disc then
         Tctouch.Touch
           (Dto.Fieldb);  ------------------------------------------ B
      else
         Tctouch.Touch
           (Dto.Fieldc);  ------------------------------------------ C
      end if;
   end Dispatching_Op;

   procedure Dispatching_Op (Dr : in out Derived_Record) is
   begin
      Tctouch.Touch
        ('2');  --------------------------------------------------- 2
      if Dr.Disc1 then
         Tctouch.Touch
           (Dr.Fieldb);   ------------------------------------------ B
      else
         Tctouch.Touch
           (Dr.Fieldc);   ------------------------------------------ C
      end if;
      if Dr.Disc2 then
         Tctouch.Touch
           (Dr.Fielde);   ------------------------------------------ E
      else
         Tctouch.Touch
           (Dr.Fieldf);   ------------------------------------------ F
      end if;
   end Dispatching_Op;

   procedure Pcw_Op (Spca : in Subtype_Parent_Class_Access) is
   begin

      -- the following line is the "heart" of this test, objects of all types
      -- covered by the classwide type will be passed to this subprogram in
      -- the execution of the test.
      if Spca.Disc then
         Tctouch.Touch
           (Spca.Fieldb); ------------------------------------------ B
      else
         Tctouch.Touch
           (Spca.Fieldc); ------------------------------------------ C
      end if;

      Dispatching_Op (Spca.all);  -- check that this dispatches correctly,
      -- with discriminants correctly represented

   end Pcw_Op;

end C390010_0;
