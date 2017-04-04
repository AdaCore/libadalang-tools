with C650a01_Map, C650a01_Alerts, F650a00.P, F650a00.S, Report, Ada.Tags;
procedure C650a01 is
begin
   Report.Test
     ("C650A01",
      "Check that Constraint_Error is raised if the " &
      "result subtype of the function is an anonymous " &
      "access type designating a specific tagged " &
      "type and the result value is not null and " &
      "designates some other specific type");

   C650a01_Map.Clear_Map;

   -- Put the objects into the Map:
   C650a01_Map.Set_Item (Key => 4, Item => C650a01_Alerts.Special'Access);
   C650a01_Map.Set_Item (Key => 15, Item => C650a01_Alerts.Practice'Access);
   C650a01_Map.Set_Item (Key => 66, Item => C650a01_Alerts.Trial'Access);

   -- Check that we can retrieve the items with access-to-classwide:
   declare
      Pa : access F650a00.Alert'Class;
      Ta : access F650a00.Alert'Class;
      Sa : access F650a00.Alert'Class;
      Na : access F650a00.Alert'Class;
      use type Ada.Tags.Tag;
   begin
      Pa := C650a01_Map.Get_Item (Key => 15);
      Ta := C650a01_Map.Get_Item (Key => 66);
      Sa := C650a01_Map.Get_Item (Key => 4);
      Na := C650a01_Map.Get_Item (Key => 33); -- No item here.
      if Pa.all'Tag /= F650a00.P.Practice_Alert'Tag
        or else Pa.Alert_Time /= 8.0
      then
         Report.Failed ("PA object has wrong value");
      end if;
      if Ta.all'Tag /= F650a00.P.Practice_Alert'Tag
        or else Ta.Alert_Time /= 0.0
      then
         Report.Failed ("TA object has wrong value");
      end if;
      if Sa.all'Tag /= F650a00.S.Special_Alert'Tag
        or else Sa.Alert_Time /= 54.0
      then
         Report.Failed ("SA object has wrong value");
      end if;
      if Na /= null then
         Report.Failed ("NA object has wrong value");
      end if;
      -- These are accesses to the actual objects: verify that for TA:
      Ta.Set_Alert_Time (Time => 3.5);
      if C650a01_Alerts.Trial.Alert_Time /= 3.5 then
         Report.Failed ("TA object did not change");
      end if;
   end;

   -- OK, try retriving practice items.
   declare
      Pa : access F650a00.P.Practice_Alert;
      Sa : access F650a00.P.Practice_Alert;
      Na : access F650a00.P.Practice_Alert;
   begin
      begin
         Pa := C650a01_Map.Get_Practice_Item (Key => 15);
         if Pa.Alert_Time /= 8.0 then
            Report.Failed ("PA practice object has wrong value");
         end if;
      exception
         when Constraint_Error =>
            Report.Failed ("Get_Practice_Item of PA object raised exception");
      end;
      begin
         Sa := C650a01_Map.Get_Practice_Item (Key => 4);
         Report.Failed
           ("Get_Practice_Item of Special object did not raise an " &
            "exception");
      exception
         when Constraint_Error =>
            null;
      end;
      begin
         Na := C650a01_Map.Get_Practice_Item (Key => 54);
         if Na /= null then
            Report.Failed ("NA practice object has wrong value");
         end if;
      exception
         when Constraint_Error =>
            Report.Failed
              ("Get_Practice_Item of unused item raised exception");
      end;
   end;

   Report.Result;

end C650a01;
