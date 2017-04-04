with Report;
with Tctouch;
with Cc30003_Root;
with Cc30003_1; use Cc30003_1;
with Cc30003_2; use Cc30003_2;
with Cc30003_3; use Cc30003_3;
with Cc30003_4; use Cc30003_4;
procedure Cc30003 is
   use Cc30003_3.Counted_Rectangle_Inst;
   use Cc30003_4.Counted_Triangle_Inst;

   type Array_Of_Objects is
     array (Positive range <>) of access Cc30003_Root.Object'Class;

   function Total_Area (Objects : in Array_Of_Objects) return Float is
      Result : Float := 0.0;
   begin
      for I in Objects'Range loop
         Result := Result + Cc30003_Root.Area (Objects (I).all);
      end loop;
      return Result;
   end Total_Area;

begin
   Report.Test
     ("CC30003",
      "Check that, for a type extension derived from " &
      "a formal tagged private type, a whole new set of primitive " &
      "subprograms is declared for use outside of the instance. " &
      "Check that primitive subprograms of the type extension " &
      "can override those inherited from the actual type in " &
      "the instance");

   declare
      R : Rectangle :=
        (X_Coord => 0.0, Y_Coord => 0.0, Height => 2.0, Width => 3.0);
      S : Counted_Square;
      T : Counted_Triangle_Inst.Counted_Type :=
        (X_Coord => 5.0, Y_Coord => 5.0, A => 3.0, B => 4.0, Count => 0);
      Rt : Right_Triangle :=
        (X_Coord => 7.0, Y_Coord => 3.0, A => 3.0, B => 4.0);
   begin
      -- Statically bound calls:
      Make_Square (S, Side => 4.0);
      Tctouch.Validate ("smC", "Sqr initialization bad");
      if Area (R) /= 6.0 then
         Report.Failed ("Wrong area (R)");
      end if;
      if Area (S) /= 16.0 then
         Report.Failed ("Wrong area (S)");
      end if;
      if Area (T) /= 6.0 then
         Report.Failed ("Wrong area (T)");
      end if;
      Tctouch.Validate ("rasata", "Unexpected calls in area calculations");
      if Area (Rt) /= 6.0 then
         Report.Failed ("Wrong area (RT)");
      end if;
      Clear (Rt); -- Original routine.
      if Area (Rt) /= 0.0 then
         Report.Failed ("Wrong area (RTC)");
      end if;
      Tctouch.Validate
        ("tatcta",
         "Unexpected calls in right triangle calculations");

      Clear (T); -- Note: Original Clear overridden by generic one.
      Bump (T);
      Bump (T);
      Bump (S); -- Note: Generic Bump overriden by primitive one.
      if Count (S) /= 1 then
         Report.Failed ("Wrong count (S)");
      end if;
      if Count (T) /= 2 then
         Report.Failed ("Wrong count (T)");
      end if;

      Tctouch.Validate ("CBBsbBcc", "Unexpected calls in count calculations");

      -- Dynamically bound calls in generic instances:
      Cc30003_4.Counted_Triangle_Inst.Double_Bump (T);
      Cc30003_3.Counted_Rectangle_Inst.Double_Bump (S);
      if Count (S) /= 3 then
         Report.Failed ("Wrong count (S2)");
      end if;
      if Count (T) /= 4 then
         Report.Failed ("Wrong count (T2)");
      end if;

      Tctouch.Validate
        ("DBBDsbBsbBcc",
         "Unexpected calls in dynamic count calculations");
   end;

   -- Dynamically bound calls here:
   declare
      List : Array_Of_Objects (1 .. 4);
   begin
      List (Report.Ident_Int (1)) :=
        new Rectangle'
          (X_Coord => 0.0, Y_Coord => 0.0, Height => 1.5, Width => 4.0);
      List (Report.Ident_Int (2)) := new Counted_Square;
      List (Report.Ident_Int (3)) :=
        new Counted_Triangle_Inst.Counted_Type'
          (X_Coord => 5.0, Y_Coord => 5.0, A => 3.0, B => 4.0, Count => 0);
      List (Report.Ident_Int (4)) :=
        new Counted_Rectangle_Inst.Counted_Type'
          (X_Coord => 5.0,
           Y_Coord => 5.0,
           Height  => 2.0,
           Width   => 2.5,
           Count   => 0);
      Make_Square (Counted_Square (List (2).all), Side => 3.0);
      Tctouch.Validate ("smC", "2nd Sqr initialization bad");

      Bump (Counted_Rectangle_Inst.Counted_Type'Class (List (2).all));
      Bump (Counted_Triangle_Inst.Counted_Type'Class (List (3).all));
      Bump (Counted_Triangle_Inst.Counted_Type'Class (List (3).all));
      Bump (Counted_Rectangle_Inst.Counted_Type'Class (List (4).all));
      Bump (Counted_Rectangle_Inst.Counted_Type'Class (List (4).all));
      Bump (Counted_Rectangle_Inst.Counted_Type'Class (List (4).all));
      if Count (Counted_Rectangle_Inst.Counted_Type'Class (List (2).all)) /=
        1
      then
         Report.Failed ("Wrong count (L2)");
      end if;
      if Count (Counted_Triangle_Inst.Counted_Type'Class (List (3).all)) /=
        2
      then
         Report.Failed ("Wrong count (L3)");
      end if;
      if Count (Counted_Rectangle_Inst.Counted_Type'Class (List (4).all)) /=
        3
      then
         Report.Failed ("Wrong count (L4)");
      end if;
      Tctouch.Validate
        ("sbBBBBBBccc",
         "Unexpected calls dispatching count calculations");

      if Total_Area (List) /= (6.0 + 9.0 + 6.0 + 5.0) then
         Report.Failed ("Wrong total area");
      end if;
      Tctouch.Validate ("rasatara", "Unexpected calls in total area");
   end;

   Report.Result;
end Cc30003;
