------------------------------------------------------------------- CXH3002

with Report;
with Cxh3002_0.Cxh3002_1;
procedure Cxh3002 is

   use type
     Cxh3002_0.Enum,
     Cxh3002_0.Int,
     Cxh3002_0.Unt,
     Cxh3002_0.Flt,
     Cxh3002_0.Fix,
     Cxh3002_0.Root;

   Main_Enum : Cxh3002_0.Enum                   := Cxh3002_0.Item;
   Main_Int  : Cxh3002_0.Int;
   Main_Unt  : Cxh3002_0.Unt;
   Main_Flt  : Cxh3002_0.Flt;
   Main_Fix  : Cxh3002_0.Fix;
   Main_Rec  : Cxh3002_0.Root (Cxh3002_0.Stuff) :=
     (Cxh3002_0.Stuff, I => 1, U => 2);

   -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---=====
   pragma Inspection_Point (Main_Rec);                               -- BJQO
   -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---=====

   Main_List : Cxh3002_0.List := (others => Main_Rec);

   Main_A_List : Cxh3002_0.A_List := new Cxh3002_0.List'(others => Main_Rec);
   Main_A_Proc : Cxh3002_0.A_Proc := Cxh3002_0.Func;
   -- CXH3002_0.Proc'Access
   Main_Pt : Cxh3002_0.Pt;
   Main_Tt : Cxh3002_0.Tt;

   type Test_Range is (First, Second);

   procedure Assert (Truth : Boolean; Message : String) is
   begin
      if not Truth then
         Report.Failed ("Unexpected value found in " & Message);
      end if;
   end Assert;

begin  -- Main test procedure.

   Report.Test ("CXH3002", "Check pragma Inspection_Point");

   Enclosure : declare
      Main_Final : Cxh3002_0.Cxh3002_1.Final;
      Xtra_Final : Cxh3002_0.Cxh3002_1.Final;
   begin
      for Test_Case in Test_Range loop

         case Test_Case is
            when First =>
               Main_Final.Value                  := 5;
               Xtra_Final                        := Main_Final; -- call Adjust
               Main_Enum                         := Cxh3002_0.Things;
               Main_Int                          := Cxh3002_0.Int'First;
               Main_Unt                          := Cxh3002_0.Unt'Last;
               Main_Flt                          := 3.14;
               Main_Fix                          := 0.5;
               Main_Rec := (Cxh3002_0.Stuff, I => 3, U => 4);
               Main_List (Main_Unt)              := Main_Rec;
               Main_A_List (Cxh3002_0.Unt'First) :=
                 (Cxh3002_0.Stuff, I => 5, U => 6);
               Main_A_Proc (Main_A_List (2));
               Main_Pt.Prot_Entry (True);
               Main_Tt.Task_Entry (null);

            when Second =>
               Assert (Main_Final.Value = 5, "Main_Final");
               Assert (Xtra_Final.Value = 2, "Xtra_Final");
               Assert (Main_Enum = Cxh3002_0.Things, "Main_Enum");
               Assert (Main_Int = Cxh3002_0.Int'First, "Main_Int");
               Assert (Main_Unt = Cxh3002_0.Unt'Last, "Main_Unt");
               Assert (Main_Flt in 3.0 .. 3.5, "Main_Flt");
               Assert (Main_Fix = 0.5, "Main_Fix");
               Assert
                 (Main_Rec = (Cxh3002_0.Stuff, I => 3, U => 4),
                  "Main_Rec");
               Assert (Main_List (Main_Unt) = Main_Rec, "Main_List");
               Assert
                 (Main_A_List (Cxh3002_0.Unt'First) =
                  (Cxh3002_0.Stuff, I => 5, U => 6),
                  "Main_A_List");

         end case;

      -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---==
         pragma Inspection_Point
           (                                       -- CQP
           Main_Final,                                    -- M
            Main_Enum,                                     -- D
            Main_Int,                                      -- E
            Main_Unt,                                      -- E
            Main_Flt,                                      -- G
            Main_Fix,                                      -- H
            Main_Rec,                                      -- J
            Main_List,                                     -- I
            Main_A_List,                                   -- F
            Main_A_Proc,                                   -- F
            Main_Pt,                                       -- L
            Main_Tt);                                     -- N
      -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---==

      end loop;
   end Enclosure;

   Report.Result;

end Cxh3002;
