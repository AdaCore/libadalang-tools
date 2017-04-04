with C340001_0; -- nonlimited tagged record declarations
with C340001_1; -- limited tagged record declarations
with C340001_2; -- tagged variant declarations
with Ada.Calendar;
with Report;
procedure C340001 is

   Db_Rec1 : C340001_0.Db_Record := (Key => 1, Data => "aaaaaaaaaa");
   Db_Rec2 : C340001_0.Db_Record := (Key => 55, Data => "aaaaaaaaaa");
   -- DB_Rec1  = DB_Rec2 using user-defined equality
   -- DB_Rec1 /= DB_Rec2 using predefined equality

   Some_Time : Ada.Calendar.Time :=
     Ada.Calendar.Time_Of (Month => 9, Day => 16, Year => 1_993);

   Another_Time : Ada.Calendar.Time :=
     Ada.Calendar.Time_Of (Month => 9, Day => 19, Year => 1_993);

   Dated_Rec1 : C340001_0.Dated_Record :=
     (Key => 2, Data => "aaaaaaaaaa", Retrieval_Time => Some_Time);
   Dated_Rec2 : C340001_0.Dated_Record :=
     (Key => 77, Data => "aaaaaaaaaa", Retrieval_Time => Some_Time);
   Dated_Rec3 : C340001_0.Dated_Record :=
     (Key => 77, Data => "aaaaaaaaaa", Retrieval_Time => Another_Time);
   -- Dated_Rec1  = Dated_Rec2 if DB_Record."=" used for parent portion
   -- Dated_Rec2 /= Dated_Rec3 if extended component is compared
   --    using Ada.Calendar.Time."="

   List1 : C340001_1.List;
   List2 : C340001_1.List;

   Rlist1 : C340001_1.Revisable_List;
   Rlist2 : C340001_1.Revisable_List;
   Rlist3 : C340001_1.Revisable_List;

   Current : C340001_2.Transaction (C340001_2.Paper) :=
     (C340001_2.Paper, 2_001);
   Last : C340001_2.Transaction (C340001_2.Electronic) :=
     (C340001_2.Electronic, 2_001);
   -- Current  = Last using user-defined equality
   -- Current /= Last using predefined equality

   Approval1 : C340001_2.Authorization (C340001_2.Paper) :=
     (Kind => C340001_2.Paper, Id => 1_040, Signature_On_File => True);
   Approval2 : C340001_2.Authorization (C340001_2.Paper) :=
     (Kind => C340001_2.Paper, Id => 2_167, Signature_On_File => False);
   Approval3 : C340001_2.Authorization (C340001_2.Electronic) :=
     (Kind => C340001_2.Electronic, Id => 2_167, Paper_Backup => False);
   -- Approval1 /= Approval2 if user-defined equality extended with
   --    component equality.
   -- Approval2 /= Approval3 if differing variant parts checked

   -- Direct visibility to operator symbols
   use type C340001_0.Db_Record;
   use type C340001_0.Dated_Record;

   use type C340001_1.List;
   use type C340001_1.Revisable_List;

   use type C340001_2.Transaction;
   use type C340001_2.Authorization;

begin

   Report.Test ("C340001", "Inheritance of user-defined ""=""");

   -- Approval1 /= Approval2 if user-defined equality extended with
   --    component equality.
   -- Approval2 /= Approval3 if differing variant parts checked

   ---------------------------------------------------------------------
   -- Check that "=" and "/=" for the parent type call the user-defined
   -- operation
   ---------------------------------------------------------------------

   if not (Db_Rec1 = Db_Rec2) then
      Report.Failed
        ("Nonlimited tagged record: " &
         "User-defined equality did not override predefined " &
         "equality");
   end if;

   if Db_Rec1 /= Db_Rec2 then
      Report.Failed
        ("Nonlimited tagged record: " &
         "User-defined equality did not override predefined " &
         "inequality as well");
   end if;

   ---------------------------------------------------------------------
   -- Check that "=" and "/=" for the type extension use the user-defined
   -- equality operations from the parent to compare the inherited
   -- components
   ---------------------------------------------------------------------

   if not (Dated_Rec1 = Dated_Rec2) then
      Report.Failed
        ("Nonlimited tagged record: " &
         "User-defined equality was not used to compare " &
         "components inherited from parent");
   end if;

   if Dated_Rec1 /= Dated_Rec2 then
      Report.Failed
        ("Nonlimited tagged record: " &
         "User-defined inequality was not used to compare " &
         "components inherited from parent");
   end if;

   ---------------------------------------------------------------------
   -- Check that equality and inequality for the type extension incorporate
   -- the predefined equality operators for the extended component type
   ---------------------------------------------------------------------
   if Dated_Rec2 = Dated_Rec3 then
      Report.Failed
        ("Nonlimited tagged record: " &
         "Record equality was not extended with component " &
         "equality");
   end if;

   if not (Dated_Rec2 /= Dated_Rec3) then
      Report.Failed
        ("Nonlimited tagged record: " &
         "Record inequality was not extended with component " &
         "equality");
   end if;

   ---------------------------------------------------------------------
   C340001_1.Add_To (List1, 1);
   C340001_1.Add_To (List1, 2);
   C340001_1.Add_To (List1, 3);
   C340001_1.Remove_From (List1);

   C340001_1.Add_To (List2, 1);
   C340001_1.Add_To (List2, 2);

   -- List1 contents are (2, (1, 2, 3, 0, 0, 0, 0, 0, 0, 0))
   -- List2 contents are (2, (1, 2, 0, 0, 0, 0, 0, 0, 0, 0))

   -- List1 = List2 using user-defined equality
   -- List1 /= List2 using predefined equality

   ---------------------------------------------------------------------
   -- Check that "=" and "/=" for the parent type call the user-defined
   -- operation
   ---------------------------------------------------------------------
   if not (List1 = List2) then
      Report.Failed
        ("Limited tagged record   : " &
         "User-defined equality incorrectly implemented ");
   end if;

   if List1 /= List2 then
      Report.Failed
        ("Limited tagged record   : " &
         "User-defined equality incorrectly implemented ");
   end if;

   ---------------------------------------------------------------------
   -- RList1 and RList2 are made equal but "different" by adding
   -- a nonzero value to RList1 then removing it. Removal updates
   -- the list Length only, not its contents. The two lists will be
   -- equal according to the defined list abstraction, but the records
   -- will contain differing component values.

   C340001_1.Add_To (Rlist1, 1);
   C340001_1.Add_To (Rlist1, 2);
   C340001_1.Add_To (Rlist1, 3);
   C340001_1.Remove_From (Rlist1);

   C340001_1.Add_To (Rlist2, 1);
   C340001_1.Add_To (Rlist2, 2);

   C340001_1.Add_To (Rlist3, 1);
   C340001_1.Add_To (Rlist3, 2);

   C340001_1.Revise (Rlist3);

   -- RList1 contents are (2, (1, 2, 3, 0, 0, 0, 0, 0, 0, 0), 'A')
   -- RList2 contents are (2, (1, 2, 0, 0, 0, 0, 0, 0, 0, 0), 'A')
   -- RList3 contents are (2, (1, 2, 0, 0, 0, 0, 0, 0, 0, 0), 'B')

   -- RList1 = RList2 if List."=" inherited
   -- RList2 /= RList3 if List."=" inherited and extended with Character "="

   ---------------------------------------------------------------------
   -- Check that "=" and "/=" are the user-defined operations inherited
   -- from the parent type.
   ---------------------------------------------------------------------
   if not (Rlist1 = Rlist2) then
      Report.Failed
        ("Limited tagged record   : " &
         "User-defined equality was not inherited");
   end if;

   if Rlist1 /= Rlist2 then
      Report.Failed
        ("Limited tagged record   : " &
         "User-defined inequality was not inherited");
   end if;
   ---------------------------------------------------------------------
   -- Check that "=" and "/=" for the type extension are NOT extended
   -- with the predefined equality operators for the extended component.
   -- A limited type extension should inherit the parent equality operation
   -- as is.
   ---------------------------------------------------------------------
   if not (Rlist2 = Rlist3) then
      Report.Failed
        ("Limited tagged record   : " &
         "Inherited equality operation was extended with " &
         "component equality");
   end if;

   if Rlist2 /= Rlist3 then
      Report.Failed
        ("Limited tagged record   : " &
         "Inherited inequality operation was extended with " &
         "component equality");
   end if;

   ---------------------------------------------------------------------
   -- Check that "=" and "/=" for the parent type call the user-defined
   -- operation
   ---------------------------------------------------------------------
   if not (Current = Last) then
      Report.Failed
        ("Variant record          : " &
         "User-defined equality did not override predefined " &
         "equality");
   end if;

   if Current /= Last then
      Report.Failed
        ("Variant record          : " &
         "User-defined inequality did not override predefined " &
         "inequality");
   end if;

   ---------------------------------------------------------------------
   -- Check that user-defined equality was incorporated and extended
   -- with equality of extended components.
   ---------------------------------------------------------------------
   if not (Approval1 /= Approval2) then
      Report.Failed
        ("Variant record          : " &
         "Inequality was not extended with component " &
         "inequality");
   end if;

   if Approval1 = Approval2 then
      Report.Failed
        ("Variant record          : " &
         "Equality was not extended with component " &
         "equality");
   end if;

   ---------------------------------------------------------------------
   -- Check that equality and inequality for the type extension
   -- succeed despite the presence of differing variant parts.
   ---------------------------------------------------------------------
   if Approval2 = Approval3 then
      Report.Failed
        ("Variant record          : " &
         "Equality succeeded even though variant parts " &
         "in type extension differ");
   end if;

   if not (Approval2 /= Approval3) then
      Report.Failed
        ("Variant record          : " &
         "Inequality failed even though variant parts " &
         "in type extension differ");
   end if;

   ---------------------------------------------------------------------
   Report.Result;
   ---------------------------------------------------------------------

end C340001;
