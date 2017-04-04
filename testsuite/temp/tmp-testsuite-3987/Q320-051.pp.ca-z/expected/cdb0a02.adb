------------------------------------------------------------------ CDB0A02

with Report;
with Tctouch;
with System.Storage_Elements;
with Cdb0a02_2;
with Cdb0a02_3;
with Fdb0a00;

procedure Cdb0a02 is

   Banyan : Cdb0a02_2.Small_Tree;
   Torrey : Cdb0a02_3.Large_Tree;

   use type Cdb0a02_2.Small_Tree;
   use type Cdb0a02_3.Large_Tree;

   Countess1 : constant String := "Ada ";
   Countess2 : constant String := "Augusta ";
   Countess3 : constant String := "Lovelace";
   Cenosstu  : constant String := "  AALaaacdeeglostuuv";
   Insertion : constant String :=
     "AAAAAAAAAAAAAAAAAAAA" & "AAAAAAAAAAAAAAAAAAAA";
   Deallocation : constant String := "DDDDDDDDDDDDDDDDDDDD";

begin  -- Main test procedure.

   Report.Test
     ("CDB0A02",
      "Check that several access types can share " &
      "the same pool.  Check that any exception " &
      "propagated by Allocate is propagated by the " &
      "allocator.  Check that for an access type S, " &
      "S'Max_Size_In_Storage_Elements denotes the " &
      "maximum values for Size_In_Storage_Elements " &
      "that will be requested via Allocate");

   -- Check that access types can share the same pool.

   for Count in Countess1'Range loop
      Cdb0a02_2.Insert (Countess1 (Count), Banyan);
   end loop;

   for Count in Countess1'Range loop
      Cdb0a02_3.Insert (Countess1 (Count), Torrey);
   end loop;

   for Count in Countess2'Range loop
      Cdb0a02_2.Insert (Countess2 (Count), Banyan);
   end loop;

   for Count in Countess2'Range loop
      Cdb0a02_3.Insert (Countess2 (Count), Torrey);
   end loop;

   for Count in Countess3'Range loop
      Cdb0a02_2.Insert (Countess3 (Count), Banyan);
   end loop;

   for Count in Countess3'Range loop
      Cdb0a02_3.Insert (Countess3 (Count), Torrey);
   end loop;

   Tctouch.Validate (Insertion, "Allocate calls via CDB0A02_2");

   Cdb0a02_2.Traverse (Banyan);
   Tctouch.Validate (Cenosstu, "Traversal of Banyan");

   Cdb0a02_3.Traverse (Torrey);
   Tctouch.Validate (Cenosstu, "Traversal of Torrey");

   Cdb0a02_2.Defoliate (Banyan);
   Tctouch.Validate (Deallocation, "Deforestation of Banyan");
   Tctouch.Assert (Banyan = null, "Banyan Deallocation result not null");

   Cdb0a02_3.Defoliate (Torrey);
   Tctouch.Validate (Deallocation, "Deforestation of Torrey");
   Tctouch.Assert (Torrey = null, "Torrey Deallocation result not null");

   -- Check that for an access type S, S'Max_Size_In_Storage_Elements
   -- denotes the maximum values for Size_In_Storage_Elements that will
   -- be requested via Allocate. (Of course, all we can do is check that
   -- whatever was requested of Allocate did not exceed the values of the
   -- attributes.)

   Tctouch.Assert
     (Fdb0a00.Tc_Largest_Request in
        1 ..
              System.Storage_Elements.Storage_Count'Max
                (Cdb0a02_2.Small_Cell'Max_Size_In_Storage_Elements,
                 Cdb0a02_3.Large_Cell'Max_Size_In_Storage_Elements),
      "An object of excessive size was allocated.  Size: " &
      System.Storage_Elements.Storage_Count'Image
        (Fdb0a00.Tc_Largest_Request));

-- Check that an exception raised in Allocate is propagated by the allocator.

   Cdb0a02_2.Tc_Exceed_Pool;

   Report.Result;

end Cdb0a02;
