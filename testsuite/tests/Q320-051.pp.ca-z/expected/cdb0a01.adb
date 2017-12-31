------------------------------------------------------------------ CDB0A01

with Report;
with Tctouch;
with Fdb0a00.Comparator;
with Fdb0a00.Pool1;
with Cdb0a01_2;
with Cdb0a01_3;

procedure Cdb0a01 is

   Banyan : Cdb0a01_2.User_Pool_Tree;
   Torrey : Cdb0a01_3.System_Pool_Tree;

   use type Cdb0a01_2.User_Pool_Tree;
   use type Cdb0a01_3.System_Pool_Tree;

   Countess     : constant String := "Ada Augusta Lovelace";
   Cenosstu     : constant String := "  AALaaacdeeglostuuv";
   Insertion    : constant String := "AAAAAAAAAAAAAAAAAAAA";
   Deallocation : constant String := "DDDDDDDDDDDDDDDDDDDD";

begin  -- Main test procedure.

   Report.Test
     ("CDB0A01",
      "Check that a storage pool may be " &
      "user_determined, and that storage is " &
      "allocated by calling Allocate.  Check that " &
      "a storage.pool may be specified using " &
      "'Storage_Pool and that S'Storage_Pool denotes " &
      "the storage pool of the type S");

--      Check that S'Storage_Pool denotes the storage pool for the type S.

   Tctouch.Assert
     (Fdb0a00.Comparator."="
        (Fdb0a00.Pool1.User_Pool, Cdb0a01_2.User_Pool_Tree'Storage_Pool),
      "'Storage_Pool not correct for CDB0A01_2.User_Pool_Tree");

   Tctouch.Assert_Not
     (Fdb0a00.Comparator."="
        (Fdb0a00.Pool1.User_Pool, Cdb0a01_3.System_Pool_Tree'Storage_Pool),
      "'Storage_Pool not correct for CDB0A01_3.System_Pool_Tree");

--      Check that storage is allocated by calling Allocate.

   for Count in Countess'Range loop
      Cdb0a01_2.Insert (Countess (Count), Banyan);
   end loop;
   Tctouch.Validate (Insertion, "Allocate calls via CDB0A01_2");

   for Count in Countess'Range loop
      Cdb0a01_3.Insert (Countess (Count), Torrey);
   end loop;
   Tctouch.Validate ("", "Allocate calls via CDB0A01_3");

   Cdb0a01_2.Traverse (Banyan);
   Tctouch.Validate (Cenosstu, "Traversal of Banyan");

   Cdb0a01_3.Traverse (Torrey);
   Tctouch.Validate (Cenosstu, "Traversal of Torrey");

   Cdb0a01_2.Defoliate (Banyan);
   Tctouch.Validate (Deallocation, "Deforestation of Banyan");
   Tctouch.Assert (Banyan = null, "Banyan Deallocation result not null");

   Cdb0a01_3.Defoliate (Torrey);
   Tctouch.Validate ("", "Deforestation of Torrey");
   Tctouch.Assert (Torrey = null, "Torrey Deallocation result not null");

   Report.Result;

end Cdb0a01;
