-- CE3801B.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- OBJECTIVE:
--     CHECK THAT EACH FIXED_IO OPERATION RAISES STATUS_ERROR
--     WHEN CALLED WITH A FILE PARAMETER DESIGNATING AN UN-OPEN FILE.

-- HISTORY:
--     DWC 09/11/87  CREATED ORIGINAL TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3801b is
begin

   Test
     ("CE3801B",
      "CHECK THAT EACH FIXED_IO " &
      "OPERATION RAISES STATUS_ERROR WHEN CALLED " &
      "WITH A FILE PARAMETER DESIGNATING AN " &
      "UN-OPEN FILE");

   declare
      type Fix is delta 0.1 range 1.0 .. 10.0;
      package Fix_Io is new Fixed_Io (Fix);
      use Fix_Io;
      X  : Fix := Fix'Last;
      Ft : File_Type;

   begin
      begin
         Get (Ft, X);
         Failed ("STATUS_ERROR NOT RAISED - GET FIXED_IO - 1");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET " & "FIXED_IO - 1");
      end;

      begin
         Put (Ft, X);
         Failed ("STATUS_ERROR NOT RAISED - PUT FIXED_IO - 1");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PUT " & "FIXED_IO - 1");
      end;

      begin
         Create (Ft, Out_File);   -- THIS IS JUST AN ATTEMPT TO
         Close (Ft);              -- CREATE A FILE.  OBJECTIVE
      exception                     -- IS MET EITHER WAY.
         when Use_Error =>
            null;
      end;

      begin
         Get (Ft, X);
         Failed ("STATUS_ERROR NOT RAISED - GET FIXED_IO - 2");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - GET " & "FIXED_IO - 2");
      end;

      begin
         Put (Ft, X);
         Failed ("STATUS_ERROR NOT RAISED - PUT FIXED_IO - 2");
      exception
         when Status_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - PUT " & "FIXED_IO - 2");
      end;
   end;

   Result;

end Ce3801b;
