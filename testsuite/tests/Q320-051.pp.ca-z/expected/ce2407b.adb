-- CE2407B.ADA

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
--     CHECK THAT WRITE RAISES MODE_ERROR WHEN THE CURRENT MODE
--     IS IN_FILE.

--          2) CHECK TEMPORARY FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH OUT_FILE MODE AND RESET FROM OUT_FILE MODE TO
--     IN_FILE MODE.

-- HISTORY:
--     GMT 08/06/86  CREATED ORIGINAL TEST.

with Report; use Report;
with Direct_Io;

procedure Ce2407b is

   package Dir is new Direct_Io (Integer);
   use Dir;
   Incomplete : exception;
   File2 : File_Type;
   Int   : Integer := Ident_Int (18);

begin
   Test
     ("CE2407B",
      "CHECK THAT WRITE RAISES MODE_ERROR WHEN THE " &
      "CURRENT MODE IS IN_FILE AND THE FILE IS " & "A TEMPORARY FILE");
   begin
      Create (File2, Out_File);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON CREATE - 1");
         raise Incomplete;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE - 2");
         raise Incomplete;
   end;

   Write (File2, Int);

   begin
      Reset (File2, In_File);
   exception
      when Use_Error =>
         Not_Applicable ("USE_ERROR RAISED ON RESET - 3");
         raise Incomplete;
   end;

   begin
      Write (File2, Int);
      Failed ("MODE_ERROR NOT RAISED ON WRITE - 4");
   exception
      when Mode_Error =>
         null;
      when others =>
         Failed ("OTHER EXCEPTION RAISED ON WRITE - 5");
   end;

   Close (File2);

   Result;

exception
   when Incomplete =>
      Result;

end Ce2407b;
