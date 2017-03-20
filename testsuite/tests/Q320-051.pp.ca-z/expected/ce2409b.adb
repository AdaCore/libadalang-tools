-- CE2409B.ADA

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
--     FOR DIRECT ACCESS FILES, CHECK THAT A WRITE TO A POSITION
--     GREATER THAN THE CURRENT END POSITION CAUSES THE WRITE
--     POSITION AND THE FILE SIZE TO BE INCREMENTED.

--          2) CHECK FILES OF MODE OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     CREATE WITH MODE OUT_FILE FOR DIRECT FILES.

-- HISTORY:
--     GMT 08/05/87  CREATED ORIGINAL TEST.

with Report; use Report;
with Direct_Io;

procedure Ce2409b is

   package Dir is new Direct_Io (Integer);
   use Dir;
   File1 : File_Type;
   Incomplete : exception;

begin

   Test
     ("CE2409B",
      "CHECK THAT WRITE POSITION AND " & "SIZE ARE INCREMENTED APPROPRIATELY");
   begin
      Create (File1, Out_File);
   exception
      when Use_Error =>
         Not_Applicable
           ("CREATE WITH MODE OUT_FILE NOT " & "SUPPORTED FOR DIR FILES - 1");
         raise Incomplete;
   end;

   declare
      Int      : Integer        := Ident_Int (18);
      Two_C    : Count          := Count (Ident_Int (2));
      Three_C  : Count          := Count (Ident_Int (3));
      Three_Pc : Positive_Count := Positive_Count (Ident_Int (3));
      Four_Pc  : Positive_Count := Positive_Count (Ident_Int (4));
   begin
      Write (File1, Int);
      Write (File1, Int);
      if Index (File1) /= Three_Pc then
         Failed ("INCORRECT VALUE FOR INDEX - 2");
      end if;
      if Size (File1) /= Two_C then
         Failed ("INCORRECT VALUE FOR SIZE - 3");
      end if;

      Write (File1, Int);
      if Index (File1) /= Four_Pc then
         Failed ("INCORRECT VALUE FOR INDEX - 4");
      end if;
      if Size (File1) /= Three_C then
         Failed ("INCORRECT VALUE FOR SIZE - 5");
      end if;

   end;

   Close (File1);

   Result;

exception
   when Incomplete =>
      Result;

end Ce2409b;
