-- CE2102V.ADA

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
--     CHECK THAT USE_ERROR IS RAISED WHEN OPENING A FILE OF MODE
--     OUT_FILE, WHEN OUT_FILE MODE IS NOT SUPPORTED FOR OPEN BY THE
--     IMPLEMENTATION FOR DIRECT FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH DO NOT
--     SUPPORT OPEN WITH OUT_FILE MODE FOR DIRECT FILES.

-- HISTORY:
--     TBN 07/23/87  CREATED ORIGINAL TEST.

with Report; use Report;
with Direct_Io;

procedure Ce2102v is
begin

   Test
     ("CE2102V",
      "CHECK THAT USE_ERROR IS RAISED WHEN MODE " &
      "OUT_FILE IS NOT SUPPORTED FOR THE OPERATION " &
      "OF OPEN FOR DIRECT FILES");
   declare
      package Dir is new Direct_Io (Boolean);
      use Dir;
      File1 : File_Type;
      Incomplete : exception;
      Var1 : Boolean := False;
   begin
      begin
         Create (File1, Inout_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON CREATE FOR " & "INOUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON CREATE FOR " & "INOUT_FILE MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE");
            raise Incomplete;
      end;

      Write (File1, Var1);
      Close (File1);

      begin
         Open (File1, Out_File, Legal_File_Name);
         Not_Applicable ("OPEN FOR OUT_FILE MODE ALLOWED");
      exception
         when Use_Error =>
            null;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON OPEN");
      end;

      if Is_Open (File1) then
         begin
            Delete (File1);
         exception
            when Use_Error =>
               null;
         end;
      end if;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce2102v;
