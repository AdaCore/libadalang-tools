-- CE2102O.ADA

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
--     CHECK THAT USE_ERROR IS RAISED WHEN RESETTING A FILE OF MODE
--     IN_FILE, WHEN IN_FILE MODE IS NOT SUPPORTED FOR RESET BY THE
--     IMPLEMENTATION FOR SEQUENTIAL FILES.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH DO NOT
--     SUPPORT RESET WITH IN_FILE MODE FOR SEQUENTIAL FILES.

-- HISTORY:
--     TBN 07/23/87  CREATED ORIGINAL TEST.

with Report; use Report;
with Sequential_Io;

procedure Ce2102o is
begin

   Test
     ("CE2102O",
      "CHECK THAT USE_ERROR IS RAISED WHEN RESETTING " &
      "A FILE OF MODE IN_FILE, WHEN IN_FILE MODE IS " &
      "NOT SUPPORTED FOR RESET BY THE IMPLEMENTATION " &
      "FOR SEQUENTIAL FILES");

   declare
      package Seq is new Sequential_Io (Boolean);
      use Seq;
      File1 : File_Type;
      Incomplete : exception;
      Var1 : Boolean := False;
   begin
      begin
         Create (File1, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON CREATE FOR " & "OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON CREATE FOR " & "OUT_FILE MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE");
            raise Incomplete;
      end;

      Write (File1, Var1);
      Close (File1);

      begin
         begin
            Open (File1, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable ("OPEN WITH IN_FILE MODE NOT " & "SUPPORTED");
               raise Incomplete;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED ON OPEN");
               raise Incomplete;
         end;

         begin
            Reset (File1);
            Not_Applicable ("RESET FOR IN_FILE MODE IS " & "SUPPORTED");
         exception
            when Use_Error =>
               null;
            when others =>
               Failed ("UNEXPECTED EXCEPTION RAISED ON " & "RESET");
         end;
      exception
         when Incomplete =>
            null;
      end;

      begin
         Delete (File1);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce2102o;
