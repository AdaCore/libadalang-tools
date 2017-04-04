-- CE3804E.ADA

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
--     CHECK THAT FIXED_IO GET RAISES DATA_ERROR WHEN THE DATA READ IS
--     OUT-OF-RANGE CHECK THAT ITEM IS LEFT UNAFFECTED AND THAT
--     READING MAY CONTINUE AFTER THE EXCEPTION HAS BEEN HANDLED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 09/07/82
--     SPS 02/10/83
--     JBG 08/30/83
--     EG  11/02/84
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/11/87  REMOVED UNNECESSARY CODE AND CORRECTED
--                   EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3804e is
   Incomplete : exception;

begin

   Test
     ("CE3804E",
      "FIXED_IO GET RAISES DATA_ERROR FOR " & "OUT-OF-RANGE DATA");

   declare
      Ft : File_Type;
   begin

-- CREATE AND INITIALIZE TEST FILE

      begin
         Create (Ft, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED; TEXT CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
      end;

      Put (Ft, "1.25");
      New_Line (Ft);
      Put (Ft, "-7.5");
      New_Line (Ft);
      Put (Ft, "3.5");
      New_Line (Ft);
      Put (Ft, "2.5");
      New_Line (Ft);
      Close (Ft);

-- BEGIN TEST

      declare
         type Fx is delta 0.001 range 1.0 .. 3.0;
         package Fx_Io is new Fixed_Io (Fx);
         X : Fx;
         use Fx_Io;
      begin

         begin
            Open (Ft, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable
                 ("USE_ERROR RAISED; TEXT " & "OPEN WITH IN_FILE MODE");
               raise Incomplete;
         end;

         Get (Ft, X, 0);

         begin
            Get (Ft, X, 0);
            Failed ("DATA_ERROR NOT RAISED - 1");
         exception
            when Data_Error =>
               if X /= 1.25 then
                  Failed ("ITEM ALTERED WHEN DATA_ERROR " & "IS RAISED - 1");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 1");
         end;

         begin
            Get (Ft, X, 0);
            Failed ("DATA_ERROR NOT RAISED - 2");
         exception
            when Data_Error =>
               if X /= 1.25 then
                  Failed ("ITEM ALTERED WHEN DATA_ERROR " & "IS RAISED - 2");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 2");
         end;

         Get (Ft, X, 0);
         if X /= 2.5 then
            Failed ("READING NOT CONTINUED CORRECTLY " & "AFTER DATA_ERROR");
         end if;

         begin
            Delete (Ft);
         exception
            when Use_Error =>
               null;
         end;

      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3804e;
