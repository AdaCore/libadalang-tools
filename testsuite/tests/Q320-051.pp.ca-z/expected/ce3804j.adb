-- CE3804J.ADA

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
--     CHECK THAT FIXED_IO GET OPERATES ON IN_FILE FILE AND WHEN
--     NO FILE IS SPECIFIED THE CURRENT DEFAULT INPUT FILE IS USED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     DWC 09/14/87  CREATED ORIGINAL TEST.
--     JRL 02/28/96  Changed upper bound of type FX from 1000.0 to 250.0.
--                   Corrected TEST string.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3804j is
   Incomplete : exception;

begin

   Test
     ("CE3804J",
      "CHECK THAT FIXED_IO GET OPERATES ON " &
      "IN_FILE FILE AND WHEN NO FILE IS " &
      "SPECIFIED THE CURRENT DEFAULT INPUT " & "FILE IS USED");

   declare
      Ft1, Ft2 : File_Type;
   begin

-- CREATE AND INITIALIZE FILES

      begin
         Create (Ft1, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT " & "CREATE WITH OUT_FILE MODE - 1");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED; TEXT " & "CREATE WITH OUT_FILE MODE - 1");
            raise Incomplete;
      end;

      Create (Ft2, Out_File, Legal_File_Name (2));

      Put (Ft1, "1.0");
      New_Line (Ft1);

      Close (Ft1);

      begin
         Open (Ft1, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT OPEN " & "FOR IN_FILE MODE");
            raise Incomplete;
      end;

      Put (Ft2, "2.0");
      New_Line (Ft2);

      Close (Ft2);
      Open (Ft2, In_File, Legal_File_Name (2));

      Set_Input (Ft2);

      declare
         type Fx is delta 0.000_1 range 1.0 .. 250.0;
         package Fxio is new Fixed_Io (Fx);
         use Fxio;
         X : Fx;
      begin
         begin
            Get (Ft1, X);
            if X /= 1.0 then
               Failed ("FIXED FILE VALUE INCORRECT");
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED - FILE FIXED");
         end;

         begin
            Get (X);
            if X /= 2.0 then
               Failed ("FIXED DEFAULT VALUE INCORRECT");
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED - DEFAULT FIXED");
         end;
      end;

      begin
         Delete (Ft1);
         Delete (Ft2);
      exception
         when Use_Error =>
            null;
      end;
   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3804j;
