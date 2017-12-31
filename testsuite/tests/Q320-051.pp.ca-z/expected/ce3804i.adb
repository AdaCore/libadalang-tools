-- CE3804I.ADA

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
--     CHECK THAT FLOAT_IO GET OPERATES ON IN_FILE FILE AND WHEN
--     NO FILE IS SPECIFIED THE CURRENT DEFAULT INPUT FILE IS USED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/06/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/14/87  SPLIT CASE FOR FIXED_IO INTO CE3804J.ADA AND
--                   CORRECTED EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3804i is
   Incomplete : exception;

begin

   Test
     ("CE3804I",
      "CHECK THAT FLOAT_IO GET OPERATES ON " &
      "IN_FILE FILE AND WHEN NO FILE IS " &
      "SPECIFIED THE CURRENT DEFAULT INPUT " & "FILE IS USED.");

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
         type Fl is new Float;
         package Flio is new Float_Io (Fl);
         use Flio;
         X : Fl;
      begin
         begin
            Get (Ft1, X);
            if X /= 1.0 then
               Failed ("FLOAT FILE VALUE INCORRECT");
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED - FILE FLOAT");
         end;

         begin
            Get (X);
            if X /= 2.0 then
               Failed ("FLOAT DEFAULT VALUE INCORRECT");
            end if;
         exception
            when others =>
               Failed ("EXCEPTION RAISED - DEFAULT FLOAT");
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

end Ce3804i;
