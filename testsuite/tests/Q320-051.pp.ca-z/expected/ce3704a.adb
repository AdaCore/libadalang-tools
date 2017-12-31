-- CE3704A.ADA

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
-- HISTORY:
--     CHECK THAT GET FOR INTEGER_IO CAN OPERATE ON ANY FILE OF MODE
--     IN_FILE AND THAT IF NO FILE IS SPECIFIED THE CURRENT DEFAULT
--     INPUT FILE IS USED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/01/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/09/87  REMOVED UNNECESSARY CODE, CORRECTED EXCEPTION
--                   HANDLING, AND REMOVED DEPENDENCE ON RESET.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3704a is
   Incomplete : exception;

begin

   Test
     ("CE3704A",
      "CHECK THAT GET FOR INTEGER_IO CAN OPERATE " &
      "ON ANY FILE OF MODE IN_FILE AND THAT IF " &
      "NO FILE IS SPECIFIED THE CURRENT DEFAULT " & "INPUT FILE IS USED");

   declare
      Ft  : File_Type;
      Ft2 : File_Type;
      type Ni is new Integer range 1 .. 700;
      X : Ni;
      package Iio is new Integer_Io (Ni);
      use Iio;
   begin

-- CREATE AND INITIALIZE DATA FILES

      begin
         Create (Ft, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT " & "CREATE WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED; TEXT " & "CREATE WITH OUT_FILE MODE");
            raise Incomplete;
      end;

      Put (Ft, '3');
      Put (Ft, '6');
      Put (Ft, '9');

      Close (Ft);

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

      Create (Ft2, Out_File, Legal_File_Name (2));

      Put (Ft2, '6');
      Put (Ft2, '2');
      Put (Ft2, '4');

      Close (Ft2);
      Open (Ft2, In_File, Legal_File_Name (2));

      Set_Input (Ft2);

      Get (Ft, X);

      if X /= 369 then
         Failed ("GET RETURNED WRONG VALUE; VALUE WAS" & Ni'Image (X));
      end if;

      Get (X);

      if X /= 624 then
         Failed ("GET FOR DEFAULT WAS WRONG; VALUE WAS" & Ni'Image (X));
      end if;

      begin
         Delete (Ft);
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

end Ce3704a;
