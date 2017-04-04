-- CE3704E.ADA

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
--     CHECK THAT INTEGER_IO GET RAISES DATA_ERROR WHEN THE LEXICAL
--     ELEMENT IS NOT OF THE INTEGER TYPE EXPECTED.  CHECK THAT ITEM
--     IS UNAFFECTED AND READING CAN CONTINUE AFTER THE EXCEPTION
--     HAS BEEN HANDLED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 10/04/82
--     VKG 01/14/83
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/10/87  REMOVED UNNECCESSARY CODE, CORRECTED EXCEPTION
--                   HANDLING, AND CHECKED FOR USE_ERROR ON DELETE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3704e is
   Incomplete : exception;

begin

   Test
     ("CE3704E",
      "CHECK THAT INTEGER_IO GET RAISES DATA_ERROR " &
      "WHEN THE LEXICAL ELEMENT IS NOT OF THE " &
      "INTEGER TYPE EXPECTED.  CHECK THAT ITEM " &
      "IS UNAFFECTED AND READING CAN CONTINUE AFTER " &
      "THE EXCEPTION HAS BEEN HANDLED");

   declare
      Ft : File_Type;
      type Int is new Integer range 10 .. 20;
      package Iio is new Integer_Io (Int);
      use Iio;
      X : Int := 16;
   begin

-- CREATE AND INITIALIZE FILE

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

      Put (Ft, " 101 12");
      Close (Ft);

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

      begin
         Get (Ft, X, 2);
         Failed ("DATA_ERROR NOT RAISED - 1");
      exception
         when Data_Error =>
            if X /= 16 then
               Failed ("ITEM AFFECTED BY GET WHEN DATA" & "_ERROR IS RAISED");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 1");
      end;

      begin
         Get (Ft, X, 3);
         Failed ("DATA_ERROR NOT RAISED - 2");
      exception
         when Data_Error =>
            if X /= 16 then
               Failed ("ITEM AFFECTED BY GET WHEN DATA" & "_ERROR IS RAISED");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 2");
      end;

      begin
         Get (Ft, X, 2);
         if X /= 12 then
            Failed ("READING NOT CONTINUED CORRECTLY " & "AFTER EXCEPTION");
         end if;
      exception
         when others =>
            Failed ("GET OF CORRECT DATA RAISED EXCEPTION");
      end;

      begin
         Delete (Ft);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;
   end;

   Result;

end Ce3704e;
