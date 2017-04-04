-- CE2205A.ADA

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
--     CHECK WHETHER  READ  FOR A SEQUENTIAL FILE RAISES  DATA_ERROR OR
--     CONSTRAINT_ERROR WHEN AN ELEMENT IS READ THAT IS OUTSIDE THE
--     RANGE OF THE ITEM TYPE BUT WITHIN THE RANGE OF THE INSTANTIATED
--     TYPE, AND CHECK THAT READING CAN CONTINUE AFTER THE EXCEPTION
--     HAS BEEN HANDLED.

--          A) CHECK ENUMERATION TYPE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT SEQUENTIAL FILES.

-- HISTORY:
--     SPS 09/28/82
--     JBG 06/04/84
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     GMT 07/24/87  RENAMED FROM CE2210A.ADA AND REMOVED THE USE OF
--                   RESET.
--     PWB 05/18/89  DELETED CALL TO FAILED WHEN NO EXCEPTION RAISED.

with Report; use Report;
with Sequential_Io;

procedure Ce2205a is
begin

   Test
     ("CE2205A",
      "CHECK WHETHER READ FOR A SEQUENTIAL FILE " &
      "RAISES  DATA_ERROR OR CONSTRAINT_ERROR WHEN " &
      "AN ELEMENT IS READ THAT IS OUTSIDE THE RANGE " &
      "OF THE ITEM TYPE BUT WITHIN THE RANGE OF THE " &
      "INSTANTIATED TYPE, AND CHECK THAT READING CAN " &
      "CONTINUE AFTER THE EXCEPTION HAS BEEN HANDLED");
   declare
      package Seq is new Sequential_Io (Character);
      use Seq;
      Ft : File_Type;
      subtype Ch is Character range 'A' .. 'D';
      X : Ch;
      Incomplete : exception;
   begin
      begin
         Create (Ft, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON SEQUENTIAL " &
               "CREATE WITH OUT_FILE MODE - 1");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON SEQUENTIAL " &
               "CREATE WITH OUT_FILE MODE - 2");
            raise Incomplete;
         when others =>
            Failed
              ("UNEXPECTED EXCEPTION RAISED ON " & "SEQUENTIAL CREATE - 3");
            raise Incomplete;
      end;

      Write (Ft, 'A');
      Write (Ft, 'M');
      Write (Ft, 'B');
      Write (Ft, 'C');

      Close (Ft);

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("OPEN WITH IN_FILE MODE IS NOT " & "SUPPORTED - 4");
            raise Incomplete;
      end;

      -- BEGIN TEST

      Read (Ft, X);
      if X /= 'A' then
         Failed ("INCORRECT VALUE FOR READ - 5");
      end if;

      begin
         Read (Ft, X);
         Comment
           ("NO EXCEPTION RAISED FOR READ WITH ELEMENT " & "OUT OF RANGE");
      exception
         when Constraint_Error =>
            Comment ("CONSTRAINT_ERROR RAISED FOR SCALAR " & "TYPES - 7");
         when Data_Error =>
            Comment ("DATA_ERROR RAISED FOR SCALAR TYPES - 8");
         when others =>
            Failed ("WRONG EXCEPTION RAISED - 9");
      end;

      begin
         Read (Ft, X);
         if X /= 'B' then
            Failed ("INCORRECT VALUE FOR READ - 10");
         end if;

         Read (Ft, X);
         if X /= 'C' then
            Failed ("INCORRECT VALUE FOR READ - 11");
         end if;
      exception
         when others =>
            Failed ("UNABLE TO CONTINUE READING - 12");
            raise Incomplete;
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

end Ce2205a;
