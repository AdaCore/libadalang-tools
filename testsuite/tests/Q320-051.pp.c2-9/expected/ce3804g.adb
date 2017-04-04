-- CE3804G.ADA

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
--     CHECK THAT FLOAT_IO GET WHEN SUPPLIED WITH A WIDTH PARAMETER
--     GREATER THAN ZERO READS ONLY THAT MANY CHARACTERS.  ALSO CHECK
--     THAT INPUT TERMINATES WHEN A LINE TERMINATOR IS ENCOUNTERED AND
--     THAT DATA_ERROR IS RAISED WHEN THE DATA IS INVALID.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 09/08/82
--     SPS 12/14/82
--     VKG 01/13/83
--     SPS 02/08/83
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/14/87  SPLIT CASE FOR FIXED_IO INTO CE3804H.ADA AND
--                   CORRECTED EXCEPTION HANDLING.
--     LDC 06/01/88  CHANGED TEST VALUE FROM "3.525" TO "3.625".

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3804g is
   Incomplete : exception;

begin

   Test
     ("CE3804G",
      "CHECK THAT FLOAT_IO GET WHEN SUPPLIED WITH " &
      "A WIDTH PARAMETER GREATER THAN ZERO READS " &
      "ONLY THAT MANY CHARACTERS.  ALSO CHECK THAT " &
      "INPUT TERMINATES WHEN A LINE TERMINATOR IS " &
      "ENCOUNTERED AND THAT DATA_ERROR IS RAISED " &
      "WHEN THE DATA IS INVALID.");

   declare
      Ft : File_Type;
   begin

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

      Put (Ft, "3.259.5 8.52");
      New_Line (Ft);
      Put (Ft, "  ");
      New_Line (Ft);
      Put (Ft, Ascii.Ht & "9.0");
      New_Line (Ft);
      Put (Ft, "-3.625");
      New_Line (Ft);
      Close (Ft);

-- BEGIN TEST

      declare
         type Fl is digits 4;
         package Fl_Io is new Float_Io (Fl);
         use Fl_Io;
         X : Fl;
      begin

         begin
            Open (Ft, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable
                 ("USE_ERROR RAISED; TEXT" & "OPEN WITH IN_FILE MODE");
               raise Incomplete;
         end;

         Get (Ft, X, 4);
         if X /= 3.25 then
            Failed ("WIDTH CHARACTERS NOT READ - FLOAT");
         else
            Get (Ft, X, 3);
            if X /= 9.5 then
               Failed ("WIDTH CHARACTERS NOT READ - " & "FLOAT 2");
            else
               Get (Ft, X, 4);
               if X /= 8.5 then
                  Failed ("DIDN'T COUNT LEADING BLANKS " & "- FLOAT");
               else
                  Skip_Line (Ft);
                  begin
                     Get (Ft, X, 2);
                     Failed ("DATA_ERROR NOT RAISED - " & "FLOAT");
                  exception
                     when Data_Error =>
                        null;
                     when others =>
                        Failed ("WRONG EXCEPTION RAISED" & " - FLOAT");
                  end;
                  Skip_Line (Ft);
                  Get (Ft, X, 4);
                  if X /= 9.0 then
                     Failed ("GET WITH WIDTH " & "INCORRECT - 3");
                  end if;

                  Skip_Line (Ft);
                  Get (Ft, X, 7);
                  if X /= -3.625 then
                     Failed ("WIDTH CHARACTERS NOT " & "READ - FLOAT 3");
                  end if;
               end if;
            end if;
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

end Ce3804g;
