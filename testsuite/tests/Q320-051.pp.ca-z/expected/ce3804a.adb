-- CE3804A.ADA

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
--     CHECK THAT GET FOR FLOAT_IO READS A PLUS OR MINUS SIGN
--     IF PRESENT.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS ONLY APPLICABLE TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     SPS 09/07/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/11/87  CORRECTED EXCEPTION HANDLING AND REVISED IFS
--                   TO CHECK FOR CASE WHEN VALUE IS NEGATIVE OF WHAT
--                   IS EXPECTED.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3804a is
   Incomplete : exception;

begin

   Test
     ("CE3804A",
      "CHECK THAT GET FOR FLOAT_IO READS A PLUS OR " &
      "MINUS SIGN IF PRESENT");

   declare
      Ft : File_Type;
      type Fl is new Float range -3.0 .. 3.0;
      X   : Fl;
      St1 : constant String := Ident_Str ("-3.0");
      St2 : constant String := Ident_Str ("+2.0");
      St3 : constant String := Ident_Str ("1.0");
   begin

-- CREATE AND INITIALIZE DATA FILE

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

      Put (Ft, St1);
      New_Line (Ft);
      Put (Ft, St2);
      New_Line (Ft);
      Put (Ft, St3);
      New_Line (Ft);
      Close (Ft);

-- BEGIN TEST

      declare
         package Fl_Io is new Float_Io (Fl);
         use Fl_Io;
         Lst : Positive;
      begin

         begin
            Open (Ft, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable
                 ("USE_ERROR RAISED; TEXT " & "OPEN WITH IN_FILE MODE");
               raise Incomplete;
         end;

         Get (Ft, X);
         if X = 3.0 then
            Failed ("MINUS SIGN NOT READ - 1");
         elsif X /= -3.0 then
            Failed ("INCORRECT VALUE READ - 1");
         end if;

         Get (Ft, X);
         if X = -2.0 then
            Failed ("PLUS SIGN NOT READ - 2");
         elsif X /= +2.0 then
            Failed ("INCORRECT VALUE READ - 2");
         end if;

         Get (Ft, X);
         if X /= 1.0 then
            Failed ("INCORRECT VALUE READ - 3");
         end if;

         Get (St1, X, Lst);
         if X = 3.0 then
            Failed ("MINUS SIGN NOT READ - 4");
         elsif X /= -3.0 then
            Failed ("INCORRECT VALUE READ - 4");
         end if;

         Get (St2, X, Lst);
         if X = -2.0 then
            Failed ("PLUS SIGN NOT READ - 5");
         elsif X /= +2.0 then
            Failed ("INCORRECT VALUE READ - 5");
         end if;

         Get (St3, X, Lst);
         if X /= 1.0 then
            Failed ("INCORRECT VALUE READ - 6");
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

end Ce3804a;
