-- CE3804M.ADA

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
--     CHECK THAT GET WILL RAISE DATA_ERROR IF THE USE OF # AND :
--     IN BASED LITERALS IS MIXED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILES.

-- HISTORY:
--     VKG 02/07/83
--     JBG 03/30/84
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/14/87  SPLIT CASE FOR FIXED_IO INTO CE3804N.ADA AND
--                   CORRECTED EXCEPTION HANDLING.

with Text_Io; use Text_Io;
with Report;  use Report;

procedure Ce3804m is

   Incomplete : exception;

begin
   Test
     ("CE3804M",
      "CHECK THAT FLOAT_IO GET WILL RAISE " &
      "DATA_ERROR IF THE USE OF # AND : IN " &
      "BASED LITERALS IS MIXED");

   declare
      Ft : File_Type;
   begin

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

      Put_Line (Ft, "2#1.1#E+2");   -- 2#1.1#E+2
      Put_Line (Ft, "8:1.1:E-2");   -- 8:1.1:E-2
      Put (Ft, "2#1.1:E+1");        -- 2#1.1:E+1
      New_Line (Ft);
      Put (Ft, "4:2.23#E+2");       -- 4:2.23#E+2
      New_Line (Ft);
      Put (Ft, "2#1.0#E+1");        -- 2#1.0#E+1
      New_Line (Ft);
      Close (Ft);

      declare
         package Fl_Io is new Float_Io (Float);
         use Fl_Io;
         X : Float := 1.00E+10;
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
         if X /= 2#1.1#E+2 then
            Failed ("DID NOT GET RIGHT VALUE - 1");
         end if;

         Get (Ft, X);
         if X /= 8#1.1#E-2 then
            Failed ("DID NOT GET RIGHT VALUE - 2");
         end if;

         begin
            X := 1.0E+10;
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - 1");
         exception
            when Data_Error =>
               if X /= 1.00E+10 then
                  Failed
                    ("ACTUAL PARAMETER TO GET " &
                     "AFFECTED ON DATA_ERROR - 1");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 1");
         end;

         Skip_Line (Ft);

         begin
            Get (Ft, X);
            Failed ("DATA_ERROR NOT RAISED - 2");
         exception
            when Data_Error =>
               if X /= 1.00E+10 then
                  Failed
                    ("ACTUAL PARAMETER TO GET " &
                     "AFFECTED ON DATA_ERROR - 2");
               end if;
            when others =>
               Failed ("WRONG EXCEPTION RAISED - 2");
         end;

         Skip_Line (Ft);

         Get (Ft, X);
         if X /= 2#1.0#E+1 then
            Failed ("DID NOT GET RIGHT VALUE - 3");
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

end Ce3804m;
