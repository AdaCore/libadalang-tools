-- CE3806D.ADA

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
--     CHECK THAT FLOAT_IO PUT OPERATES ON FILES OF MODE OUT_FILE AND
--     IF NO FILE IS SPECIFIED THE CURRENT DEFAULT OUTPUT FILE IS USED.

--- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 10/06/82
--     VKG 02/15/83
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/14/87  REMOVED DEPENDENCE ON RESET AND CORRECT EXCEPTION
--                   HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3806d is

begin

   Test
     ("CE3806D",
      "CHECK THAT FLOAT_IO OPERATES ON FILES OF MODE " &
      "OUT_FILE AND IF NO FILE IS SPECIFIED THE " &
      "CURRENT DEFAULT OUTPUT FILE IS USED");

   declare
      Ft1, Ft2 : File_Type;
      type Fl is digits 3;
      package Flio is new Float_Io (Fl);
      use Flio;
      Incomplete : exception;
      X : Fl := -1.5;

   begin

      begin
         Create (Ft1, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON TEXT " & "CREATE WITH OUT_FILE MODE");
            raise Incomplete;
      end;

      Create (Ft2, Out_File, Legal_File_Name (2));

      Set_Output (Ft2);

      begin
         Put (Ft1, X);
         Put (X + 1.0);
         Close (Ft1);

         begin
            Open (Ft1, In_File, Legal_File_Name);
         exception
            when Use_Error =>
               Not_Applicable
                 ("USE_ERROR RAISED ON TEXT " & "OPEN WITH IN_FILE MODE");
               raise Incomplete;
         end;

         Set_Output (Standard_Output);

         Close (Ft2);
         Open (Ft2, In_File, Legal_File_Name (2));

         X := 0.0;
         Get (Ft1, X);
         if X /= -1.5 then
            Failed ("VALUE INCORRECT - FLOAT FROM FILE");
         end if;
         X := 0.0;
         Get (Ft2, X);
         if X /= -0.5 then
            Failed (" VVALUE INCORRECT - FLOAT FROM DEFAULT");
         end if;
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

end Ce3806d;
