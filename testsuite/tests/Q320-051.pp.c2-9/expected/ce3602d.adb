-- CE3602D.ADA

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
--     CHECK THAT FILES ARE OF MODE IN_FILE AND THAT WHEN NO FILE IS
--     SPECIFIED THAT CURRENT DEFAULT INPUT FILE IS USED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 10/06/82
--     SPS 12/17/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  REMOVED DEPENDENCE ON RESET AND CORRECTED
--                   EXCEPTION HANDLING.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3602d is
   Incomplete : exception;

begin

   Test
     ("CE3602D",
      "CHECK THAT GET FOR STRINGS AND CHARACTERS " &
      "OPERATES ON IN_FILE FILES");

   declare
      Ft, File : File_Type;
      X        : Character;
      St       : String (1 .. 3);
   begin

-- CREATE AND INITIALIZE FILES

      begin
         Create (Ft, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON TEXT " & "CREATE WITH OUT_FILE MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON TEXT " & "CREATE");
            raise Incomplete;
      end;

      Put (Ft, "ABCE");
      New_Line (Ft);
      Put (Ft, "EFGHIJKLM");

      Close (Ft);

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT OPEN " & "WITH IN_FILE MODE - 1");
            raise Incomplete;
      end;

      Create (File, Out_File, Legal_File_Name (2));

      Put (File, "STRING");
      New_Line (File);
      Put (File, "END OF OUTPUT");

      Close (File);

      Open (File, In_File, Legal_File_Name (2));

      Set_Input (File);

-- BEGIN TEST

      Get (Ft, X);
      if X /= Ident_Char ('A') then
         Failed ("CHARACTER FROM FILE INCORRECT, WAS '" & X & "'");
      end if;

      Get (Ft, St);
      if St /= "BCE" then
         Failed ("STRING FROM FILE INCORRECT; WAS """ & St & """");
      end if;

      Get (X);
      if X /= Ident_Char ('S') then
         Failed ("CHARACTER FROM DEFAULT INCORRECT; WAS '" & X & "'");
      end if;

      Get (St);
      if St /= "TRI" then
         Failed ("STRING FROM DEFAULT INCORRECT; WAS """ & St & """");
      end if;

      begin
         Delete (Ft);
         Delete (File);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce3602d;
