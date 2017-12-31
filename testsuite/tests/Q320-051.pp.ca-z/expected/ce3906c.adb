-- CE3906C.ADA

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
--      CHECK THAT PUT FOR ENUMERATION TYPES OUTPUTS THE ENUMERATION
--      LITERAL WITH NO TRAILING OR PRECEDING BLANKS WHEN WIDTH IS
--      NOT SPECIFIED OR IS SPECIFIED TO BE LESS THAN OR EQUAL TO THE
--      LENGTH OF THE STRING.  CHECK THAT WHEN WIDTH IS SPECIFIED TO
--      BE GREATER THAN THE LENGTH OF THE STRING, TRAILING BLANKS ARE
--      OUTPUT.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 10/08/82
--     SPS 01/03/83
--     VKG 01/07/83
--     JBG 02/22/84  CHANGED TO .ADA TEST.
--     TBN 11/10/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 09/18/87  REMOVED CALL TO CHECKFILE.  CLOSED AND REOPENED
--                   FILE AND CHECKED CONTENTS OF FILE USING
--                   ENUMERATION_IO GETS.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3906c is
   Incomplete : exception;

begin

   Test
     ("CE3906C",
      "CHECK THAT ENUMERATION_IO PUT OUTPUTS " &
      "ENUMERATION LITERALS CORRECTLY WITH AND " & "WITHOUT WIDTH PARAMETERS");

   declare
      Ft : File_Type;
      type Mood is (Angry, Happy, Bored, Sad);
      X : Mood := Bored;
      package Mood_Io is new Enumeration_Io (Mood);
      Ch : Character;
      use Mood_Io;
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

      Default_Width := Field (Ident_Int (5));

      if Default_Width /= Field (Ident_Int (5)) then
         Failed ("DEFAULT_WIDTH NOT SET CORRECTLY");
      end if;

      Put (Ft, X, 3);                             -- BORED
      X := Happy;
      New_Line (Ft);
      Put (File => Ft, Item => X, Width => 5);    -- HAPPY
      New_Line (Ft);
      Put (Ft, Sad, 5);                           -- SAD
      Default_Width := Field (Ident_Int (6));
      Put (Ft, X);                                -- HAPPY
      Put (Ft, Sad, 3);                           -- SAD
      New_Line (Ft);
      Default_Width := Field (Ident_Int (2));
      Put (Ft, Sad);                              -- SAD

      Close (Ft);

      begin
         Open (Ft, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED; TEXT OPEN FOR " & "IN_FILE MODE");
            raise Incomplete;
      end;

      Get (Ft, X);
      if X /= Bored then
         Failed ("BORED NOT READ CORRECTLY");
      end if;

      Get (Ft, X);
      if X /= Happy then
         Failed ("HAPPY NOT READ CORRECTLY - 1");
      end if;

      Skip_Line (Ft);

      Get (Ft, X);
      if X /= Sad then
         Failed ("SAD NOT READ CORRECTLY - 1");
      end if;

      Get (Ft, Ch);
      if Ch /= ' ' then
         Failed ("BLANKS NOT POSITIONED CORRECTLY - 1");
      end if;

      Get (Ft, Ch);
      if Ch /= ' ' then
         Failed ("BLANKS NOT POSITIONED CORRECTLY - 2");
      end if;

      Get (Ft, X);
      if X /= Happy then
         Failed ("HAPPY NOT READ CORRECTLY - 2");
      end if;

      Get (Ft, Ch);
      if Ch /= ' ' then
         Failed ("BLANKS NOT POSITIONED CORRECTLY - 3");
      end if;

      Get (Ft, X);
      if X /= Sad then
         Failed ("SAD NOT READ CORRECTLY - 2");
      end if;

      Skip_Line (Ft);

      Get (Ft, X);
      if X /= Sad then
         Failed ("SAD NOT READ CORRECTLY - 3");
      end if;

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

end Ce3906c;
