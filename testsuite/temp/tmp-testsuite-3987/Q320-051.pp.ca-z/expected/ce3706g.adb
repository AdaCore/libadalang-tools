-- CE3706G.ADA

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
--     CHECK THAT INTEGER_IO PUT USES THE MINIMUM FIELD REQUIRED IF
--     WIDTH IS TOO SMALL AND THE LINE LENGTH IS SUFFICIENTLY LARGE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 10/05/82
--     JLH 09/17/87  COMPLETELY REVISED TEST.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3706g is

begin

   Test
     ("CE3706G",
      "CHECK THAT INTEGER_IO PUT USES THE MINIMUM " &
      "FIELD REQUIRED IF WIDTH IS TOO SMALL AND THE " &
      "LINE LENGTH IS SUFFICIENTLY LARGE");

   declare
      File : File_Type;
      package Iio is new Integer_Io (Integer);
      use Iio;
      Incomplete : exception;
      Num : Integer := 12_345;
      Ch  : Character;

   begin

      begin
         Create (File, Out_File, Legal_File_Name);
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

      Put (File, Num, Width => 3);
      Text_Io.Put (File, ' ');

      Close (File);

      begin
         Open (File, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

      Get (File, Num);
      Get (File, Ch);
      if Ch /= ' ' and Col (File) /= 7 then
         Failed
           ("INTEGER_IO PUT DOES NOT USE MINIMUM FIELD " &
            "REQUIRED WHEN WIDTH IS TOO SMALL");
      end if;

      if Num /= 12_345 then
         Failed ("INCORREC VALUE READ");
      end if;

      begin
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

end Ce3706g;
