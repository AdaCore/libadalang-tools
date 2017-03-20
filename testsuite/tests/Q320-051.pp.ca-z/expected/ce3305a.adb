-- CE3305A.ADA

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
--     CHECK THAT THE LINE AND PAGE LENGTHS MAY BE ALTERED DYNAMICALLY
--     SEVERAL TIMES.  CHECK THAT WHEN RESET TO ZERO, THE LENGTHS ARE
--     UNBOUNDED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES WITH UNBOUNDED LINE LENGTH.

-- HISTORY:
--     SPS 09/28/82
--     EG  05/22/85
--     DWC 08/18/87  ADDED CHECK_FILE WITHOUT A'S.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;

procedure Ce3305a is

   Incomplete : exception;

begin

   Test
     ("CE3305A",
      "CHECK THAT LINE AND PAGE LENGTHS MAY BE " & "ALTERED DYNAMICALLY");

   declare
      Ft : File_Type;

      procedure Put_Chars (Cnt : Integer; Ch : Character) is
      begin
         for I in 1 .. Cnt loop
            Put (Ft, Ch);
         end loop;
      end Put_Chars;

   begin

      begin
         Create (Ft, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable
              ("NAME_ERROR RAISED ON CREATE " & "WITH OUT_FILE MODE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED ON CREATE");
            raise Incomplete;
      end;

      Set_Line_Length (Ft, 10);
      Set_Page_Length (Ft, 5);

      Put_Chars (150, 'X');         -- 15 LINES

      begin
         Set_Line_Length (Ft, 5);
         Set_Page_Length (Ft, 10);
      exception
         when others =>
            Failed ("UNABLE TO CHANGE LINE OR PAGE LENGTH");
      end;

      Put_Chars (50, 'B');               -- 10 LINES

      begin
         Set_Line_Length (Ft, 25);
         Set_Page_Length (Ft, 4);
      exception
         when others =>
            Failed ("UNABLE TO CHANGE LINE OR PAGE LENGTH - 2");
      end;

      Put_Chars (310, 'K');              -- 12 LINES, 10 CHARACTERS

--  THIS CAN RAISE USE_ERROR IF AN IMPLEMENTATION REQUIRES A BOUNDED
--  LINE LENGTH FOR AN EXTERNAL FILE.

      begin
         begin
            Set_Line_Length (Ft, Unbounded);
            Set_Page_Length (Ft, Unbounded);
         exception
            when Use_Error =>
               Not_Applicable ("BOUNDED LINE LENGTH " & "REQUIRED");
               raise Incomplete;
         end;

         Put_Chars (100, 'A');              -- ONE LINE

         Check_File
           (Ft,
            "XXXXXXXXXX#" &
            "XXXXXXXXXX#" &
            "XXXXXXXXXX#" &
            "XXXXXXXXXX#" &
            "XXXXXXXXXX#@" &
            "XXXXXXXXXX#" &
            "XXXXXXXXXX#" &
            "XXXXXXXXXX#" &
            "XXXXXXXXXX#" &
            "XXXXXXXXXX#@" &
            "XXXXXXXXXX#" &
            "XXXXXXXXXX#" &
            "XXXXXXXXXX#" &
            "XXXXXXXXXX#" &
            "XXXXXXXXXX#" &
            "BBBBB#" &
            "BBBBB#" &
            "BBBBB#" &
            "BBBBB#" &
            "BBBBB#@" &
            "BBBBB#" &
            "BBBBB#" &
            "BBBBB#" &
            "BBBBB#" &
            "BBBBBKKKKKKKKKKKKKKKKKKKK#@" &
            "KKKKKKKKKKKKKKKKKKKKKKKKK#" &
            "KKKKKKKKKKKKKKKKKKKKKKKKK#" &
            "KKKKKKKKKKKKKKKKKKKKKKKKK#" &
            "KKKKKKKKKKKKKKKKKKKKKKKKK#@" &
            "KKKKKKKKKKKKKKKKKKKKKKKKK#" &
            "KKKKKKKKKKKKKKKKKKKKKKKKK#" &
            "KKKKKKKKKKKKKKKKKKKKKKKKK#" &
            "KKKKKKKKKKKKKKKKKKKKKKKKK#@" &
            "KKKKKKKKKKKKKKKKKKKKKKKKK#" &
            "KKKKKKKKKKKKKKKKKKKKKKKKK#" &
            "KKKKKKKKKKKKKKKKKKKKKKKKK#" &
            "KKKKKKKKKKKKKKKAAAAAAAAAAA" &
            "AAAAAAAAAAAAAAAAAAAAAAAAAA" &
            "AAAAAAAAAAAAAAAAAAAAAAAAAA" &
            "AAAAAAAAAAAAAAAAAAAAAAAAAA" &
            "AAAAAAAAAAA#@%");

      exception
         when Incomplete =>
            null;
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

end Ce3305a;
