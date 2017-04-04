-- CE3103A.ADA

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
--     CHECK THAT THE PAGE AND LINE LENGTH OF TEXT FILES ARE ZERO
--     AFTER A CREATE, OPEN, OR RESET TO OUT_FILE.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH
--     SUPPORT TEXT FILE.

-- HISTORY:
--     ABW 08/24/82
--     SPS 09/16/82
--     SPS 11/09/82
--     SPS 01/18/83
--     EG  11/02/84
--     TBN 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     DWC 08/13/87  REVISED TEST TO INCLUDE CASES TO RESET THE FILE.

with Report;  use Report;
with Text_Io; use Text_Io;

procedure Ce3103a is

   Subtest    : exception;
   Incomplete : exception;
   File : File_Type;
   Zero : constant Count := Count (Ident_Int (0));
   Two  : constant Count := Count (Ident_Int (2));
   Five : constant Count := Count (Ident_Int (5));

begin

   Test
     ("CE3103A",
      "CHECK THAT PAGE AND LINE LENGTH " &
      "ARE SET TO ZERO AFTER CREATE, " &
      "OPEN, OR RESET");

   begin

      begin
         Create (File, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED; TEXT CREATE");
            raise Incomplete;
         when Name_Error =>
            Not_Applicable ("NAME_ERROR RAISED; TEXT CREATE");
            raise Incomplete;
         when others =>
            Failed ("UNEXPECTED EXCEPTION RAISED; TEXT CREATE");
            raise Incomplete;
      end;

      if Line_Length (File) /= Zero then
         Failed ("LINE_LENGTH FOR CREATE IS NOT ZERO");
      end if;
      if Page_Length (File) /= Zero then
         Failed ("PAGE_LENGTH FOR CREATE IS NOT ZERO");
      end if;

      Set_Line_Length (File, Two);
      Set_Page_Length (File, Five);

      Put_Line (File, "HI");

      Close (File);

      begin
         Open (File, Out_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable ("USE_ERROR RAISED; TEXT OPEN");
            raise Incomplete;
      end;

      if Line_Length (File) /= Zero then
         Failed ("LINE_LENGTH FOR OPEN IS NOT ZERO");
      end if;
      if Page_Length (File) /= Zero then
         Failed ("PAGE_LENGTH FOR OPEN IS NOT ZERO");
      end if;

      Set_Line_Length (File, Two);
      Set_Page_Length (File, Two);

      Put_Line (File, "HI");

      begin
         begin
            Reset (File, Out_File);
         exception
            when Use_Error =>
               raise Subtest;
         end;

         if Line_Length (File) /= Zero then
            Failed ("LINE_LENGTH FOR RESET TO OUT_FILE IS NOT " & "ZERO - 1");
         end if;
         if Page_Length (File) /= Zero then
            Failed ("PAGE_LENGTH FOR RESET TO OUT_FILE IS NOT " & "ZERO - 1");
         end if;
      exception
         when Subtest =>
            null;
      end;

      Set_Line_Length (File, Five);
      Set_Page_Length (File, Five);

      Put_Line (File, "HELLO");

      if Line_Length (File) /= 5 then
         Failed
           ("LINE_LENGTH FOR RESET IN OUT_FILE, PLUS HELLO " & "IS NOT FIVE");
      end if;
      if Page_Length (File) /= 5 then
         Failed
           ("PAGE_LENGTH FOR RESET IN OUT_FILE, PLUS HELLO " & "IS NOT FIVE");
      end if;

      begin
         begin
            Reset (File);
         exception
            when Use_Error =>
               raise Subtest;
         end;

         if Line_Length (File) /= Zero then
            Failed ("LINE_LENGTH FOR RESET IS NOT ZERO");
         end if;
         if Page_Length (File) /= Zero then
            Failed ("PAGE_LENGTH FOR RESET IS NOT ZERO");
         end if;
      exception
         when Subtest =>
            null;
      end;

      Set_Line_Length (File, Five);
      Set_Page_Length (File, Five);

      Put_Line (File, "HELLO");

      if Line_Length (File) /= 5 then
         Failed ("LINE_LENGTH FOR RESET PLUS HELLO");
      end if;
      if Page_Length (File) /= 5 then
         Failed ("PAGE_LENGTH FOR RESET PLUS HELLO");
      end if;

      Close (File);

      begin
         Open (File, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            raise Incomplete;
      end;

      begin
         Reset (File, Out_File);
      exception
         when Use_Error =>
            raise Incomplete;
      end;

      if Line_Length (File) /= Zero then
         Failed ("LINE_LENGTH FOR RESET TO OUT_FILE IS NOT ZERO - 2");
      end if;
      if Page_Length (File) /= Zero then
         Failed ("PAGE_LENGTH FOR RESET TO OUT_FILE IS NOT ZERO - 2");
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

end Ce3103a;
