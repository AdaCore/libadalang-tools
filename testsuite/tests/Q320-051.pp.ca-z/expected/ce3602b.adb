-- CE3602B.ADA

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
--     CHECK THAT GET (FOR CHARACTER AND STRINGS) PROPERLY SETS THE
--     PAGE, LINE, AND COLUMN NUMBERS AFTER EACH OPERATION.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 08/30/82
--     SPS 12/17/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NON_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/04/87  REMOVED DEPENDENCE ON UNBOUNDED LINE LENGTH AND
--                   CORRECTED EXCEPTION HANDLING.
--     BCB 11/13/87  GAVE SET_LINE_LENGTH PROCEDURE THE FILE VARIABLE
--                   AS A PARAMETER.  REMOVED LINE WHICH SAVED AND
--                   RESTORED THE LINE LENGTH.

with Report;  use Report;
with Text_Io; use Text_Io;
with Check_File;

procedure Ce3602b is
   Incomplete : exception;

begin

   Test
     ("CE3602B",
      "CHECK THAT GET PROPERLY SETS PAGE, LINE, AND " & "COLUMN NUMBERS");

   declare
      File1                : File_Type;
      Line1                : constant String := "LINE ONE OF TEST DATA FILE";
      Line2                : constant String := "LINE TWO";
      Line3                : constant String := "LINE THREE";
      Cn, Ln               : Positive_Count;
      Ch                   : Character;
      St                   : String (1 .. 5);
      Original_Line_Length : Count;

   begin

-- CREATE AND INITIALIZE TEST DATA FILE

      begin
         Create (File1, Out_File, Legal_File_Name);
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
            Failed ("UNEXPECTED EXCEPTION RAISED ON " & "TEXT CREATE");
            raise Incomplete;
      end;

      Original_Line_Length := Line_Length;
      Set_Line_Length (File1, Line1'Length);

      Put (File1, Line1);
      Set_Line_Length (File1, Line2'Length);
      Put (File1, Line2);
      New_Line (File1, 2);
      New_Page (File1);
      Set_Line_Length (File1, Line3'Length);
      Put (File1, Line3);
      Close (File1);

-- BEGIN TEST

      begin
         Open (File1, In_File, Legal_File_Name);
      exception
         when Use_Error =>
            Not_Applicable
              ("USE_ERROR RAISED ON TEXT OPEN " & "WITH IN_FILE MODE");
            raise Incomplete;
      end;

      if Col (File1) /= 1 then
         Failed ("COLUMN NUMBER NOT INITIALLY ONE");
      end if;

      if Line (File1) /= 1 then
         Failed ("LINE NUMBER NOT INITIALLY ONE");
      end if;

      if Page (File1) /= 1 then
         Failed ("PAGE NUMBER NOT INITIALLY ONE");
      end if;

-- TEST COLUMN NUMBER FOR CHARACTER

      Get (File1, Ch);
      if Ch /= 'L' then
         Failed ("CHARACTER NOT EQUAL TO L - 1");
      end if;
      Cn := Col (File1);
      if Cn /= 2 then
         Failed
           ("COLUMN NUMBER NOT SET CORRECTLY " &
            "- GET CHARACTER.  COL NUMBER IS" & Count'Image (Cn));
      end if;

-- TEST COLUMN NUMBER FOR STRING

      Get (File1, St);
      Cn := Col (File1);
      if Cn /= 7 then
         Failed
           ("COLUMN NUMBER NOT SET CORRECTLY " &
            "- GET STRING.  COL NUMBER IS" & Count'Image (Cn));
      end if;

-- POSITION CURRENT INDEX TO END OF LINE

      while not End_Of_Line (File1) loop
         Get (File1, Ch);
      end loop;

      if Ch /= 'E' then
         Failed ("CHARACTER NOT EQUAL TO E");
      end if;

-- TEST LINE NUMBER FOR CHARACTER

      Get (File1, Ch);
      if Ch /= 'L' then
         Failed ("CHARACTER NOT EQUAL TO L - 2");
      end if;
      Ln := Line (File1);
      if Ln /= 2 then
         Failed
           ("LINE NUMBER NOT SET CORRECTLY " &
            "- GET CHARACTER.  LINE NUMBER IS" & Count'Image (Ln));
      end if;
      if Page (File1) /= Positive_Count (Ident_Int (1)) then
         Failed
           ("PAGE NUMBER NOT CORRECT - 1.  PAGE IS" &
            Count'Image (Page (File1)));
      end if;

-- TEST LINE NUMBER FOR STRING

      while not End_Of_Line (File1) loop
         Get (File1, Ch);
      end loop;
      Get (File1, St);
      if St /= "LINE " then
         Failed ("INCORRECT VALUE READ - ST");
      end if;
      Ln := Line (File1);
      Cn := Col (File1);
      if Cn /= 6 then
         Failed
           ("COLUMN NUMBER NOT SET CORRECTLY " &
            "- GET STRING.  COL NUMBER IS" & Count'Image (Cn));
      end if;
      if Ln /= 1 then
         Failed
           ("LINE NUMBER NOT SET CORRECTLY " &
            "- GET STRING.  LINE NUMBER IS" & Count'Image (Ln));
      end if;
      if Page (File1) /= Positive_Count (Ident_Int (2)) then
         Failed
           ("PAGE NUMBER NOT CORRECT - 2.  PAGE IS" &
            Count'Image (Page (File1)));
      end if;

      begin
         Delete (File1);
      exception
         when Use_Error =>
            null;
      end;

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce3602b;
