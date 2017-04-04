-- CE3605B.ADA

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
-- OBJECTIVE;
--     CHECK THAT PUT OUTPUTS A LINE TERMINATOR, RESETS THE COLUMN
--     NUMBER AND INCREMENTS THE LINE NUMBER WHEN THE LINE LENGTH IS
--     BOUNDED AND THE COLUMN NUMBER EQUALS THE LINE LENGTH WHEN PUT
--     IS CALLED.

-- APPLICABILITY CRITERIA:
--     THIS TEST IS APPLICABLE ONLY TO IMPLEMENTATIONS WHICH SUPPORT
--     TEXT FILES.

-- HISTORY:
--     SPS 09/02/82
--     JBG 12/28/82
--     JBG 02/22/84  CHANGED TO .ADA TEST
--     RJW 11/04/86  REVISED TEST TO OUTPUT A NOT_APPLICABLE
--                   RESULT WHEN FILES ARE NOT SUPPORTED.
--     JLH 09/08/87  GAVE FILE A NAME AND REMOVED CODE WHICH RESETS
--                   THE FILE.

with Report;  use Report;
with Text_Io; use Text_Io;
procedure Ce3605b is
   Incomplete : exception;

begin

   Test
     ("CE3605B",
      "CHECK THAT PUT PROPERLY MAINTAINS THE " &
      "LINE NUMBER AND COLUMN NUMBER WHEN THE " &
      "LINE LENGTH IS BOUNDED");

   declare
      File1  : File_Type;
      Ln_Cnt : Positive_Count;
   begin

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

      Set_Line_Length (File1, 5);
      Ln_Cnt := Line (File1);

      for I in 1 .. 5 loop
         Put (File1, 'X');
      end loop;

      if Col (File1) /= 6 then
         Failed
           ("COLUMN NUMBER NOT INCREMENTED - PUT; " &
            "VALUE WAS" &
            Count'Image (Col (File1)));
      end if;

      if Line (File1) /= Ln_Cnt then
         Failed
           ("LINE COUNT MODIFIED - PUT CHARACTER; " &
            "VALUE WAS" &
            Count'Image (Line (File1)));
      end if;

      Put (File1, 'X');
      if Col (File1) /= 2 then
         Failed
           ("COLUMN NUMBER NOT RESET - PUT CHARACTER; " &
            "VALUE WAS" &
            Count'Image (Col (File1)));
      end if;

      if Line (File1) /= Ln_Cnt + 1 then
         Failed
           ("LINE NUMBER NOT INCREMENTED - PUT CHARACTER; " &
            "VALUE WAS" &
            Count'Image (Line (File1)));
      end if;

      New_Line (File1);

      Set_Line_Length (File1, 4);
      Ln_Cnt := Line (File1);

      Put (File1, "XXXX");

      if Col (File1) /= 5 then
         Failed
           ("COLUMN NUMBER NOT INCREMENTED - PUT STRING; " &
            "VALUE WAS" &
            Count'Image (Col (File1)));
      end if;

      if Line (File1) /= Ln_Cnt then
         Failed
           ("LINE NUMBER INCREMENTED - PUT STRING; " &
            "VALUE WAS" &
            Count'Image (Line (File1)));
      end if;

      Put (File1, "STR");

      if Col (File1) /= 4 then
         Failed
           ("COLUMN NUMBER NOT SET CORRECTLY - PUT" &
            "STRING; VALUE WAS" &
            Count'Image (Col (File1)));
      end if;

      if Line (File1) /= Ln_Cnt + 1 then
         Failed
           ("LINE NUMBER NOT INCREMENTED - PUT STRING; " &
            "VALUE WAS" &
            Count'Image (Line (File1)));
      end if;

   exception
      when Incomplete =>
         null;

   end;

   Result;

end Ce3605b;
