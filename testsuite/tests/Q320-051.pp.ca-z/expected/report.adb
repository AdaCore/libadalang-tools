-- REPBODY.ADA
--
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
--
-- HISTORY:
--      DCB 04/27/80
--      JRK 6/10/80
--      JRK 11/12/80
--      JRK 8/6/81
--      JRK 10/27/82
--      JRK 6/1/84
--      JRK 11/18/85  ADDED PRAGMA ELABORATE.
--      PWB 07/29/87  ADDED STATUS ACTION_REQUIRED AND
--                    PROCEDURE SPECIAL_ACTION.
--      TBN 08/20/87  ADDED FUNCTION LEGAL_FILE_NAME.
--      BCB 05/17/90  MODIFIED TO ALLOW OUTPUT TO DIRECT_IO FILE.
--                    ADDED TIME-STAMP.
--      LDC 05/17/90  REMOVED OUTPUT TO DIRECT_IO FILE.
--      WMC 08/11/92  UPDATED ACVC VERSION STRING TO "9X BASIC".
--      DTN 07/05/92  UPDATED ACVC VERSION STRING TO
--                    "ACVC 2.0 JULY 6 1993 DRAFT".
--      WMC 01/24/94  MODIFIED LEGAL_FILE_NAME TO ALLOW FIVE POSSIBLE
--                    FILE NAMES (INCREASED RANGE OF TYPE FILE_NUM TO 1..5).
--      WMC 11/06/94  UPDATED ACVC VERSION STRING TO
--                    "ACVC 2.0 NOVEMBER 6 1994 DRAFT".
--      DTN 12/04/94  UPDATED ACVC VERSION STRING TO
--                    "ACVC 2.0".
--      KAS 06/19/95  ADDED FUNCTION IDENT_WIDE_CHAR.
--      KAS 06/19/95  ADDED FUNCTION IDENT_WIDE_STR.
--      DTN 11/21/95  UPDATED ACVC VERSION STRING TO
--                    "ACVC 2.0.1".
--      DTN 12/14/95  UPDATED ACVC VERSION STRING TO
--                    "ACVC 2.1".
--      EDS 12/17/97  UPDATED ACVC VERSION STRING TO
--                    "2.2".
--      RLB  3/16/00  UPDATED ACATS VERSION STRING TO "2.3".
--                    CHANGED VARIOUS STRINGS TO READ "ACATS".
--      RLB  3/22/01  UPDATED ACATS VERSION STRING TO "2.4".
--      RLB  3/29/02  UPDATED ACATS VERSION STRING TO "2.5".
--      RLB  3/06/07  UPDATED ACATS VERSION STRING TO "2.6".
--      RLB  3/22/07  UPDATED ACATS VERSION STRING TO "3.0".
--      RLB  1/20/14  UPDATED ACATS VERSION STRING TO "3.1".
--      RLB  2/28/14  UPDATED ACATS VERSION STRING TO "4.0".

with Text_Io, Calendar;
use Text_Io, Calendar;
pragma Elaborate (Text_Io, Calendar);

package body Report is

   type Status is (Pass, Fail, Does_Not_Apply, Action_Required, Unknown);

   type Time_Integer is range 0 .. 86_400;

   Test_Status : Status := Fail;

   Max_Name_Len : constant := 15;     -- MAXIMUM TEST NAME LENGTH.
   Test_Name    : String (1 .. Max_Name_Len);

   No_Name       : constant String (1 .. 7)        := "NO_NAME";
   Test_Name_Len : Integer range 0 .. Max_Name_Len := 0;

   Acats_Version : constant String := "4.0";
   -- VERSION OF ACATS BEING RUN (X.XX).

   procedure Put_Msg (Msg : String) is
      -- WRITE MESSAGE. LONG MESSAGES ARE FOLDED (AND INDENTED).
      Max_Len : constant Integer range 50 .. 150 := 72;  -- MAXIMUM
      -- OUTPUT LINE LENGTH.
      Indent : constant Integer := Test_Name_Len + 9;  -- AMOUNT TO
      -- INDENT CONTINUATION LINES.
      I : Integer := 0;             -- CURRENT INDENTATION.
      M : Integer := Msg'First;     -- START OF MESSAGE SLICE.
      N : Integer;                  -- END OF MESSAGE SLICE.
   begin
      loop
         if I + (Msg'Last - M + 1) > Max_Len then
            N := M + (Max_Len - I) - 1;
            if Msg (N) /= ' ' then
               while N >= M and then Msg (N + 1) /= ' ' loop
                  N := N - 1;
               end loop;
               if N < M then
                  N := M + (Max_Len - I) - 1;
               end if;
            end if;
         else
            N := Msg'Last;
         end if;
         Set_Col (Standard_Output, Text_Io.Count (I + 1));
         Put_Line (Standard_Output, Msg (M .. N));
         I := Indent;
         M := N + 1;
         while M <= Msg'Last and then Msg (M) = ' ' loop
            M := M + 1;
         end loop;
         exit when M > Msg'Last;
      end loop;
   end Put_Msg;

   function Time_Stamp return String is
      Time_Now                               : Calendar.Time;
      Year, Month, Day, Hour, Minute, Second : Time_Integer := 1;

      function Convert (Number : Time_Integer) return String is
         Str       : String (1 .. 2) := (others => '0');
         Dec_Digit : constant String := "0123456789";
         Num       : Time_Integer    := Number;
      begin
         if Num = 0 then
            return Str;
         else
            Num     := Num mod 100;
            Str (2) := Dec_Digit (Integer (Num mod 10 + 1));
            Num     := Num / 10;
            Str (1) := Dec_Digit (Integer (Num + 1));
            return Str;
         end if;
      end Convert;
   begin
      Time_Now := Calendar.Clock;
      Split
        (Time_Now, Year_Number (Year), Month_Number (Month), Day_Number (Day),
         Day_Duration (Second));
      Hour   := Second / 3_600;
      Second := Second mod 3_600;
      Minute := Second / 60;
      Second := Second mod 60;
      return
        (Convert (Time_Integer (Year)) & "-" & Convert (Time_Integer (Month)) &
         "-" & Convert (Time_Integer (Day)) & " " &
         Convert (Time_Integer (Hour)) & ":" &
         Convert (Time_Integer (Minute)) & ":" &
         Convert (Time_Integer (Second)));
   end Time_Stamp;

   procedure Test (Name : String; Descr : String) is
   begin
      Test_Status := Pass;
      if Name'Length <= Max_Name_Len then
         Test_Name_Len := Name'Length;
      else
         Test_Name_Len := Max_Name_Len;
      end if;
      Test_Name (1 .. Test_Name_Len) :=
        Name (Name'First .. Name'First + Test_Name_Len - 1);

      Put_Msg ("");
      Put_Msg
        (",.,. " & Test_Name (1 .. Test_Name_Len) & " " & "ACATS " &
         Acats_Version & " " & Time_Stamp);
      Put_Msg ("---- " & Test_Name (1 .. Test_Name_Len) & " " & Descr & ".");
   end Test;

   procedure Comment (Descr : String) is
   begin
      Put_Msg ("   - " & Test_Name (1 .. Test_Name_Len) & " " & Descr & ".");
   end Comment;

   procedure Failed (Descr : String) is
   begin
      Test_Status := Fail;
      Put_Msg ("   * " & Test_Name (1 .. Test_Name_Len) & " " & Descr & ".");
   end Failed;

   procedure Not_Applicable (Descr : String) is
   begin
      if Test_Status = Pass or Test_Status = Action_Required then
         Test_Status := Does_Not_Apply;
      end if;
      Put_Msg ("   + " & Test_Name (1 .. Test_Name_Len) & " " & Descr & ".");
   end Not_Applicable;

   procedure Special_Action (Descr : String) is
   begin
      if Test_Status = Pass then
         Test_Status := Action_Required;
      end if;
      Put_Msg ("   ! " & Test_Name (1 .. Test_Name_Len) & " " & Descr & ".");
   end Special_Action;

   procedure Result is
   begin
      case Test_Status is
         when Pass =>
            Put_Msg
              ("==== " & Test_Name (1 .. Test_Name_Len) &
               " PASSED ============================.");
         when Does_Not_Apply =>
            Put_Msg
              ("++++ " & Test_Name (1 .. Test_Name_Len) &
               " NOT-APPLICABLE ++++++++++++++++++++.");
         when Action_Required =>
            Put_Msg
              ("!!!! " & Test_Name (1 .. Test_Name_Len) &
               " TENTATIVELY PASSED !!!!!!!!!!!!!!!!.");
            Put_Msg
              ("!!!! " & (1 .. Test_Name_Len => ' ') &
               " SEE '!' COMMENTS FOR SPECIAL NOTES!!");
         when others =>
            Put_Msg
              ("**** " & Test_Name (1 .. Test_Name_Len) &
               " FAILED ****************************.");
      end case;
      Test_Status                    := Fail;
      Test_Name_Len                  := No_Name'Length;
      Test_Name (1 .. Test_Name_Len) := No_Name;
   end Result;

   function Ident_Int (X : Integer) return Integer is
   begin
      if Equal (X, X) then          -- ALWAYS EQUAL.
         return X;                -- ALWAYS EXECUTED.
      end if;
      return 0;                     -- NEVER EXECUTED.
   end Ident_Int;

   function Ident_Char (X : Character) return Character is
   begin
      if Equal (Character'Pos (X), Character'Pos (X)) then  -- ALWAYS
         -- EQUAL.
         return X;                -- ALWAYS EXECUTED.
      end if;
      return '0';                   -- NEVER EXECUTED.
   end Ident_Char;

   function Ident_Wide_Char (X : Wide_Character) return Wide_Character is
   begin
      if Equal (Wide_Character'Pos (X), Wide_Character'Pos (X)) then
         -- ALWAYS EQUAL.
         return X;                -- ALWAYS EXECUTED.
      end if;
      return '0';                   -- NEVER EXECUTED.
   end Ident_Wide_Char;

   function Ident_Bool (X : Boolean) return Boolean is
   begin
      if Equal (Boolean'Pos (X), Boolean'Pos (X)) then  -- ALWAYS
         -- EQUAL.
         return X;                -- ALWAYS EXECUTED.
      end if;
      return False;                 -- NEVER EXECUTED.
   end Ident_Bool;

   function Ident_Str (X : String) return String is
   begin
      if Equal (X'Length, X'Length) then  -- ALWAYS EQUAL.
         return X;                -- ALWAYS EXECUTED.
      end if;
      return "";                    -- NEVER EXECUTED.
   end Ident_Str;

   function Ident_Wide_Str (X : Wide_String) return Wide_String is
   begin
      if Equal (X'Length, X'Length) then  -- ALWAYS EQUAL.
         return X;                -- ALWAYS EXECUTED.
      end if;
      return "";                    -- NEVER EXECUTED.
   end Ident_Wide_Str;

   function Equal (X, Y : Integer) return Boolean is
      Rec_Limit : constant Integer range 1 .. 100 := 3;  -- RECURSION
      -- LIMIT.
      Z : Boolean;                  -- RESULT.
   begin
      if X < 0 then
         if Y < 0 then
            Z := Equal (-X, -Y);
         else
            Z := False;
         end if;
      elsif X > Rec_Limit then
         Z := Equal (Rec_Limit, Y - X + Rec_Limit);
      elsif X > 0 then
         Z := Equal (X - 1, Y - 1);
      else
         Z := Y = 0;
      end if;
      return Z;
   exception
      when others =>
         return X = Y;
   end Equal;

   function Legal_File_Name (X : File_Num := 1;
      Nam                      : String   := "") return String
   is
      Suffix : String (2 .. 6);
   begin
      if Nam = "" then
         Suffix := Test_Name (3 .. 7);
      else
         Suffix := Nam (3 .. 7);
      end if;

      case X is
         when 1 =>
            return ('X' & Suffix);
         when 2 =>
            return ('Y' & Suffix);
         when 3 =>
            return ('Z' & Suffix);
         when 4 =>
            return ('V' & Suffix);
         when 5 =>
            return ('W' & Suffix);
      end case;
   end Legal_File_Name;

begin

   Test_Name_Len                  := No_Name'Length;
   Test_Name (1 .. Test_Name_Len) := No_Name;

end Report;
