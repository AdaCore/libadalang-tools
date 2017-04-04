-- C45503A.ADA

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
-- CHECK THAT 'REM' AND 'MOD' YIELD CORRECT RESULTS WHEN THE OPERANDS ARE OF
-- PREDEFINED TYPE INTEGER.

-- R.WILLIAMS 9/1/86

with Report; use Report;
procedure C45503a is

begin
   Test
     ("C45503A",
      "CHECK THAT 'REM' AND 'MOD' YIELD CORRECT " &
      "RESULTS WHEN THE OPERANDS ARE OF PREDEFINED " &
      "TYPE INTEGER");

   declare
      I0  : Integer := 0;
      I1  : Integer := 1;
      I2  : Integer := 2;
      I3  : Integer := 3;
      I4  : Integer := 4;
      I5  : Integer := 5;
      I10 : Integer := 10;
      I11 : Integer := 11;
      I12 : Integer := 12;
      I13 : Integer := 13;
      I14 : Integer := 14;
      N1  : Integer := -1;
      N2  : Integer := -2;
      N3  : Integer := -3;
      N4  : Integer := -4;
      N5  : Integer := -5;
      N10 : Integer := -10;
      N11 : Integer := -11;
      N12 : Integer := -12;
      N13 : Integer := -13;
      N14 : Integer := -14;

   begin
      if I10 rem I5 /= I0 then
         Failed ("INCORRECT RESULT FOR I10 REM I5");
      end if;

      if Ident_Int (I11) rem Ident_Int (I5) /= I1 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (I11) REM " & "IDENT_INT (I5)");
      end if;

      if I12 rem I5 /= I2 then
         Failed ("INCORRECT RESULT FOR I12 REM I5");
      end if;

      if "REM" (Left => I12, Right => I5) /= I2 then
         Failed
           ("INCORRECT RESULT FOR ""REM"" (LEFT => I12, " & "RIGHT => I5)");
      end if;

      if Ident_Int (I13) rem Ident_Int (I5) /= I3 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (I13) REM " & "IDENT_INT (I5)");
      end if;

      if I14 rem I5 /= I4 then
         Failed ("INCORRECT RESULT FOR I14 REM I5");
      end if;

      if Ident_Int (I10) rem Ident_Int (N5) /= I0 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (I10) REM " & "IDENT_INT (N5)");
      end if;

      if "REM" (Left => Ident_Int (I10), Right => Ident_Int (N5)) /= I0 then
         Failed
           ("INCORRECT RESULT FOR ""REM"" (LEFT => " &
            "IDENT_INT (I10), RIGHT => IDENT_INT (N5))");
      end if;

      if I11 rem N5 /= I1 then
         Failed ("INCORRECT RESULT FOR I11 REM N5");
      end if;

      if Ident_Int (I12) rem Ident_Int (N5) /= I2 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (I12) REM " & "IDENT_INT (N5)");
      end if;

      if I13 rem N5 /= I3 then
         Failed ("INCORRECT RESULT FOR I13 REM N5");
      end if;

      if "REM" (Left => I13, Right => N5) /= I3 then
         Failed
           ("INCORRECT RESULT FOR ""REM"" (LEFT => I13, " & "RIGHT => N5)");
      end if;

      if Ident_Int (I14) rem Ident_Int (N5) /= I4 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (I14) REM " & "IDENT_INT (N5)");
      end if;

      if N10 rem I5 /= I0 then
         Failed ("INCORRECT RESULT FOR N10 REM I5");
      end if;

      if Ident_Int (N11) rem Ident_Int (I5) /= N1 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (N11) REM " & "IDENT_INT (I5)");
      end if;

      if "REM" (Left => Ident_Int (N11), Right => Ident_Int (I5)) /= N1 then
         Failed
           ("INCORRECT RESULT FOR ""REM"" (LEFT => " &
            "IDENT_INT (N11), RIGHT => IDENT_INT (I5))");
      end if;

      if N12 rem I5 /= N2 then
         Failed ("INCORRECT RESULT FOR N12 REM I5");
      end if;

      if Ident_Int (N13) rem Ident_Int (I5) /= N3 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (N13) REM " & "IDENT_INT (I5)");
      end if;

      if N14 rem I5 /= N4 then
         Failed ("INCORRECT RESULT FOR N14 REM I5");
      end if;

      if "REM" (Left => N14, Right => I5) /= N4 then
         Failed
           ("INCORRECT RESULT FOR ""REM"" (LEFT => N14, " & "RIGHT => I5)");
      end if;

      if Ident_Int (N10) rem Ident_Int (N5) /= I0 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (N10) REM " & "IDENT_INT (N5)");
      end if;

      if N11 rem N5 /= N1 then
         Failed ("INCORRECT RESULT FOR N11 REM N5");
      end if;

      if Ident_Int (N12) rem Ident_Int (N5) /= N2 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (N12) REM " & "IDENT_INT (N5)");
      end if;

      if "REM" (Left => Ident_Int (N12), Right => Ident_Int (N5)) /= N2 then
         Failed
           ("INCORRECT RESULT FOR ""REM"" (LEFT => " &
            "IDENT_INT (N12), RIGHT => IDENT_INT (N5))");
      end if;

      if N13 rem N5 /= N3 then
         Failed ("INCORRECT RESULT FOR N13 REM N5");
      end if;

      if Ident_Int (N14) rem Ident_Int (N5) /= N4 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (N14) REM " & "IDENT_INT (N5)");
      end if;

      if I10 mod I5 /= I0 then
         Failed ("INCORRECT RESULT FOR I10 MOD I5");
      end if;

      if Ident_Int (I11) mod Ident_Int (I5) /= I1 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (I11) MOD " & "IDENT_INT (I5)");
      end if;

      if I12 mod I5 /= I2 then
         Failed ("INCORRECT RESULT FOR I12 MOD I5");
      end if;

      if "MOD" (Left => I12, Right => I5) /= I2 then
         Failed
           ("INCORRECT RESULT FOR ""MOD"" (LEFT => I12, " & "RIGHT => I5)");
      end if;

      if Ident_Int (I13) mod Ident_Int (I5) /= I3 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (I13) MOD " & "IDENT_INT (I5)");
      end if;

      if I14 mod I5 /= I4 then
         Failed ("INCORRECT RESULT FOR I14 MOD I5");
      end if;

      if Ident_Int (I10) mod Ident_Int (N5) /= I0 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (I10) MOD " & "IDENT_INT (N5)");
      end if;

      if "MOD" (Left => Ident_Int (I10), Right => Ident_Int (N5)) /= I0 then
         Failed
           ("INCORRECT RESULT FOR ""MOD"" (LEFT => " &
            "IDENT_INT (I10), RIGHT => IDENT_INT (N5))");
      end if;

      if I11 mod N5 /= N4 then
         Failed ("INCORRECT RESULT FOR I11 MOD N5");
      end if;

      if Ident_Int (I12) mod Ident_Int (N5) /= N3 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (I12) MOD " & "IDENT_INT (N5)");
      end if;

      if I13 mod N5 /= N2 then
         Failed ("INCORRECT RESULT FOR I13 MOD N5");
      end if;

      if "MOD" (Left => I13, Right => N5) /= N2 then
         Failed
           ("INCORRECT RESULT FOR ""MOD"" (LEFT => I13, " & "RIGHT => N5)");
      end if;

      if Ident_Int (I14) mod Ident_Int (N5) /= N1 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (I14) MOD " & "IDENT_INT (N5)");
      end if;

      if N10 mod I5 /= I0 then
         Failed ("INCORRECT RESULT FOR N10 MOD I5");
      end if;

      if Ident_Int (N11) mod Ident_Int (I5) /= I4 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (N11) MOD " & "IDENT_INT (I5)");
      end if;

      if "MOD" (Left => Ident_Int (N11), Right => Ident_Int (I5)) /= I4 then
         Failed
           ("INCORRECT RESULT FOR ""MOD"" (LEFT => " &
            "IDENT_INT (N11), RIGHT => IDENT_INT (I5))");
      end if;

      if N12 mod I5 /= I3 then
         Failed ("INCORRECT RESULT FOR N12 MOD I5");
      end if;

      if Ident_Int (N13) mod Ident_Int (I5) /= I2 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (N13) MOD " & "IDENT_INT (I5)");
      end if;

      if N14 mod I5 /= I1 then
         Failed ("INCORRECT RESULT FOR N14 MOD I5");
      end if;

      if "MOD" (Left => N14, Right => I5) /= I1 then
         Failed
           ("INCORRECT RESULT FOR ""MOD"" (LEFT => I14, " & "RIGHT => I5)");
      end if;

      if Ident_Int (N10) mod Ident_Int (N5) /= I0 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (N10) MOD " & "IDENT_INT (N5)");
      end if;

      if N11 mod N5 /= N1 then
         Failed ("INCORRECT RESULT FOR N11 MOD N5");
      end if;

      if Ident_Int (N12) mod Ident_Int (N5) /= N2 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (N12) MOD " & "IDENT_INT (N5)");
      end if;

      if "MOD" (Left => Ident_Int (N12), Right => Ident_Int (N5)) /= N2 then
         Failed
           ("INCORRECT RESULT FOR ""MOD"" (LEFT => " &
            "IDENT_INT (N12), RIGHT => IDENT_INT (N5))");
      end if;

      if N13 mod N5 /= N3 then
         Failed ("INCORRECT RESULT FOR N13 MOD N5");
      end if;

      if Ident_Int (N14) mod Ident_Int (N5) /= N4 then
         Failed
           ("INCORRECT RESULT FOR IDENT_INT (N14) MOD " & "IDENT_INT (N5)");
      end if;
   end;

   Result;
end C45503a;
