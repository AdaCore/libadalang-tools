-- C62003B.ADA

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
-- CHECK THAT PRIVATE TYPES IMPLEMENTED AS SCALAR OR ACCESS TYPES ARE
--   PASSED BY COPY.
--   SUBTESTS ARE:
--        (A) PRIVATE SCALAR PARAMETERS TO PROCEDURES.
--        (B) PRIVATE SCALAR PARAMETERS TO FUNCTIONS.
--        (C) PRIVATE ACCESS PARAMETERS TO PROCEDURES.
--        (D) PRIVATE ACCESS PARAMETERS TO FUNCTIONS.

-- CPP 05/25/84
-- EG 10/29/85 ELIMINATE THE USE OF NUMERIC_ERROR IN TEST.

with Report; use Report;
procedure C62003b is

begin
   Test
     ("C62003B",
      "CHECK THAT PRIVATE SCALAR AND ACCESS " & "PARAMETERS ARE COPIED");

   ---------------------------------------------------

   A_B :
   declare

      package Scalar_Pkg is

         type T is private;
         C0   : constant T;
         C1   : constant T;
         C10  : constant T;
         C100 : constant T;

         function "+" (Old : in T; Increment : in T) return T;
         function Convert (Old_Private : in T) return Integer;

      private
         type T is new Integer;
         C0   : constant T := 0;
         C1   : constant T := 1;
         C10  : constant T := 10;
         C100 : constant T := 100;

      end Scalar_Pkg;

      package body Scalar_Pkg is

         function "+" (Old : in T; Increment : in T) return T is
         begin     -- "+"
            return T (Integer (Old) + Integer (Increment));
         end "+";

         function Convert (Old_Private : in T) return Integer is
         begin     -- CONVERT
            return Integer (Old_Private);
         end Convert;

      end Scalar_Pkg;

      use Scalar_Pkg;

      ---------------------------------------------------

   begin     -- A_B

      A :
      declare

         I : T;
         E : exception;

         procedure P (Pi : in T; Po : out T; Pio : in out T) is

            Temp : T;

         begin  -- P

            Temp := Pi;    -- SAVE VALUE OF PI AT PROC ENTRY.

            Po := C10;
            if (Pi /= Temp) then
               Failed
                 ("ASSIGNMENT TO PRIVATE (SCALAR) OUT " &
                  "PARAMETER CHANGES THE VALUE OF " & "INPUT PARAMETER");
               Temp := Pi;    -- RESET TEMP FOR NEXT CASE.
            end if;

            Pio := Pio + C100;
            if (Pi /= Temp) then
               Failed
                 ("ASSIGNMENT TO PRIVATE (SCALAR) IN " &
                  "OUT PARAMETER CHANGES THE VALUE OF " & "INPUT PARAMETER");
               Temp := Pi;    -- RESET TEMP FOR NEXT CASE.
            end if;

            I := I + C1;
            if (Pi /= Temp) then
               Failed
                 ("ASSIGNMENT TO PRIVATE (SCALAR) " &
                  "ACTUAL PARAMETER CHANGES THE " &
                  "VALUE OF INPUT PARAMETER");
            end if;

            raise E;  -- CHECK EXCEPTION HANDLING.
         end P;

      begin  -- A
         I := C0;  -- INITIALIZE I SO VARIOUS CASES CAN BE
         -- DETECTED.
         P (I, I, I);
         Failed ("EXCEPTION NOT RAISED - A");
      exception
         when E =>
            if (I /= C1) then
               case Convert (I) is
                  when 11 =>
                     Failed
                       ("OUT ACTUAL PRIVATE " & "(SCALAR) PARAMETER " &
                        "CHANGED GLOBAL VALUE");
                  when 101 =>
                     Failed
                       ("IN OUT ACTUAL PRIVATE " & "(SCALAR) PARAMETER " &
                        "CHANGED GLOBAL VALUE");
                  when 111 =>
                     Failed
                       ("OUT AND IN OUT ACTUAL " & "PRIVATE (SCALAR) " &
                        "PARAMETER CHANGED " & "GLOBAL VALUE");
                  when others =>
                     Failed ("UNDETERMINED CHANGE TO " & "GLOBAL VALUE");
               end case;
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - A");
      end A;

      ---------------------------------------------------

      B :
      declare

         I, J : T;

         function F (Fi : in T) return T is

            Temp : T := Fi;  -- SAVE VALUE OF FI AT FN ENTRY.

         begin  -- F

            I := I + C1;
            if (Fi /= Temp) then
               Failed
                 ("ASSIGNMENT TO PRIVATE (SCALAR) " &
                  "ACTUAL FUNCTION PARAMETER CHANGES " &
                  "THE VALUE OF INPUT PARAMETER ");
            end if;

            return C0;
         end F;

      begin  -- B
         I := C0;
         J := F (I);
      end B;

   end A_B;

   ---------------------------------------------------

   C_D :
   declare

      package Access_Pkg is

         type T is private;
         C_Null : constant T;
         C1     : constant T;
         C10    : constant T;
         C100   : constant T;
         C101   : constant T;

      private
         type T is access Integer;
         C_Null : constant T := null;
         C1     : constant T := new Integer'(1);
         C10    : constant T := new Integer'(10);
         C100   : constant T := new Integer'(100);
         C101   : constant T := new Integer'(101);

      end Access_Pkg;

      use Access_Pkg;

      ---------------------------------------------------

   begin     -- C_D;

      C :
      declare

         I : T;
         E : exception;
         procedure P (Pi : in T; Po : out T; Pio : in out T) is

            Temp : T;

         begin     -- P

            Temp := Pi;    -- SAVE VALUE OF PI AT PROC ENTRY.

            I := C101;
            if (Pi /= Temp) then
               Failed
                 ("ASSIGNMENT TO PRIVATE (ACCESS) " &
                  "ACTUAL VARIABLE CHANGES THE VALUE " & "OF INPUT PARAMETER");
               Temp := Pi;    -- RESET TEMP FOR NEXT CASE.
            end if;

            Po := C1;
            if (Pi /= Temp) then
               Failed
                 ("ASSIGNMENT TO PRIVATE (ACCESS) OUT " &
                  "PARAMETER CHANGES THE VALUE OF " & "INPUT PARAMETER");
               Temp := Pi;    -- RESET TEMP FOR NEXT CASE.
            end if;

            Pio := C10;
            if (Pi /= Temp) then
               Failed
                 ("ASSIGNMENT TO PRIVATE (ACCESS) IN " &
                  "OUT PARAMETER CHANGES THE VALUE " & "OF INPUT PARAMETER");
            end if;

            raise E;  -- CHECK EXCEPTION HANDLING.
         end P;

      begin     -- C
         I := C100;
         P (I, I, I);
         Failed ("EXCEPTION NOT RAISED - C");
      exception
         when E =>
            if (I /= C101) then
               Failed
                 ("OUT OR IN OUT ACTUAL PROCEDURE " &
                  "PARAMETER VALUE CHANGED DESPITE " & "RAISED EXCEPTION");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - C");
      end C;

      ---------------------------------------------------

      D :
      declare

         I, J : T;

         function F (Fi : in T) return T is

            Temp : T := Fi;     -- SAVE VALUE OF FI AT FN ENTRY.

         begin     -- F
            I := C100;
            if (Fi /= Temp) then
               Failed
                 ("ASSIGNMENT TO PRIVATE " & "(ACCESS) ACTUAL FUNCTION " &
                  "PARAMETER CHANGES THE VALUE " & "OF INPUT PARAMETER");
            end if;
            return C_Null;
         end F;

      begin     -- D
         I := C_Null;
         J := F (I);
      end D;

   end C_D;

   ---------------------------------------------------

   Result;

end C62003b;
