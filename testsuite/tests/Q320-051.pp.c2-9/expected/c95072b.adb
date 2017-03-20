-- C95072B.ADA

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
-- PASSED BY COPY FOR ALL MODES.
-- SUBTESTS ARE:
--   (A)  PRIVATE SCALAR PARAMETERS TO ENTRIES.
--   (B)  PRIVATE ACCESS PARAMETERS TO ENTRIES.

-- JWC 7/22/85

with Report; use Report;
procedure C95072b is

begin
   Test
     ("C95072B",
      "CHECK THAT PRIVATE SCALAR AND ACCESS " & "PARAMETERS ARE COPIED");

   ---------------------------------------------------

   declare  -- (A)

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
         begin
            return T (Integer (Old) + Integer (Increment));
         end "+";

         function Convert (Old_Private : in T) return Integer is
         begin
            return Integer (Old_Private);
         end Convert;

      end Scalar_Pkg;

      use Scalar_Pkg;

   begin  -- (A)

      declare  -- (A1)

         I : T;
         E : exception;

         task Ta is
            entry Ea (Ei : in T; Eo : out T; Eio : in out T);
         end Ta;

         task body Ta is

            Temp : T;

         begin

            accept Ea (Ei : in T; Eo : out T; Eio : in out T) do

               Temp := Ei;    -- SAVE VALUE OF EI AT ACCEPT.

               Eo := C10;
               if Ei /= Temp then
                  Failed
                    ("ASSIGNMENT TO PRIVATE " &
                     "(SCALAR) OUT PARAMETER " &
                     "CHANGES THE VALUE OF INPUT " &
                     "PARAMETER");
                  Temp := Ei;   -- RESET TEMP FOR NEXT CASE.
               end if;

               Eio := Eio + C100;
               if Ei /= Temp then
                  Failed
                    ("ASSIGNMENT TO PRIVATE " &
                     "(SCALAR) IN OUT PARAMETER " &
                     "CHANGES THE VALUE OF INPUT " &
                     "PARAMETER");
                  Temp := Ei;   -- RESET TEMP FOR NEXT CASE.
               end if;

               I := I + C1;
               if Ei /= Temp then
                  Failed
                    ("ASSIGNMENT TO PRIVATE " &
                     "(SCALAR) ACTUAL PARAMETER " &
                     "CHANGES THE VALUE OF " &
                     "INPUT PARAMETER");
               end if;

               raise E;              -- CHECK EXCEPTION
               -- HANDLING.
            end Ea;

         exception
            when others =>
               null;
         end Ta;

      begin    -- (A1)

         I := C0;  -- INITIALIZE I SO VARIOUS CASES CAN BE
         -- DETECTED.
         Ta.Ea (I, I, I);
         Failed ("EXCEPTION NOT RAISED - A");

      exception
         when E =>
            if I /= C1 then
               case Convert (I) is
                  when 11 =>
                     Failed
                       ("OUT ACTUAL PRIVATE " &
                        "(SCALAR) PARAMETER " &
                        "CHANGED GLOBAL VALUE");
                  when 101 =>
                     Failed
                       ("IN OUT ACTUAL PRIVATE " &
                        "(SCALAR) PARAMETER " &
                        "CHANGED GLOBAL VALUE");
                  when 111 =>
                     Failed
                       ("OUT AND IN OUT ACTUAL " &
                        "PRIVATE (SCALAR) " &
                        "PARAMETER CHANGED " &
                        "GLOBAL VALUE");
                  when others =>
                     Failed ("UNDETERMINED CHANGE TO " & "GLOBAL VALUE");
               end case;
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - A");
      end;  -- (A1)

   end;  -- (A)

   ---------------------------------------------------

   declare  -- (B)

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

   begin    -- (B)

      declare  -- (B1)

         I : T;
         E : exception;

         task Tb is
            entry Eb (Ei : in T; Eo : out T; Eio : in out T);
         end Tb;

         task body Tb is

            Temp : T;

         begin

            accept Eb (Ei : in T; Eo : out T; Eio : in out T) do

               Temp := Ei;  -- SAVE VALUE OF EI AT ACCEPT.

               I := C101;
               if Ei /= Temp then
                  Failed
                    ("ASSIGNMENT TO PRIVATE " &
                     "(ACCESS) ACTUAL VARIABLE " &
                     "CHANGES THE VALUE OF INPUT " &
                     "PARAMETER");
                  Temp := Ei;   -- RESET TEMP FOR NEXT CASE.
               end if;

               Eo := C1;
               if Ei /= Temp then
                  Failed
                    ("ASSIGNMENT TO PRIVATE " &
                     "(ACCESS) OUT PARAMETER " &
                     "CHANGES THE VALUE OF INPUT " &
                     "PARAMETER");
                  Temp := Ei;   -- RESET TEMP FOR NEXT CASE.
               end if;

               Eio := C10;
               if Ei /= Temp then
                  Failed
                    ("ASSIGNMENT TO PRIVATE " &
                     "(ACCESS) IN OUT PARAMETER " &
                     "CHANGES THE VALUE OF INPUT " &
                     "PARAMETER");
               end if;

               raise E;                 -- CHECK EXCEPTION
               -- HANDLING.
            end Eb;

         exception
            when others =>
               null;
         end Tb;

      begin     -- (B1)

         I := C100;
         Tb.Eb (I, I, I);
         Failed ("EXCEPTION NOT RAISED - B");

      exception
         when E =>
            if I /= C101 then
               Failed
                 ("OUT OR IN OUT ACTUAL ENTRY " &
                  "PARAMETER VALUE CHANGED DESPITE " &
                  "RAISED EXCEPTION");
            end if;
         when others =>
            Failed ("WRONG EXCEPTION RAISED - B");
      end;  -- (B1)

   end;  -- (B)

   ---------------------------------------------------

   Result;
end C95072b;
