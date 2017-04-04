-- C95072A.ADA

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
-- CHECK THAT SCALAR AND ACCESS PARAMETERS ARE COPIED FOR ALL THREE PARAMETER
-- MODES. SUBTESTS ARE:
--   (A)  SCALAR PARAMETERS TO ENTRIES.
--   (B)  ACCESS PARAMETERS TO ENTRIES.

-- JWC 7/22/85

with Report; use Report;
procedure C95072a is

begin
   Test ("C95072A", "CHECK THAT SCALAR AND ACCESS PARAMETERS ARE " & "COPIED");

   --------------------------------------------------

   declare  -- (A)

      I : Integer;
      E : exception;

      task Ta is
         entry Ea (Ei : in Integer; Eo : out Integer; Eio : in out Integer);
      end Ta;

      task body Ta is

         Tmp : Integer;

      begin

         accept Ea (Ei : in Integer; Eo : out Integer; Eio : in out Integer) do

            Tmp := Ei;     -- SAVE VALUE OF EI AT ACCEPT.

            Eo := 10;
            if Ei /= Tmp then
               Failed
                 ("ASSIGNMENT TO SCALAR OUT " &
                  "PARAMETER CHANGES THE VALUE OF " &
                  "INPUT PARAMETER");
               Tmp := Ei;     -- RESET TMP FOR NEXT CASE.
            end if;

            Eio := Eio + 100;
            if Ei /= Tmp then
               Failed
                 ("ASSIGNMENT TO SCALAR IN OUT " &
                  "PARAMETER CHANGES THE VALUE OF " &
                  "INPUT PARAMETER");
               Tmp := Ei;     -- RESET TMP FOR NEXT CASE.
            end if;

            I := I + 1;
            if Ei /= Tmp then
               Failed
                 ("ASSIGNMENT TO SCALAR ACTUAL " &
                  "PARAMETER CHANGES THE VALUE OF " &
                  "INPUT PARAMETER");
            end if;

            raise E;            -- CHECK EXCEPTION HANDLING.
         end Ea;

      exception
         when others =>
            null;
      end Ta;

   begin  -- (A)

      I := 0;   -- INITIALIZE I SO VARIOUS CASES CAN BE DETECTED.
      Ta.Ea (I, I, I);
      Failed ("EXCEPTION NOT RAISED - A");

   exception
      when E =>
         if I /= 1 then
            case I is
               when 11 =>
                  Failed
                    ("OUT ACTUAL SCALAR PARAMETER " & "CHANGED GLOBAL VALUE");
               when 101 =>
                  Failed
                    ("IN OUT ACTUAL SCALAR " &
                     "PARAMETER CHANGED GLOBAL VALUE");
               when 111 =>
                  Failed
                    ("OUT AND IN OUT ACTUAL SCALAR " &
                     "PARAMETERS CHANGED GLOBAL " &
                     "VALUE");
               when others =>
                  Failed ("UNDETERMINED CHANGE TO GLOBAL " & "VALUE");
            end case;
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - A");
   end;  -- (A)

   --------------------------------------------------

   declare  -- (B)

      type Acctype is access Integer;

      I : Acctype;
      E : exception;

      task Tb is
         entry Eb (Ei : in Acctype; Eo : out Acctype; Eio : in out Acctype);
      end Tb;

      task body Tb is

         Tmp : Acctype;

      begin

         accept Eb (Ei : in Acctype; Eo : out Acctype; Eio : in out Acctype) do

            Tmp := Ei;     -- SAVE VALUE OF EI AT ACCEPT.

            I := new Integer'(101);
            if Ei /= Tmp then
               Failed
                 ("ASSIGNMENT TO ACCESS ACTUAL " &
                  "PARAMETER CHANGES THE VALUE OF " &
                  "INPUT PARAMETER");
               Tmp := Ei;     -- RESET TMP FOR NEXT CASE.
            end if;

            Eo := new Integer'(1);
            if Ei /= Tmp then
               Failed
                 ("ASSIGNMENT TO ACCESS OUT " &
                  "PARAMETER CHANGES THE VALUE OF " &
                  "INPUT PARAMETER");
               Tmp := Ei;     -- RESET TMP FOR NEXT CASE.
            end if;

            Eio := new Integer'(10);
            if Ei /= Tmp then
               Failed
                 ("ASSIGNMENT TO ACCESS IN OUT " &
                  "PARAMETER CHANGES THE VALUE OF " &
                  "INPUT PARAMETER");
            end if;

            raise E;            -- CHECK EXCEPTION HANDLING.
         end Eb;

      exception
         when others =>
            null;
      end Tb;

   begin  -- (B)

      I := new Integer'(100);
      Tb.Eb (I, I, I);
      Failed ("EXCEPTION NOT RAISED - B");

   exception
      when E =>
         if I.all /= 101 then
            Failed
              ("OUT OR IN OUT ACTUAL ENTRY " &
               "PARAMETER VALUE CHANGED DESPITE " &
               "RAISED EXCEPTION");
         end if;
      when others =>
         Failed ("WRONG EXCEPTION RAISED - B");
   end;  -- (B)

   --------------------------------------------------

   Result;
end C95072a;
