-- C59002B.ADA

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
-- CHECK THAT JUMPS OUT OF COMPOUND STATEMENTS (OTHER THAN
--    ACCEPT STATEMENTS) ARE POSSIBLE AND ARE CORRECTLY PERFORMED.

-- FLOW OF CONTROL: A -> B -> C -> D -> E -> F -> G -> H .
--                    |     |     |     |      |     |     |
--                    IF   LOOP   CASE  BLOCK  IF   LOOP   CASE
--                                             LOOP CASE   BLOCK

--          A : GOTO B              L111 -> L311
--          FAILURE                 L121
--          E : GOTO F              L131 -> L331

--          FAILURE                 L100

--          C : GOTO D              L211 -> L411
--          FAILURE                 L221
--          G : GOTO H              L231

--          FAILURE                 L200

--          B : GOTO C              L311 -> L211
--          FAILURE                 L321
--          F : GOTO G              L331

--          FAILURE                 L300

--          D : GOTO E              L411 -> L131
--          FAILURE                 L421
--          H :                     L431 -> (OUT)

--          PRINT RESULTS

-- RM 06/05/81
-- SPS 3/8/83

with Report;
procedure C59002b is

   use Report;

begin

   Test ("C59002B", "CHECK THAT ONE CAN JUMP OUT OF COMPOUND STATE" & "MENTS");

   declare

      Flow_String : String (1 .. 8) := "XXXXXXXX";
      Index       : Integer         := 1;

   begin

      <<L111>>

      Flow_String (Index) := 'A';
      Index               := Index + 1;

      if False then
         Failed ("WRONG 'IF' BRANCH");
      else
         goto L311;
      end if;

      <<L121>>

      Failed ("AT L121  -  WRONGLY");

      <<L131>>

      Flow_String (Index) := 'E';
      Index               := Index + 1;

      if False then
         Failed ("WRONG 'IF' BRANCH");
      else
         for J in 1 .. 1 loop
            goto L331;
         end loop;
      end if;

      <<L100>>

      Failed ("AT L100  -  WRONGLY");

      <<L211>>

      Flow_String (Index) := 'C';
      Index               := Index + 1;

      case 2 is
         when 1 =>
            Failed ("WRONG 'CASE' BRANCH");
         when others =>
            goto L411;
      end case;

      <<L221>>

      Failed ("AT L221  -  WRONGLY");

      <<L231>>

      Flow_String (Index) := 'G';
      Index               := Index + 1;

      case 2 is
         when 1 =>
            Failed ("WRONG 'CASE' BRANCH");
         when others =>
            declare
            begin
               goto L431;
            end;
      end case;

      <<L200>>

      Failed ("AT L200  -  WRONGLY");

      <<L311>>

      Flow_String (Index) := 'B';
      Index               := Index + 1;

      for I in 1 .. 1 loop
         goto L211;
      end loop;

      <<L321>>

      Failed ("AT L321  -  WRONGLY");

      <<L331>>

      Flow_String (Index) := 'F';
      Index               := Index + 1;

      for I in 1 .. 1 loop
         case 2 is
            when 1 =>
               Failed ("WRONG 'CASE' BRANCH");
            when others =>
               goto L231;
         end case;
      end loop;

      <<L300>>

      Failed ("AT L300  -  WRONGLY");

      <<L411>>

      Flow_String (Index) := 'D';
      Index               := Index + 1;

      declare
         K : Integer := 17;
      begin
         goto L131;
      end;

      <<L421>>

      Failed ("AT L421  -  WRONGLY");

      <<L431>>

      Flow_String (Index) := 'H';

      if Flow_String /= "ABCDEFGH" then
         Failed ("WRONG FLOW OF CONTROL");
      end if;

   end;

   Result;

end C59002b;
