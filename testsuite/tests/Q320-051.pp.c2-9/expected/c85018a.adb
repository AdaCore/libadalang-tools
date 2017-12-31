-- C85018A.ADA

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
-- CHECK THAT AN ENTRY FAMILY MEMBER CAN BE RENAMED WITH:
--           1) DIFFERENT PARAMETER NAMES;
--           2) DIFFERENT DEFAULT VALUES;
--      AND THAT THE NEW NAMES/DEFAULTS ARE USED WHEN THE NEW NAME
--      IS USED IN A CALL.

-- RJW 6/3/86

with Report; use Report;

procedure C85018a is

begin

   Test
     ("C85018A",
      "CHECK THAT AN ENTRY FAMILY MEMBER CAN BE " &
      "RENAMED AND THAT THE NEW NAMES/DEFAULTS ARE " &
      "THOSE ASSOCIATED WITH THE RENAMED ENTITY");

   declare

      Results : Integer;

      type Ta is array (1 .. 5) of Integer;

      task T is
         entry Ent1 (Boolean) (A : Integer := 1; B : Ta := (1 .. 5 => 1));
      end T;

      procedure Enta (C : Integer := 1; D : Ta := (1 .. 5 => 1)) renames
        T.Ent1 (True);

      procedure Entb (B : Integer := 1; A : Ta := (1 .. 5 => 1)) renames
        T.Ent1 (True);

      procedure Entc (A : Integer := 2; B : Ta := (1, 2, 3, 4, 5)) renames
        T.Ent1 (True);

      procedure Entd (C : Integer := 2; D : Ta := (1, 2, 3, 4, 5)) renames
        T.Ent1 (True);

      task body T is
      begin
         loop
            select
               accept Ent1
               (Ident_Bool (True)) (A : Integer := 1; B : Ta := (1 .. 5 => 1))
               do
                  if A in 1 .. 5 then
                     Results := B (A);
                  else
                     Results := 0;
                  end if;
               end Ent1;
            or
               terminate;
            end select;
         end loop;
      end T;

   begin

      T.Ent1 (True);
      if Results /= 1 then
         Failed ("PARAMETERS NOT PROPERLY INITIALIZED");
      end if;

      T.Ent1 (True) (A => 6);
      if Results /= 0 then
         Failed ("INCORRECT RESULTS");
      end if;

      Enta;
      if Results /= 1 then
         Failed ("CASE 1 : INCORRECT RESULTS (DEFAULT)");
      end if;

      Enta (D => (5, 4, 3, 2, 1));
      if Results /= 5 then
         Failed ("CASE 1 : INCORRECT RESULTS");
      end if;

      Entb;
      if Results /= 1 then
         Failed ("CASE 1 : INCORRECT RESULTS (DEFAULT)");
      end if;

      Entb (A => (5, 4, 3, 2, 1), B => 2);
      if Results /= 4 then
         Failed ("CASE 1 : INCORRECT RESULTS ");
      end if;

      Entc;
      if Results /= 2 then
         Failed ("CASE 2 : INCORRECT RESULTS (DEFAULT)");
      end if;

      Entc (3);
      if Results /= 3 then
         Failed ("CASE 2 : INCORRECT RESULTS ");
      end if;

      Entd;
      if Results /= 2 then
         Failed ("CASE 2 : INCORRECT RESULTS (DEFAULT)");
      end if;

      Entd (4);
      if Results /= 4 then
         Failed ("CASE 2 : INCORRECT RESULTS ");
      end if;

   end;
   Result;

end C85018a;
