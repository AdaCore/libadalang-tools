-- C53007A.ADA

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
-- CHECK THAT CONTROL FLOWS CORRECTLY IN SIMPLE NESTED IF_STATEMENTS.

-- JRK 7/23/80
-- SPS 3/4/83

with Report;
procedure C53007a is

   use Report;

   Ci1 : constant Integer := 1;
   Ci9 : constant Integer := 9;
   Cbt : constant Boolean := True;
   Cbf : constant Boolean := False;

   Vi1 : Integer := Ident_Int (1);
   Vi9 : Integer := Ident_Int (9);
   Vbt : Boolean := Ident_Bool (True);
   Vbf : Boolean := Ident_Bool (False);

   Flow_Count : Integer := 0;

begin
   Test
     ("C53007A",
      "CHECK THAT CONTROL FLOWS CORRECTLY IN SIMPLE " &
      "NESTED IF_STATEMENTS");

   if Vbf then  -- (FALSE)
      Failed ("INCORRECT CONTROL FLOW 1");
   elsif Ci9 < 20 then  -- (TRUE)
      Flow_Count := Flow_Count + 1;
      if Vi1 /= 0 and True then  -- (TRUE)
         Flow_Count := Flow_Count + 1;
      else
         Failed ("INCORRECT CONTROL FLOW 2");
      end if;
   else
      Failed ("INCORRECT CONTROL FLOW 3");
   end if;

   if Cbf or else Vi9 = 9 then  -- (TRUE)
      if Vi1 + Ci9 > 0 or (Cbf and Vbt) then  -- (TRUE)
         Flow_Count := Flow_Count + 1;
      end if;
   elsif Vbf or Vi1 > 10 then  -- (FALSE)
      Failed ("INCORRECT CONTROL FLOW 4");
   end if;

   if not Cbt and then not Vbt and then Ci9 < 0 then  -- (FALSE)
      if False or not True then  -- (FALSE)
         Failed ("INCORRECT CONTROL FLOW 5");
      elsif Vi1 >= 0 then  -- (TRUE)
         null;
      else
         Failed ("INCORRECT CONTROL FLOW 6");
      end if;
      Failed ("INCORRECT CONTROL FLOW 7");
   elsif (Vi1 * Ci9 + 3 < 0) or (Vbt and not (Ci1 < 0)) then -- (TRUE)
      Flow_Count := Flow_Count + 1;
      if not Cbt or else Ci9 + 1 = 0 then  -- (FALSE)
         Failed ("INCORRECT CONTROL FLOW 8");
      else
         Flow_Count := Flow_Count + 1;
         if Vi1 * 2 > 0 then  -- (TRUE)
            Flow_Count := Flow_Count + 1;
         elsif True then  -- (TRUE)
            Failed ("INCORRECT CONTROL FLOW 9");
         else
            null;
         end if;
      end if;
   elsif False and Cbf then  -- (FALSE)
      Failed ("INCORRECT CONTROL FLOW 10");
   else
      if Vbt then  -- (TRUE)
         Failed ("INCORRECT CONTROL FLOW 11");
      elsif Vi1 = 0 then  -- (FALSE)
         Failed ("INCORRECT CONTROL FLOW 12");
      else
         Failed ("INCORRECT CONTROL FLOW 13");
      end if;
   end if;

   if 3 = 5 or not Vbt then  -- (FALSE)
      Failed ("INCORRECT CONTROL FLOW 14");
      if True and Cbt then  -- (TRUE)
         Failed ("INCORRECT CONTROL FLOW 15");
      else
         Failed ("INCORRECT CONTROL FLOW 16");
      end if;
   elsif Cbf then  -- (FALSE)
      if Vi9 >= 0 or False then  -- (TRUE)
         if Vbt then  -- (TRUE)
            Failed ("INCORRECT CONTROL FLOW 17");
         end if;
         Failed ("INCORRECT CONTROL FLOW 18");
      elsif Vi1 + Ci9 /= 0 then  -- (TRUE)
         Failed ("INCORRECT CONTROL FLOW 19");
      end if;
      Failed ("INCORRECT CONTROL FLOW 20");
   else
      if Vbt and Ci9 - 9 = 0 then  -- (TRUE)
         if False then  -- (FALSE)
            Failed ("INCORRECT CONTROL FLOW 21");
         elsif not Vbf and then Ci1 > 0 then  -- (TRUE)
            Flow_Count := Flow_Count + 1;
         else
            Failed ("INCORRECT CONTROL FLOW 22");
         end if;
         Flow_Count := Flow_Count + 1;
      elsif not Cbf or Vi1 /= 0 then  -- (TRUE)
         if Vbt then  -- (TRUE)
            null;
         end if;
         Failed ("INCORRECT CONTROL FLOW 23");
      else
         Failed ("INCORRECT CONTROL FLOW 24");
      end if;
      Flow_Count := Flow_Count + 1;
   end if;

   if Flow_Count /= 9 then
      Failed ("INCORRECT FLOW_COUNT VALUE");
   end if;

   Result;
end C53007a;
