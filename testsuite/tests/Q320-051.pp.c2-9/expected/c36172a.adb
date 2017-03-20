-- C36172A.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED APPROPRIATELY
-- ON DISCRETE_RANGES USED AS INDEX_CONSTRAINTS.

-- DAT 2/9/81
-- SPS 4/7/82
-- JBG 6/5/85

with Report;
procedure C36172a is

   use Report;

   subtype Int_10 is Integer range 1 .. 10;
   type A is array (Int_10 range <>) of Integer;

   subtype Int_11 is Integer range 0 .. 11;
   subtype Null_6_4 is Integer range 6 .. 4;
   subtype Null_11_10 is Integer range 11 .. 10;
   subtype Int_9_11 is Integer range 9 .. 11;

   type A_9_11 is array (9 .. 11) of Boolean;
   type A_11_10 is array (11 .. 10) of Integer;
   subtype A_1_10 is A (Int_10);

begin
   Test
     ("C36172A",
      "CONSTRAINT_ERROR IS RAISED APPROPRIATELY" & " FOR INDEX_RANGES");

   begin
      declare
         V : A (9 .. 11);
      begin
         if Equal (V'First, V'First) then
            Failed ("OUT-OF-BOUNDS INDEX_RANGE 1");
         else
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION 1");
   end;

   begin
      declare
         V : A (11 .. 10);
      begin
         if Equal (V'First, V'First) then
            null;
         else
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR " & "RAISED INAPPROPRIATELY 2");
      when others =>
         Failed ("EXCEPTION RAISED WHEN NONE " & "SHOULD BE 2");
   end;

   begin
      declare
         V : A (6 .. 4);
      begin
         if Equal (V'First, V'First) then
            null;
         else
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR " & "RAISED INAPPROPRIATELY 3");
      when others =>
         Failed ("EXCEPTION RAISED WHEN NONE " & "SHOULD BE 3");
   end;

   begin
      declare
         V : A (Int_9_11);
      begin
         if Equal (V'First, V'First) then
            Failed ("OUT-OF-BOUNDS INDEX RANGE 4");
         else
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION 4");
   end;

   begin
      declare
         V : A (Null_11_10);
      begin
         if Equal (V'First, V'First) then
            null;
         else
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR " & "RAISED INAPPROPRIATELY 5");
      when others =>
         Failed ("EXCEPTION RAISED WHEN NONE " & "SHOULD BE 5");
   end;

   begin
      declare
         V : A (Null_6_4);
      begin
         if Equal (V'First, V'First) then
            null;
         else
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR " & "RAISED INAPPROPRIATELY 6");
      when others =>
         Failed ("EXCEPTION RAISED WHEN NONE " & "SHOULD BE 6");
   end;

   begin
      declare
         V : A (Int_9_11 range 10 .. 11);
      begin
         if Equal (V'First, V'First) then
            Failed ("BAD NON-NULL INDEX RANGE 7");
         else
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION 7");
   end;

   begin
      declare
         V : A (Null_11_10 range 11 .. 10);
      begin
         if Equal (V'First, V'First) then
            null;
         else
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR " & "RAISED INAPPROPRIATELY 8");
      when others =>
         Failed ("EXCEPTION RAISED WHEN NONE " & "SHOULD BE 8");
   end;

   begin
      declare
         V : A (Null_6_4 range 6 .. 4);
      begin
         if Equal (V'First, V'First) then
            null;
         else
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR " & "RAISED INAPPROPRIATELY 9");
      when others =>
         Failed ("EXCEPTION RAISED WHEN NONE " & "SHOULD BE 9");
   end;

   begin
      declare
         V : A (A_9_11'Range);
      begin
         if Equal (V'First, V'First) then
            Failed ("BAD INDEX RANGE 10");
         else
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("WRONG EXCEPTION 10");
   end;

   begin
      declare
         V : A (A_11_10'Range);
      begin
         if Equal (V'First, V'First) then
            null;
         else
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR " & "RAISED INAPPROPRIATELY 11");
      when others =>
         Failed ("EXCEPTION RAISED WHEN NONE " & "SHOULD BE 11");
   end;

   begin
      declare
         V : A (6 .. 4);
      begin
         if Equal (V'First, V'First) then
            null;
         else
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when Constraint_Error =>
         Failed ("CONSTRAINT_ERROR " & "RAISED INAPPROPRIATELY 12");
      when others =>
         Failed ("EXCEPTION RAISED WHEN NONE " & "SHOULD BE 12");
   end;

   Result;
end C36172a;
