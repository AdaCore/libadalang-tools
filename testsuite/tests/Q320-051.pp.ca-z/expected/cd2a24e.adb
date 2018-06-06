-- CD2A24E.ADA

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
-- OBJECTIVE:
--     CHECK THAT IF A SIZE CLAUSE AND AN ENUMERATION
--     REPRESENTATION CLAUSE ARE GIVEN FOR AN ENUMERATION TYPE,
--     AND THE SMALLEST SIZE APPROPRIATE FOR AN UNSIGNED REPRESENTATION
--     IS SPECIFIED, THEN OPERATIONS ON THE TYPE ARE NOT AFFECTED.

-- HISTORY:
--     JET 08/19/87 CREATED ORIGINAL TEST.
--     PWB 05/11/89 CHANGED EXTENSION FROM '.DEP' TO '.ADA'.
--     WMC 03/27/92 ELIMINATED TEST REDUNDANCIES.
--     RLB 03/20/14 ELIMINATED ADA 2012 INCOMPATIBILITY.

with Report; use Report;
procedure Cd2a24e is

   Basic_Size : constant := 3;

   type Check_Type is (Zero, One, Two);

   for Check_Type use (Zero => 3, One => 4, Two => 5);

   for Check_Type'Size use Basic_Size;

   C0 : Check_Type := Zero;
   C1 : Check_Type := One;
   C2 : Check_Type := Two;
   C3 : Check_Type := Two;

   type Array_Type is array (0 .. 2) of Check_Type;
   Charray : Array_Type := (Zero, One, Two);

   type Rec_Type is record
      Comp0 : Check_Type := Zero;
      Comp1 : Check_Type := One;
      Comp2 : Check_Type := Two;
   end record;

   Chrec : Rec_Type;

   function Ident (Ch : Check_Type) return Check_Type is
   begin
      if Equal (3, 3) then
         return Ch;
      else
         return One;
      end if;
   end Ident;

   procedure Proc
     (Ci0, Ci2 :     Check_Type; Cio1, Cio2 : in out Check_Type;
      Co2      : out Check_Type)
   is
   begin
      if not
        ((Ci0 < Ident (One)) and (Ident (Ci2) > Ident (Cio1)) and
         (Cio1 <= Ident (One)) and (Ident (Two) = Ci2))
      then
         Failed ("INCORRECT RESULTS FOR RELATIONAL OPERATORS " & "- 1");
      end if;

      if Check_Type'Pos (Ci0) /= Ident_Int (0) or
        Check_Type'Pos (Cio1) /= Ident_Int (1) or
        Check_Type'Pos (Ci2) /= Ident_Int (2) then
         Failed ("INCORRECT VALUE FOR CHECK_TYPE'POS - 1");
      end if;

      if Check_Type'Succ (Ci0) /= Ident (Cio1) or
        Check_Type'Succ (Cio1) /= Ident (Ci2) then
         Failed ("INCORRECT VALUE FOR CHECK_TYPE'SUCC - 1");
      end if;

      if Check_Type'Image (Ci0) /= Ident_Str ("ZERO") or
        Check_Type'Image (Cio1) /= Ident_Str ("ONE") or
        Check_Type'Image (Ci2) /= Ident_Str ("TWO") then
         Failed ("INCORRECT VALUE FOR CHECK_TYPE'IMAGE - 1");
      end if;

      Co2 := Two;

   end Proc;

begin
   Test
     ("CD2A24E",
      "CHECK THAT IF A SIZE CLAUSE AND AN ENUMERATION " &
      "REPRESENTATION CLAUSE ARE GIVEN FOR AN " &
      "ENUMERATION TYPE, AND THE SMALLEST SIZE " &
      "APPROPRIATE FOR AN UNSIGNED REPRESENTATION " &
      "IS SPECIFIED, THEN OPERATIONS ON THE TYPE " & "ARE NOT AFFECTED");

   Proc (Zero, Two, C1, C2, C3);

   if C1 /= One or C2 /= Two or C3 /= Two then
      Failed ("INCORRECT VALUE RETURNED BY PROCEDURE");
   end if;

   if Check_Type'Size /= Ident_Int (Basic_Size) then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'SIZE");
   end if;

   if C0'Size < Ident_Int (Basic_Size) then
      Failed ("INCORRECT VALUE FOR C0'SIZE");
   end if;

   if not ((Ident (C1) in C1 .. C2) and (C0 not in Ident (One) .. C2)) then
      Failed ("INCORRECT RESULTS FOR MEMBERSHIP OPERATORS - 2");
   end if;

   if Check_Type'First /= Ident (Zero) then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'FIRST - 2");
   end if;

   if Check_Type'Val (0) /= Ident (C0) or Check_Type'Val (1) /= Ident (C1) or
     Check_Type'Val (2) /= Ident (C2) then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'VAL - 2");
   end if;

   if Check_Type'Pred (C1) /= Ident (C0) or Check_Type'Pred (C2) /= Ident (C1)
   then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'PRED - 2");
   end if;

   if Check_Type'Value ("ZERO") /= Ident (C0) or
     Check_Type'Value ("ONE") /= Ident (C1) or
     Check_Type'Value ("TWO") /= Ident (C2) then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'VALUE - 2");
   end if;

   if Charray (1)'Size < Ident_Int (Basic_Size) then
      Failed ("INCORRECT VALUE FOR CHARRAY(1)'SIZE");
   end if;

   if not
     ((Charray (0) < Ident (One)) and
      (Ident (Charray (2)) > Ident (Charray (1))) and
      (Charray (1) <= Ident (One)) and (Ident (Two) = Charray (2)))
   then
      Failed ("INCORRECT RESULTS FOR RELATIONAL OPERATORS - 3");
   end if;

   if not
     ((Ident (Charray (1)) in Charray (1) .. Charray (2)) and
      (Charray (0) not in Ident (One) .. Charray (2)))
   then
      Failed ("INCORRECT RESULTS FOR MEMBERSHIP OPERATORS - 3");
   end if;

   if Check_Type'Pos (Charray (0)) /= Ident_Int (0) or
     Check_Type'Pos (Charray (1)) /= Ident_Int (1) or
     Check_Type'Pos (Charray (2)) /= Ident_Int (2) then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'POS - 3");
   end if;

   if Check_Type'Succ (Charray (0)) /= Ident (Charray (1)) or
     Check_Type'Succ (Charray (1)) /= Ident (Charray (2)) then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'SUCC - 3");
   end if;

   if Check_Type'Image (Charray (0)) /= Ident_Str ("ZERO") or
     Check_Type'Image (Charray (1)) /= Ident_Str ("ONE") or
     Check_Type'Image (Charray (2)) /= Ident_Str ("TWO") then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'IMAGE - 3");
   end if;

   if Chrec.Comp2'Size < Ident_Int (Basic_Size) then
      Failed ("INCORRECT VALUE FOR CHREC.COMP2'SIZE");
   end if;

   if not
     ((Chrec.Comp0 < Ident (One)) and
      (Ident (Chrec.Comp2) > Ident (Chrec.Comp1)) and
      (Chrec.Comp1 <= Ident (One)) and (Ident (Two) = Chrec.Comp2))
   then
      Failed ("INCORRECT RESULTS FOR RELATIONAL OPERATORS - 4");
   end if;

   if not
     ((Ident (Chrec.Comp1) in Chrec.Comp1 .. Chrec.Comp2) and
      (Chrec.Comp0 not in Ident (One) .. Chrec.Comp2))
   then
      Failed ("INCORRECT RESULTS FOR MEMBERSHIP OPERATORS - 4");
   end if;

   if Check_Type'Val (0) /= Ident (Chrec.Comp0) or
     Check_Type'Val (1) /= Ident (Chrec.Comp1) or
     Check_Type'Val (2) /= Ident (Chrec.Comp2) then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'VAL - 4");
   end if;

   if Check_Type'Pred (Chrec.Comp1) /= Ident (Chrec.Comp0) or
     Check_Type'Pred (Chrec.Comp2) /= Ident (Chrec.Comp1) then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'PRED - 4");
   end if;

   if Check_Type'Value ("ZERO") /= Ident (Chrec.Comp0) or
     Check_Type'Value ("ONE") /= Ident (Chrec.Comp1) or
     Check_Type'Value ("TWO") /= Ident (Chrec.Comp2) then
      Failed ("INCORRECT VALUE FOR CHECK_TYPE'VALUE - 4");
   end if;

   Result;
end Cd2a24e;
