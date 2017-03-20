-- CD2A32E.ADA

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
--     CHECK THAT WHEN A SIZE SPECIFICATION IS GIVEN FOR AN
--     INTEGER TYPE, THEN OPERATIONS ON VALUES OF SUCH A TYPE
--     WITH THE SMALLEST APPROPRIATE UNSIGNED SIZE ARE NOT
--     AFFECTED BY THE REPRESENTATION CLAUSE.

-- HISTORY:
--     JET 08/12/87  CREATED ORIGINAL TEST.
--     DHH 04/11/89  CHANGED EXTENSION FROM '.DEP' TO '.ADA', CHANGED
--                   SIZE CLAUSE VALUE TO 7, AND CHANGED OPERATOR ON
--                   'SIZE CHECKS.
--     JRL 03/27/92  ELIMINATED REDUNDANT TESTING.
--     RLB 03/20/14  ELIMINATED ADA 2012 INCOMPATIBILITY.

with Report; use Report;
procedure Cd2a32e is

   Basic_Size : constant := 7;

   type Int is range 0 .. 126;

   for Int'Size use Basic_Size;

   I0 : Int := 0;
   I1 : Int := 63;
   I2 : Int := 126;
   I3 : Int := 95;

   type Array_Type is array (Integer range 0 .. 2) of Int;
   Intarray : Array_Type := (0, 63, 126);

   type Rec_Type is record
      Comp0 : Int := 0;
      Comp1 : Int := 63;
      Comp2 : Int := 126;
   end record;

   Irec : Rec_Type;

   function Ident (I : Int) return Int is
   begin
      if Equal (0, 0) then
         return I;
      else
         return 0;
      end if;
   end Ident;

   procedure Proc (Pi0, Pi2 : Int; Pio1, Pio2 : in out Int; Po2 : out Int) is

   begin
      if Pi0'Size < Ident_Int (Basic_Size) then
         Failed ("INCORRECT VALUE FOR PI0'SIZE");
      end if;

      if not
        ((Pi0 < Ident (1)) and
         (Ident (Pi2) > Ident (Pio1)) and
         (Pio1 <= Ident (63)) and
         (Ident (126) = Pi2))
      then
         Failed ("INCORRECT RESULTS FOR RELATIONAL " & "OPERATORS - 1");
      end if;

      if not
        (((Pi0 + Pi2) = Pio2) and
         ((Pi2 - Pio1) = Pio1) and
         ((Pio1 * Ident (2)) = Pi2) and
         ((Pio2 / Pio1) = Ident (2)) and
         ((Pio1**1) = Ident (63)) and
         ((Pio2 rem 10) = Ident (6)) and
         ((Pio1 mod 10) = Ident (3)))
      then
         Failed ("INCORRECT RESULTS FOR BINARY ARITHMETIC " & "OPERATORS - 1");
      end if;

      if Int'Pos (Pi0) /= Ident_Int (0) or
        Int'Pos (Pio1) /= Ident_Int (63) or
        Int'Pos (Pi2) /= Ident_Int (126)
      then
         Failed ("INCORRECT VALUE FOR INT'POS - 1");
      end if;

      if Int'Succ (Pi0) /= Ident (1) or Int'Succ (Pio1) /= Ident (64) then
         Failed ("INCORRECT VALUE FOR INT'SUCC - 1");
      end if;

      if Int'Image (Pi0) /= Ident_Str (" 0") or
        Int'Image (Pio1) /= Ident_Str (" 63") or
        Int'Image (Pi2) /= Ident_Str (" 126")
      then
         Failed ("INCORRECT VALUE FOR INT'IMAGE - 1");
      end if;

      Po2 := 95;

   end Proc;

begin
   Test
     ("CD2A32E",
      "CHECK THAT WHEN A SIZE SPECIFICATION IS " &
      "GIVEN FOR AN INTEGER TYPE, THEN " &
      "OPERATIONS ON VALUES OF SUCH A TYPE WITH " &
      "THE SMALLEST APPROPRIATE UNSIGNED SIZE ARE " &
      "NOT AFFECTED BY THE REPRESENTATION CLAUSE");

   Proc (0, 126, I1, I2, I3);

   if Int'Size /= Ident_Int (Basic_Size) then
      Failed ("INCORRECT VALUE FOR INT'SIZE");
   end if;

   if I1'Size < Ident_Int (Basic_Size) then
      Failed ("INCORRECT VALUE FOR I1'SIZE");
   end if;

   for I in Ident (I0) .. Ident (I2) loop
      if not (I in I0 .. I2) or (I not in Ident (0) .. Ident (126)) then
         Failed ("INCORRECT RESULTS FOR MEMBERSHIP " & "OPERATORS - 2");
      end if;
   end loop;

   if not ((+I2 = I2) and (-I1 = -63) and (abs I2 = I2)) then
      Failed ("INCORRECT RESULTS FOR UNARY ARITHMETIC " & "OPERATORS - 2");
   end if;

   if Int'Val (0) /= Ident (I0) or
     Int'Val (63) /= Ident (I1) or
     Int'Val (126) /= Ident (I2)
   then
      Failed ("INCORRECT VALUE FOR INT'VAL - 2");
   end if;

   if Int'Pred (I1) /= Ident (62) or
     Int'Pred (I2) /= Ident (125) or
     Int'Pred (I3) /= Ident (94)
   then
      Failed ("INCORRECT VALUE FOR INT'PRED - 2");
   end if;

   if Int'Value ("0") /= Ident (I0) or
     Int'Value ("63") /= Ident (I1) or
     Int'Value ("126") /= Ident (I2)
   then
      Failed ("INCORRECT VALUE FOR INT'VALUE - 2");
   end if;

   if Intarray (1)'Size < Ident_Int (Basic_Size) then
      Failed ("INCORRECT VALUE FOR INTARRAY(1)'SIZE");
   end if;

   if not
     ((Intarray (0) < Ident (1)) and
      (Ident (Intarray (2)) > Ident (Intarray (1))) and
      (Intarray (1) <= Ident (63)) and
      (Ident (126) = Intarray (2)))
   then
      Failed ("INCORRECT RESULTS FOR RELATIONAL " & "OPERATORS - 3");
   end if;

   for I in Ident (Intarray (0)) .. Ident (Intarray (2)) loop
      if not (I in Intarray (0) .. Intarray (2)) or
        (I not in Ident (0) .. Ident (126))
      then
         Failed ("INCORRECT RESULTS FOR MEMBERSHIP " & "OPERATORS - 3");
      end if;
   end loop;

   if not
     (((Intarray (0) + Intarray (2)) = Intarray (2)) and
      ((Intarray (2) - Intarray (1)) = Intarray (1)) and
      ((Intarray (1) * Ident (2)) = Intarray (2)) and
      ((Intarray (2) / Intarray (1)) = Ident (2)) and
      ((Intarray (1)**1) = Ident (63)) and
      ((Intarray (2) rem 10) = Ident (6)) and
      ((Intarray (1) mod 10) = Ident (3)))
   then
      Failed ("INCORRECT RESULTS FOR BINARY ARITHMETIC " & "OPERATORS - 3");
   end if;

   if Int'Pos (Intarray (0)) /= Ident_Int (0) or
     Int'Pos (Intarray (1)) /= Ident_Int (63) or
     Int'Pos (Intarray (2)) /= Ident_Int (126)
   then
      Failed ("INCORRECT VALUE FOR INT'POS - 3");
   end if;

   if Int'Succ (Intarray (0)) /= Ident (1) or
     Int'Succ (Intarray (1)) /= Ident (64)
   then
      Failed ("INCORRECT VALUE FOR INT'SUCC - 3");
   end if;

   if Int'Image (Intarray (0)) /= Ident_Str (" 0") or
     Int'Image (Intarray (1)) /= Ident_Str (" 63") or
     Int'Image (Intarray (2)) /= Ident_Str (" 126")
   then
      Failed ("INCORRECT VALUE FOR INT'IMAGE - 3");
   end if;

   if Irec.Comp2'Size < Ident_Int (Basic_Size) then
      Failed ("INCORRECT VALUE FOR IREC.COMP2'SIZE");
   end if;

   if not
     ((Irec.Comp0 < Ident (1)) and
      (Ident (Irec.Comp2) > Ident (Irec.Comp1)) and
      (Irec.Comp1 <= Ident (63)) and
      (Ident (126) = Irec.Comp2))
   then
      Failed ("INCORRECT RESULTS FOR RELATIONAL " & "OPERATORS - 4");
   end if;

   for I in Ident (Irec.Comp0) .. Ident (Irec.Comp2) loop
      if not (I in Irec.Comp0 .. Irec.Comp2) or
        (I not in Ident (0) .. Ident (126))
      then
         Failed ("INCORRECT RESULTS FOR MEMBERSHIP " & "OPERATORS - 4");
      end if;
   end loop;

   if not
     ((+Irec.Comp2 = Irec.Comp2) and
      (-Irec.Comp1 = -63) and
      (abs Irec.Comp2 = Irec.Comp2))
   then
      Failed ("INCORRECT RESULTS FOR UNARY ARITHMETIC " & "OPERATORS - 4");
   end if;

   if Int'Val (0) /= Ident (Irec.Comp0) or
     Int'Val (63) /= Ident (Irec.Comp1) or
     Int'Val (126) /= Ident (Irec.Comp2)
   then
      Failed ("INCORRECT VALUE FOR INT'VAL - 4");
   end if;

   if Int'Pred (Irec.Comp1) /= Ident (62) or
     Int'Pred (Irec.Comp2) /= Ident (125)
   then
      Failed ("INCORRECT VALUE FOR INT'PRED - 4");
   end if;

   if Int'Value ("0") /= Ident (Irec.Comp0) or
     Int'Value ("63") /= Ident (Irec.Comp1) or
     Int'Value ("126") /= Ident (Irec.Comp2)
   then
      Failed ("INCORRECT VALUE FOR INT'VALUE - 4");
   end if;

   Result;

end Cd2a32e;
