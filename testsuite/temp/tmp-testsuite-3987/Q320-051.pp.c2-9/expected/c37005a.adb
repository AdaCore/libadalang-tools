-- C37005A.ADA

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
-- CHECK THAT SCALAR RECORD COMPONENTS MAY HAVE NON-STATIC
-- RANGE CONSTRAINTS OR DEFAULT INITIAL VALUES.

-- DAT 3/6/81
-- JWC 6/28/85   RENAMED TO -AB
-- EDS 7/16/98   AVOID OPTIMIZATION

with Report;
procedure C37005a is

   use Report;

begin
   Test
     ("C37005A",
      "SCALAR RECORD COMPONENTS MAY HAVE NON-STATIC" &
      " RANGE CONSTRAINTS OR DEFAULT INITIAL VALUES");

   declare
      subtype Dt is Integer range Ident_Int (1) .. Ident_Int (5);
      L : Integer := Ident_Int (Dt'First);
      R : Integer := Ident_Int (Dt'Last);
      subtype Dt2 is Integer range L .. R;
      M : Integer := (L + R) / 2;

      type Rec is record
         C1 : Integer                          := M;
         C2 : Dt2                              := (L + R) / 2;
         C3 : Boolean range (L < M) .. (R > M) := Ident_Bool (True);
         C4 : Integer range L .. R             := Dt'First;
      end record;

      R1, R2 : Rec := ((L + R) / 2, M, M in Dt, L);
      R3     : Rec;
   begin
      if R3 /= R1 then
         Failed ("INCORRECT RECORD VALUES");
      end if;

      R3 := (R2.C2, R2.C1, R3.C3, R);  -- CONSTRAINTS CHECKED BY :=
      if Equal (Ident_Int (1), 2) then
         Failed ("IMPOSSIBLE " & Integer'Image (R3.C1));  --USE R3
      end if;

      begin
         R3 := (M, M, Ident_Bool (False), M); -- RAISES CON_ERR.
         Failed ("CONSTRAINT ERROR NOT RAISED " & Integer'Image (R3.C1));
      exception
         when Constraint_Error =>
            null;
         when others =>
            Failed ("WRONG EXCEPTION");
      end;

      for I in Dt loop
         R3    := (I, I, I /= 100, I);
         R1.C2 := I;
         if Equal (Ident_Int (1), 2) then
            Failed
              ("IMPOSSIBLE " & Integer'Image (R3.C1 + R1.C2));  --USE R3, R1
         end if;
      end loop;

   exception
      when others =>
         Failed ("INVALID EXCEPTION");
   end;

   Result;
end C37005a;
