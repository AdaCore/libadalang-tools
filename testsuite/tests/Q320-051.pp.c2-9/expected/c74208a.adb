-- C74208A.ADA

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
--     CHECK THAT 'SIZE AND 'ADDRESS FOR OBJECTS OF LIMITED AND
--     NON-LIMITED TYPES ARE AVAILABLE BOTH INSIDE AND OUTSIDE THE
--     PACKAGE DECLARING THE TYPES.

-- HISTORY:
--     BCB 03/14/88  CREATED ORIGINAL TEST.

with Report; use Report;
with System; use System;

procedure C74208a is

   package P is
      type T is private;
      type U is limited private;
   private
      type T is range 1 .. 100;
      type U is range 1 .. 100;
   end P;

   A                  : P.T;
   B                  : P.U;
   Asize, Bsize       : Integer;
   Aaddress, Baddress : Address;

   function Ident_Adr (X : Address) return Address is
      Y : P.T;
   begin
      if Equal (3, 3) then
         return X;
      end if;
      return Y'Address;
   end Ident_Adr;

   package body P is
      X                  : T;
      Y                  : U;
      Xsize, Ysize       : Integer;
      Xaddress, Yaddress : Address;
   begin
      Test
        ("C74208A",
         "CHECK THAT 'SIZE AND 'ADDRESS FOR " &
         "OBJECTS OF LIMITED AND NON-LIMITED TYPES " &
         "ARE AVAILABLE BOTH INSIDE AND OUTSIDE " &
         "THE PACKAGE DECLARING THE TYPES");

      Xsize    := X'Size;
      Ysize    := Y'Size;
      Xaddress := X'Address;
      Yaddress := Y'Address;

      if not Equal (Xsize, X'Size) then
         Failed ("IMPROPER VALUE FOR X'SIZE");
      end if;

      if Xaddress /= Ident_Adr (X'Address) then
         Failed ("IMPROPER VALUE FOR X'ADDRESS");
      end if;

      if not Equal (Ysize, Y'Size) then
         Failed ("IMPROPER VALUE FOR Y'SIZE");
      end if;

      if Yaddress /= Ident_Adr (Y'Address) then
         Failed ("IMPROPER VALUE FOR Y'ADDRESS");
      end if;
   end P;

begin
   Asize    := A'Size;
   Bsize    := B'Size;
   Aaddress := A'Address;
   Baddress := B'Address;

   if not Equal (Asize, A'Size) then
      Failed ("IMPROPER VALUE FOR A'SIZE");
   end if;

   if Aaddress /= Ident_Adr (A'Address) then
      Failed ("IMPROPER VALUE FOR A'ADDRESS");
   end if;

   if not Equal (Bsize, B'Size) then
      Failed ("IMPROPER VALUE FOR B'SIZE");
   end if;

   if Baddress /= Ident_Adr (B'Address) then
      Failed ("IMPROPER VALUE FOR B'ADDRESS");
   end if;

   Result;
end C74208a;
