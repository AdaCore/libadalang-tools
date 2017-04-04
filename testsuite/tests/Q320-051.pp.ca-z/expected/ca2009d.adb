-- CA2009D.ADA

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
-- CHECK THAT A GENERIC SUBPROGRAM SUBUNIT CAN BE SPECIFIED AND INSTANTIATED.

-- BHS 8/01/84
-- JRK 5/24/85 CHANGED TO .ADA, SEE AI-00323.

with Report; use Report;
procedure Ca2009d is

   Int1 : Integer := 1;
   Int2 : Integer := 2;

   generic
      type Elem is private;
      Pcon1 : in Elem;
      Pvar1 : in out Elem;
   procedure Proc1;

   generic
      type Obj is private;
      Fcon1 : in Obj;
      Fvar1 : in out Obj;
   function Func1 return Obj;

   procedure Proc1 is separate;
   function Func1 return Obj is separate;

   procedure Ni_Proc1 is new Proc1 (Integer, 2, Int1);
   function Ni_Func1 is new Func1 (Integer, 3, Int2);

begin

   Test
     ("CA2009D",
      "SPECIFICATION AND INSTANTIATION " & "OF GENERIC SUBPROGRAM SUBUNITS");

   Ni_Proc1;
   if Int1 /= 2 then
      Failed ("INCORRECT INSTANTIATION - NI_PROC1");
   end if;

   if Ni_Func1 /= 3 then
      Failed ("INCORRECT INSTANTIATION - NI_FUNC1");
   end if;

   Result;

end Ca2009d;
