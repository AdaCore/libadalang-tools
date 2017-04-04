-- C48011A.ADA

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
--     CHECK THAT OVERLOADED ALLOCATORS ARE DETERMINED TO HAVE THE
--     APPROPRIATE TYPE.

-- HISTORY:
--     JET 08/17/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C48011a is

   type Acc1 is access Integer;
   type Acc2 is access Integer;

   A1 : Acc1 := null;
   A2 : Acc2 := null;

   type Rec1 is record
      A : Integer;
   end record;

   type Rec2 is record
      A : Acc2;
   end record;

   type Arec1 is access Rec1;
   type Arec2 is access Rec2;

   procedure Proc (A : Acc1) is
   begin
      if A.all /= 1 then
         Failed ("INCORRECT CALL OF FIRST PROC");
      end if;
   end Proc;

   procedure Proc (A : Integer) is
   begin
      if A /= 2 then
         Failed ("INCORRECT CALL OF SECOND PROC");
      end if;
   end Proc;

   function Func (I : Integer) return Arec1 is
   begin
      if I /= 1 then
         Failed ("INCORRECT CALL OF FIRST FUNC");
      end if;
      return new Rec1'(A => 0);
   end Func;

   function Func (I : Integer) return Arec2 is
   begin
      if I /= 2 then
         Failed ("INCORRECT CALL OF SECOND FUNC");
      end if;
      return new Rec2'(A => null);
   end Func;

begin
   Test
     ("C48011A",
      "CHECK THAT OVERLOADED ALLOCATORS ARE " &
      "DETERMINED TO HAVE THE APPROPRIATE TYPE");

   if A1 = new Integer'(1) then
      Failed ("INCORRECT RETURN VALUE FROM ALLOCATOR 1");
   end if;

   if A2 = new Integer'(2) then
      Failed ("INCORRECT RETURN VALUE FROM ALLOCATOR 2");
   end if;

   Func (1).A             := Integer'(1);
   Func (Ident_Int (2)).A := new Integer'(2);

   Proc (new Integer'(Ident_Int (1)));
   Proc (Ident_Int (2));

   Result;
end C48011a;
