-- C85014C.ADA

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
--     CHECK THAT THE PRESENCE OR ABSENCE OF A RESULT TYPE IS USED TO
--     DETERMINE WHICH SUBPROGRAM OR ENTRY IS BEING RENAMED.

-- HISTORY:
--     JET 03/24/88  CREATED ORIGINAL TEST.
--     RLB 03/19/07  Fixed limited returns to be compatible with Amendment 1.

with Report; use Report;
procedure C85014c is

   I, J : Integer;

   task type T is
      entry Q (I1 : Integer);
   end T;

   Task0 : aliased T;

   package Func is
      function Q (I1 : Integer) return Integer;
      function Func return access T;
   end Func;
   use Func;

   procedure Proc (I1 : Integer) is
   begin
      I := I1;
   end Proc;

   function Proc (I1 : Integer) return Integer is
   begin
      I := I1 + 1;
      return 0;
   end Proc;

   task body T is
   begin
      accept Q (I1 : Integer) do
         I := I1;
      end Q;
   end T;

   package body Func is
      function Q (I1 : Integer) return Integer is
      begin
         I := I1 + 1;
         return 0;
      end Q;

      function Func return access T is
      begin
         return Task0'Access;
      end Func;
   end Func;

begin
   Test
     ("C85014C",
      "CHECK THAT THE PRESENCE OR ABSENCE OF A " &
      "RESULT TYPE IS USED TO DETERMINE WHICH " &
      "SUBPROGRAM OR ENTRY IS BEING RENAMED");

   declare
      procedure Proc1 (J1 : Integer) renames Proc;

      function Proc2 (J1 : Integer) return Integer renames Proc;
   begin
      Proc1 (1);
      if I /= Ident_Int (1) then
         Failed ("INCORRECT VALUE OF I AFTER PROC1");
      end if;

      J := Proc2 (1);
      if I /= Ident_Int (2) then
         Failed ("INCORRECT VALUE OF I AFTER PROC2");
      end if;
   end;

   declare
      procedure Func1 (J1 : Integer) renames Func.Func.Q;

      function Func2 (J1 : Integer) return Integer renames Func.Q;
   begin
      Func1 (1);
      if I /= Ident_Int (1) then
         Failed ("INCORRECT VALUE OF I AFTER FUNC1");
      end if;

      J := Func2 (1);
      if I /= Ident_Int (2) then
         Failed ("INCORRECT VALUE OF I AFTER FUNC2");
      end if;
   end;

   Result;
end C85014c;
