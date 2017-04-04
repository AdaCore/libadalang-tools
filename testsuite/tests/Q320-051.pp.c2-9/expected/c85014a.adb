-- C85014A.ADA

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
--     CHECK THAT THE NUMBER OF FORMAL PARAMETERS IS USED TO DETERMINE
--     WHICH SUBPROGRAM OR ENTRY IS BEING RENAMED.

-- HISTORY:
--     JET 03/24/88  CREATED ORIGINAL TEST.
--     BCB 04/18/90  CORRECTED ERROR MESSAGE FOR ENTRY2.
--     RLB 03/19/07  Fixed limited returns to be compatible with Amendment 1.

with Report; use Report;
procedure C85014a is

   task type T1 is
      entry Enter (I1 : in out Integer);
      entry Stop;
   end T1;

   task type T2 is
      entry Enter (I1, I2 : in out Integer);
      entry Stop;
   end T2;

   Task1 : aliased T1;
   Task2 : aliased T2;

   function F return access T1 is
   begin
      return Task1'Access;
   end F;

   function F return access T2 is
   begin
      return Task2'Access;
   end F;

   procedure Proc (I1 : in out Integer) is
   begin
      I1 := I1 + 1;
   end Proc;

   procedure Proc (I1, I2 : in out Integer) is
   begin
      I1 := I1 + 2;
      I2 := I2 + 2;
   end Proc;

   task body T1 is
      Accepting_Entries : Boolean := True;
   begin
      while Accepting_Entries loop
         select
            accept Enter (I1 : in out Integer) do
               I1 := I1 + 1;
            end Enter;
         or
            accept Stop do
               Accepting_Entries := False;
            end Stop;
         end select;
      end loop;
   end T1;

   task body T2 is
      Accepting_Entries : Boolean := True;
   begin
      while Accepting_Entries loop
         select
            accept Enter (I1, I2 : in out Integer) do
               I1 := I1 + 2;
               I2 := I2 + 2;
            end Enter;
         or
            accept Stop do
               Accepting_Entries := False;
            end Stop;
         end select;
      end loop;
   end T2;

begin
   Test
     ("C85014A",
      "CHECK THAT THE NUMBER OF FORMAL PARAMETERS IS " &
      "USED TO DETERMINE WHICH SUBPROGRAM OR ENTRY " &
      "IS BEING RENAMED");

   declare
      procedure Proc1 (J1 : in out Integer) renames Proc;
      procedure Proc2 (J1, J2 : in out Integer) renames Proc;

      procedure Entry1 (J1 : in out Integer) renames F.Enter;
      procedure Entry2 (J1, J2 : in out Integer) renames F.Enter;

      K1, K2 : Integer := 0;
   begin
      Proc1 (K1);
      if K1 /= Ident_Int (1) then
         Failed ("INCORRECT RETURN VALUE FROM PROC1");
      end if;

      Entry1 (K2);
      if K2 /= Ident_Int (1) then
         Failed ("INCORRECT RETURN VALUE FROM ENTRY1");
      end if;

      Proc2 (K1, K2);
      if K1 /= Ident_Int (3) or K2 /= Ident_Int (3) then
         Failed ("INCORRECT RETURN VALUE FROM PROC2");
      end if;

      Entry2 (K1, K2);
      if K1 /= Ident_Int (5) or K2 /= Ident_Int (5) then
         Failed ("INCORRECT RETURN VALUE FROM ENTRY2");
      end if;
   end;

   Task1.Stop;
   Task2.Stop;

   Result;
end C85014a;
