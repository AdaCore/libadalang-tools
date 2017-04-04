-- C85014B.ADA

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
--     CHECK THAT THE BASE TYPE OF THE FORMAL PARAMETER AND THE RESULT
--     TYPE ARE USED TO DETERMINE WHICH SUBPROGRAM OR ENTRY IS BEING
--     RENAMED.

-- HISTORY:
--     JET 03/24/88  CREATED ORIGINAL TEST.
--     RLB 03/19/07  Fixed limited returns to be compatible with Amendment 1.

with Report; use Report;
procedure C85014b is

   type Int is new Integer;
   subtype Subint0 is Int range 0 .. Int'Last;
   subtype Subint1 is Int range 1 .. Int'Last;

   task type T1 is
      entry Enter (I1 : in out Integer);
      entry Stop;
   end T1;

   task type T2 is
      entry Enter (I1 : in out Int);
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

   procedure Proc (I1 : in out Int) is
   begin
      I1 := I1 + 2;
   end Proc;

   function Funk (I1 : Integer) return Integer is
   begin
      return I1 + 1;
   end Funk;

   function Funk (I1 : Integer) return Int is
   begin
      return Int (I1) + 2;
   end Funk;

   function Funkx (N : Natural) return Positive is
   begin
      return N + 1;
   end Funkx;

   function Funkx (N : Subint0) return Subint1 is
   begin
      return N + 2;
   end Funkx;

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
            accept Enter (I1 : in out Int) do
               I1 := I1 + 2;
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
     ("C85014B",
      "CHECK THAT THE BASE TYPE OF THE FORMAL " &
      "PARAMETER AND THE RESULT TYPE ARE USED TO " &
      "DETERMINE WHICH SUBPROGRAM OR ENTRY IS BEING " &
      "RENAMED");

   declare
      procedure Proc1 (J1 : in out Integer) renames Proc;
      procedure Proc2 (J1 : in out Int) renames Proc;

      function Funk1 (J1 : Integer) return Integer renames Funk;
      function Funk2 (J1 : Integer) return Int renames Funk;

      procedure Entry1 (J1 : in out Integer) renames F.Enter;
      procedure Entry2 (J1 : in out Int) renames F.Enter;

      function Funk3 (J1 : Positive) return Natural renames Funkx;
      function Funk4 (J1 : Subint1) return Subint0 renames Funkx;

      K1 : Integer := 0;
      K2 : Int     := 0;
   begin
      Proc1 (K1);
      if K1 /= Ident_Int (1) then
         Failed ("INCORRECT RETURN VALUE FROM PROC1");
      end if;

      K1 := Funk1 (K1);
      if K1 /= Ident_Int (2) then
         Failed ("INCORRECT RETURN VALUE FROM FUNK1");
      end if;

      Entry1 (K1);
      if K1 /= Ident_Int (3) then
         Failed ("INCORRECT RETURN VALUE FROM ENTRY1");
      end if;

      K1 := Funk3 (K1);
      if K1 /= Ident_Int (4) then
         Failed ("INCORRECT RETURN VALUE FROM FUNK3");
      end if;

      Proc2 (K2);
      if Integer (K2) /= Ident_Int (2) then
         Failed ("INCORRECT RETURN VALUE FROM PROC2");
      end if;

      K2 := Funk2 (Integer (K2));
      if Integer (K2) /= Ident_Int (4) then
         Failed ("INCORRECT RETURN VALUE FROM FUNK2");
      end if;

      Entry2 (K2);
      if Integer (K2) /= Ident_Int (6) then
         Failed ("INCORRECT RETURN VALUE FROM ENTRY2");
      end if;

      K2 := Funk4 (K2);
      if Integer (K2) /= Ident_Int (8) then
         Failed ("INCORRECT RETURN VALUE FROM FUNK4");
      end if;
   end;

   Task1.Stop;
   Task2.Stop;

   Result;
end C85014b;
