-- C43204C.ADA

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
--     CHECK THAT AN ARRAY AGGREGATE WITH AN OTHERS CHOICE CAN APPEAR
--     (AND BOUNDS ARE DETERMINED CORRECTLY) AS AN ACTUAL PARAMETER OF
--     A GENERIC INSTANTIATION WHEN THE GENERIC FORMAL PARAMETER IS
--     CONSTRAINED.

-- HISTORY:
--     JET 08/15/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C43204c is

   type Arr10 is array (Ident_Int (1) .. Ident_Int (0)) of Integer;
   type Arr11 is array (Integer range -3 .. 3) of Integer;
   type Arr12 is array (Ident_Int (-3) .. Ident_Int (3)) of Integer;

   type Arr20 is
     array
       (Ident_Int (1) .. Ident_Int (0),
        Ident_Int (0) .. Ident_Int (-1)) of Integer;
   type Arr21 is
     array (Integer range -1 .. 1, Integer range -1 .. 1) of Integer;
   type Arr22 is
     array
       (Ident_Int (-1) .. Ident_Int (1),
        Ident_Int (-1) .. Ident_Int (1)) of Integer;
   type Arr23 is
     array (Integer'(-1) .. 1, Ident_Int (-1) .. Ident_Int (1)) of Integer;

   generic
      A : Arr10;
   procedure Gproc10;

   generic
      A : Arr11;
   procedure Gproc11;

   generic
      A : Arr12;
   procedure Gproc12;

   generic
      A : Arr20;
   procedure Gproc20;

   generic
      A : Arr21;
   procedure Gproc21 (C : Integer);

   generic
      A : Arr22;
   procedure Gproc22;

   generic
      A : Arr23;
   procedure Gproc23;

   procedure Gproc10 is
   begin
      if A'Length /= Ident_Int (0) then
         Failed ("PROC10 ARRAY IS NOT NULL");
      end if;
   end Gproc10;

   procedure Gproc11 is
   begin
      if A'Length /= Ident_Int (7) or A'First /= Ident_Int (-3) or
        A'Last /= Ident_Int (3) then
         Failed ("INCORRECT LENGTH IN PROC11");
      end if;

      for I in Ident_Int (-3) .. Ident_Int (3) loop
         if Ident_Int (A (I)) /= 1 then
            Failed
              ("INCORRECT VALUE OF COMPONENT " & Integer'Image (I) &
               ", PROC11");
         end if;
      end loop;
   end Gproc11;

   procedure Gproc12 is
   begin
      if A'Length /= Ident_Int (7) then
         Failed ("INCORRECT LENGTH IN PROC12");
      end if;

      for I in Ident_Int (-3) .. Ident_Int (3) loop
         if Ident_Int (A (I)) /= 2 then
            Failed
              ("INCORRECT VALUE OF COMPONENT " & Integer'Image (I) &
               ", PROC12");
         end if;
      end loop;
   end Gproc12;

   procedure Gproc20 is
   begin
      if A'Length (1) /= Ident_Int (0) or A'Length (2) /= Ident_Int (0) then
         Failed ("GPROC20 ARRAY IS NOT NULL");
      end if;
   end Gproc20;

   procedure Gproc21 (C : Integer) is
   begin
      for I in Integer'(-1) .. 1 loop
         for J in Integer'(-1) .. 1 loop
            if Ident_Int (A (I, J)) /= C then
               Failed
                 ("INCORRECT VALUE OF COMPONENT (" & Integer'Image (I) & "," &
                  Integer'Image (J) & "), GPROC21 CALL " & "NUMBER" &
                  Integer'Image (C));
            end if;
         end loop;
      end loop;
   end Gproc21;

   procedure Gproc22 is
   begin
      for I in Integer'(-1) .. 1 loop
         for J in Integer'(-1) .. 1 loop
            if Ident_Int (A (I, J)) /= 3 then
               Failed
                 ("INCORRECT VALUE OF COMPONENT (" & Integer'Image (I) & "," &
                  Integer'Image (J) & "), GPROC22");
            end if;
         end loop;
      end loop;
   end Gproc22;

   procedure Gproc23 is
   begin
      for I in Integer'(-1) .. 1 loop
         for J in Integer'(-1) .. 1 loop
            if Ident_Int (A (I, J)) /= 4 then
               Failed
                 ("INCORRECT VALUE OF COMPONENT (" & Integer'Image (I) & "," &
                  Integer'Image (J) & "), GPROC23");
            end if;
         end loop;
      end loop;
   end Gproc23;

   procedure Proc11 is new Gproc11 ((1, 1, 1, others => 1));
   procedure Proc12 is new Gproc12 ((others => 2));
   procedure Proc10 is new Gproc10 ((others => 3));

   procedure Proc21 is new Gproc21 (((1, 1, 1), others => (1, 1, 1)));
   procedure Proc22 is new Gproc21
     (((2, others => 2), (2, others => 2), (2, 2, others => 2)));
   procedure Proc23 is new Gproc22 ((others => (others => 3)));
   procedure Proc24 is new Gproc23 ((others => (4, 4, 4)));
   procedure Proc20 is new Gproc20 ((others => (others => 5)));

begin
   Test
     ("C43204C",
      "CHECK THAT AN ARRAY AGGREGATE WITH AN OTHERS " &
      "CHOICE CAN APPEAR (AND BOUNDS ARE DETERMINED " &
      "CORRECTLY) AS AN ACTUAL PARAMETER OF A " &
      "SUBPROGRAM CALL WHEN THE FORMAL PARAMETER IS " & "CONSTRAINED");

   Proc11;
   Proc12;
   Proc10;

   Proc21 (1);
   Proc22 (2);
   Proc23;
   Proc24;
   Proc20;

   Result;
end C43204c;
