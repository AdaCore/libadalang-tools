-- C43204A.ADA

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
--     A SUBPROGRAM CALL WHEN THE FORMAL PARAMETER IS CONSTRAINED.

-- HISTORY:
--     JET 08/04/88  CREATED ORIGINAL TEST.

with Report; use Report;
procedure C43204a is

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

   procedure Proc10 (A : Arr10) is
   begin
      if A'Length /= Ident_Int (0) then
         Failed ("PROC10 ARRAY IS NOT NULL");
      end if;
   end Proc10;

   procedure Proc11 (A : Arr11; C : Integer) is
   begin
      if A'Length /= Ident_Int (7) or A'First /= Ident_Int (-3) or
        A'Last /= Ident_Int (3) then
         Failed ("INCORRECT LENGTH IN PROC11 CALL NUMBER" & Integer'Image (C));
      end if;

      for I in Ident_Int (-3) .. Ident_Int (3) loop
         if Ident_Int (A (I)) /= C then
            Failed
              ("INCORRECT VALUE OF COMPONENT " & Integer'Image (I) &
               ", PROC11 CALL NUMBER" & Integer'Image (C));
         end if;
      end loop;
   end Proc11;

   procedure Proc12 (A : Arr12) is
   begin
      if A'Length /= Ident_Int (7) then
         Failed ("INCORRECT LENGTH IN PROC12");
      end if;

      for I in Ident_Int (-3) .. Ident_Int (3) loop
         if Ident_Int (A (I)) /= 3 then
            Failed
              ("INCORRECT VALUE OF COMPONENT " & Integer'Image (I) &
               ", PROC12");
         end if;
      end loop;
   end Proc12;

   procedure Proc20 (A : Arr20) is
   begin
      if A'Length (1) /= Ident_Int (0) or A'Length (2) /= Ident_Int (0) then
         Failed ("PROC20 ARRAY IS NOT NULL");
      end if;
   end Proc20;

   procedure Proc21 (A : Arr21; C : Integer) is
   begin
      for I in Integer'(-1) .. 1 loop
         for J in Integer'(-1) .. 1 loop
            if Ident_Int (A (I, J)) /= C then
               Failed
                 ("INCORRECT VALUE OF COMPONENT (" & Integer'Image (I) & "," &
                  Integer'Image (J) & "), PROC21 CALL " & "NUMBER" &
                  Integer'Image (C));
            end if;
         end loop;
      end loop;
   end Proc21;

   procedure Proc22 (A : Arr22) is
   begin
      for I in Integer'(-1) .. 1 loop
         for J in Integer'(-1) .. 1 loop
            if Ident_Int (A (I, J)) /= 5 then
               Failed
                 ("INCORRECT VALUE OF COMPONENT (" & Integer'Image (I) & "," &
                  Integer'Image (J) & "), PROC22");
            end if;
         end loop;
      end loop;
   end Proc22;

   procedure Proc23 (A : Arr23) is
   begin
      for I in Integer'(-1) .. 1 loop
         for J in Integer'(-1) .. 1 loop
            if Ident_Int (A (I, J)) /= 7 then
               Failed
                 ("INCORRECT VALUE OF COMPONENT (" & Integer'Image (I) & "," &
                  Integer'Image (J) & "), PROC23");
            end if;
         end loop;
      end loop;
   end Proc23;

begin
   Test
     ("C43204A",
      "CHECK THAT AN ARRAY AGGREGATE WITH AN OTHERS " &
      "CHOICE CAN APPEAR (AND BOUNDS ARE DETERMINED " &
      "CORRECTLY) AS AN ACTUAL PARAMETER OF A " &
      "SUBPROGRAM CALL WHEN THE FORMAL PARAMETER IS " & "CONSTRAINED");

   Proc11 ((1, 1, 1, others => 1), 1);
   Proc11 ((2 => 2, 3 => 2, others => 2), 2);
   Proc12 ((others => 3));
   Proc10 ((others => 4));

   Proc21 (((1, 1, 1), others => (1, 1, 1)), 1);
   Proc21 ((1 => (2, 2, 2), others => (2, 2, 2)), 2);
   Proc21 (((3, others => 3), (3, others => 3), (3, 3, others => 3)), 3);
   Proc21
     (((-1 => 4, others => 4), (0 => 4, others => 4), (1 => 4, others => 4)),
      4);
   Proc22 ((others => (others => 5)));
   Proc20 ((others => (others => 6)));
   Proc23 ((others => (7, 7, 7)));

   Result;
end C43204a;
