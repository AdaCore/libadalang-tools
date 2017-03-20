-- C36204A.ADA

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
-- CHECK THAT EACH ARRAY ATTRIBUTE YIELDS THE CORRECT VALUES.
-- BOTH ARRAY OBJECTS AND TYPES ARE CHECKED.

-- DAT 2/12/81
-- SPS 11/1/82
-- WMC 03/16/92  CREATED TYPE RANGE CHECK FOR AE_TYPE.

with Report;
procedure C36204a is

   use Report;

begin
   Test ("C36204A", "ARRAY ATTRIBUTES RETURN CORRECT VALUES");

   declare
      A1 : array
        (Boolean, Integer range Ident_Int (1) .. Ident_Int (10)) of String
        (Ident_Int (5) .. Ident_Int (7));
      type Ni is range -3 .. 3;
      N : Ni := Ni (Ident_Int (2));
      subtype Sni is Ni range -N .. N;
      type Aa is array (Ni, Sni, Boolean) of Ni;
      A1_1_1 : Boolean                    := A1'First;
      A1_1_2 : Boolean                    := A1'Last (1);
      A1_2_1 : Integer range A1'Range (2) := A1'First (2);  -- 1
      A1_2_2 : Integer range A1'Range (2) := A1'Last (2);   -- 10
      subtype Ae_Type is Integer range A1 (True, 5)'Range;    -- RANGE 5..7
      A2 : Aa;
      A4 : array
        (A1_1_1 ..
             A1_1_2,
           A1_2_1 ..
             A1_2_2) of String (Ident_Int (1) .. Ident_Int (3));

      I : Integer;
      B : Boolean;
   begin
      if A4'First /= Ident_Bool (False) or
        A4'Last /= Ident_Bool (True) or
        A4'First (2) /= Integer'(1) or
        A4'Last (2) /= Integer'(10)
      then
         Failed ("INCORRECT 'FIRST OR 'LAST  - 1");
      end if;

      if A4'Length /= Integer'(2) or
        A4'Length /= Ni'(2) or
        A4'Length (1) /= N or
        A4'Length (2) /= A4'Last (2)
      then
         Failed ("INCORRECT 'LENGTH - 1");
      end if;

      A4 := (Boolean => (1 .. 10 => "XYZ"));
      for L1 in A1'Range (1) loop
         for L2 in A4'Range (2) loop
            A1 (L1, L2) := A4 (L1, L2);
         end loop;
      end loop;

      if Aa'First (1) /= Ni'(-3) or
        Aa'Last (1) /= N + 1 or
        Aa'First (2) /= -N or
        Aa'Last (2) /= N or
        Aa'First (3) /= Ident_Bool (False) or
        Aa'Last (3) /= Ident_Bool (True)
      then
         Failed ("INCORRECT 'FIRST OR 'LAST - 2");
      end if;

      if N not in Aa'Range (2) or
        Ident_Bool (False) not in Aa'Range (3) or
        N + 1 not in Aa'Range or
        N + 1 in Aa'Range (2)
      then
         Failed ("INCORRECT 'RANGE - 1");
      end if;

      if Aa'Length /= Integer'(7) or
        Aa'Length (2) - 3 /= N or
        Aa'Length (3) /= 2
      then
         Failed ("INCORRECT 'LENGTH - 2");
      end if;

      if A2'First (1) /= Ni'(-3) or
        A2'Last (1) /= N + 1 or
        A2'First (2) /= -N or
        A2'Last (2) /= N or
        A2'First (3) /= Ident_Bool (False) or
        A2'Last (3) /= Ident_Bool (True)
      then
         Failed ("INCORRECT 'FIRST OR 'LAST - 3");
      end if;

      if N not in A2'Range (2) or
        Ident_Bool (False) not in A2'Range (3) or
        N + 1 not in A2'Range or
        N + 1 in A2'Range (2)
      then
         Failed ("INCORRECT 'RANGE - 2");
      end if;

      if A2'Length /= Integer'(7) or
        A2'Length (2) - 3 /= Integer (N) or
        A2'Length (3) /= 2
      then
         Failed ("INCORRECT 'LENGTH - 3");
      end if;

      if (Ae_Type'First /= 5) or (Ae_Type'Last /= 7) then
         Failed ("INCORRECT TYPE RANGE DEFINED FOR AE_TYPE");
      end if;
   exception
      when others =>
         Failed ("EXCEPTION RAISED ?");
   end;

   Result;
end C36204a;
