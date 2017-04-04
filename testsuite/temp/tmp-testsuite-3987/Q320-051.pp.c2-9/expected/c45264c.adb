-- C45264C.ADA

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
-- CHECK THAT COMPARING ARRAYS OF DIFFERENT LENGTHS DOES NOT RAISE AN
-- EXCEPTION.

-- TBN  7/21/86

with Report; use Report;
procedure C45264c is

   subtype Int is Integer range 1 .. 10;
   type Array_Type_1 is array (Int range <>) of Integer;
   type Array_Type_2 is array (Int range <>, Int range <>) of Integer;
   type Array_Type_3 is
     array (Int range <>, Int range <>, Int range <>) of Integer;

   Array_1 : Array_Type_1 (1 .. 5)                 := (1 .. 5 => 1);
   Array_2 : Array_Type_1 (1 .. 7)                 := (1 .. 7 => 1);
   Array_3 : Array_Type_2 (1 .. 5, 1 .. 4) := (1 .. 5 => (1 .. 4 => 1));
   Array_4 : Array_Type_2 (1 .. 2, 1 .. 3) := (1 .. 2 => (1 .. 3 => 1));
   Array_5 : Array_Type_3 (1 .. 2, 1 .. 3, 1 .. 2) :=
     (1 .. 2 => (1 .. 3 => (1 .. 2 => 2)));
   Array_6 : Array_Type_3 (1 .. 1, 1 .. 2, 1 .. 3) :=
     (1 .. 1 => (1 .. 2 => (1 .. 3 => 2)));
   Array_7  : Array_Type_2 (1 .. 5, 1 .. 4) := (1 .. 5 => (1 .. 4 => 3));
   Array_8  : Array_Type_2 (1 .. 5, 1 .. 3) := (1 .. 5 => (1 .. 3 => 3));
   Array_9  : Array_Type_2 (1 .. 3, 1 .. 2) := (1 .. 3 => (1 .. 2 => 4));
   Array_10 : Array_Type_2 (1 .. 2, 1 .. 2) := (1 .. 2 => (1 .. 2 => 4));

begin
   Test
     ("C45264C",
      "CHECK THAT COMPARING ARRAYS OF DIFFERENT " &
      "LENGTHS DOES NOT RAISE AN EXCEPTION");

   begin     -- (A)
      if "=" (Array_1 (1 .. Integer'First), Array_2) then
         Failed
           ("INCORRECT RESULTS FROM COMPARING ONE " &
            "DIMENSIONAL ARRAYS - 1");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED EVALUATING - 1");
   end;     -- (A)

   begin     -- (B)
      if Array_1 /= Array_2 then
         null;
      else
         Failed
           ("INCORRECT RESULTS FROM COMPARING ONE " &
            "DIMENSIONAL ARRAYS - 2");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED EVALUATING - 2");
   end;     -- (B)

   begin     -- (C)
      if Array_3 = Array_4 then
         Failed
           ("INCORRECT RESULTS FROM COMPARING MULTI-" &
            "DIMENSIONAL ARRAYS - 3");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED EVALUATING - 3");
   end;     -- (C)

   begin     -- (D)
      if "/=" (Array_3, Array_4) then
         null;
      else
         Failed
           ("INCORRECT RESULTS FROM COMPARING MULT-" &
            "DIMENSIONAL ARRAYS - 4");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED - 4");
   end;     -- (D)

   begin     -- (E)
      if "=" (Array_5, Array_6) then
         Failed
           ("INCORRECT RESULTS FROM COMPARING MULTI-" &
            "DIMENSIONAL ARRAYS - 5");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED EVALUATING - 5");
   end;     -- (E)

   begin     -- (F)
      if Array_6 /= Array_5 then
         null;
      else
         Failed
           ("INCORRECT RESULTS FROM COMPARING MULT-" &
            "DIMENSIONAL ARRAYS - 6");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED - 6");
   end;     -- (F)

   begin     -- (G)
      if Array_7 = Array_8 then
         Failed
           ("INCORRECT RESULTS FROM COMPARING MULTI-" &
            "DIMENSIONAL ARRAYS - 7");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED EVALUATING - 7");
   end;     -- (G)

   begin     -- (H)
      if Array_9 /= Array_10 then
         null;
      else
         Failed
           ("INCORRECT RESULTS FROM COMPARING MULTI-" &
            "DIMENSIONAL ARRAYS - 8");
      end if;

   exception
      when others =>
         Failed ("EXCEPTION RAISED EVALUATING - 8");
   end;     -- (H)

   Result;
end C45264c;
