-- C36172B.ADA

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
-- CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A MULTIDIMENSIONAL INDEX
-- CONSTRAINT IF ONE OF THE RANGES IS A NULL RANGE AND THE OTHER IS A
-- NON-NULL RANGE WITH A BOUND THAT LIES OUTSIDE THE INDEX SUBTYPE.

-- CHECK THAT NO EXCEPTION IS RAISED IF ALL DISCRETE RANGES ARE NULL.

-- JBG 6/5/85
-- EDS 7/16/98 AVOID OPTIMIZATION

with Report; use Report;
procedure C36172b is
   subtype Int_10 is Integer range 1 .. 10;
   type Arr2 is array (Int_10 range <>, Int_10 range <>) of Integer;
begin
   Test
     ("C36172B",
      "CHECK THAT CONSTRAINT_ERROR IS RAISED FOR A " &
      "NON-NULL DIMENSION OF A NULL MULTIDIMENSIONAL " &
      "INDEX CONSTRAINT IF A BOUND LIES OUTSIDE THE " & "INDEX SUBTYPE");

   begin
      declare
         V : Arr2 (6 .. 4, 9 .. 11);
      begin
         Failed
           ("EXCEPTION NOT RAISED WHEN NON-NULL RANGE OF " &
            "NULL INDEX CONSTRAINT HAS A BOUND OUTSIDE " &
            "THE INDEX SUBTYPE (13) " & Integer'Image (V'First));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 13");
   end;

   begin
      declare
         V : Arr2 (0 .. 3, 8 .. 7);
      begin
         Failed
           ("EXCEPTION NOT RAISED WHEN NON-NULL RANGE OF " &
            "NULL INDEX CONSTRAINT HAS A BOUND OUTSIDE " &
            "THE INDEX SUBTYPE (14) " & Integer'Image (V'First));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 14");
   end;

   begin
      declare
         V : Arr2 (6 .. 4, Ident_Int (0) .. 2);
      begin
         Failed
           ("EXCEPTION NOT RAISED WHEN NON-NULL RANGE OF " &
            "NULL INDEX CONSTRAINT HAS A BOUND OUTSIDE " &
            "THE INDEX SUBTYPE (15) " & Integer'Image (V'First));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 15");
   end;

   begin
      declare
         V : Arr2 (9 .. Ident_Int (11), 6 .. 4);
      begin
         Failed
           ("EXCEPTION NOT RAISED WHEN NON-NULL RANGE OF " &
            "NULL INDEX CONSTRAINT HAS A BOUND OUTSIDE " &
            "THE INDEX SUBTYPE (16) " & Integer'Image (V'First));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 16");
   end;

   begin
      declare
         V : Arr2 (6 .. Ident_Int (4), 9 .. Ident_Int (11));
      begin
         Failed
           ("EXCEPTION NOT RAISED WHEN NON-NULL RANGE OF " &
            "NULL INDEX CONSTRAINT HAS A BOUND OUTSIDE " &
            "THE INDEX SUBTYPE (17) " & Integer'Image (V'First));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 17");
   end;

   begin
      declare
         V : Arr2 (Ident_Int (-1) .. 2, Ident_Int (6) .. 4);
      begin
         Failed
           ("EXCEPTION NOT RAISED WHEN NON-NULL RANGE OF " &
            "NULL INDEX CONSTRAINT HAS A BOUND OUTSIDE " &
            "THE INDEX SUBTYPE (18) " & Integer'Image (V'First));
      end;
   exception
      when Constraint_Error =>
         null;
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED - 18");
   end;

   begin
      declare
         V : Arr2 (6 .. -1, 11 .. 9);
      begin
         if not Equal (V'First, V'First) then
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED FOR NULL CONSTRAINT - 19");
   end;

   begin
      declare
         V : Arr2 (Ident_Int (11) .. 9, 6 .. Ident_Int (0));
      begin
         if not Equal (V'First, V'First) then
            Failed ("IMPOSSIBLE");
         end if;
      end;
   exception
      when others =>
         Failed ("EXCEPTION RAISED FOR NULL CONSTRAINT - 20");
   end;

   Result;
end C36172b;
