-- C36204B.ADA

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
--     CHECK THAT EACH ARRAY ATTRIBUTE YIELDS THE CORRECT VALUES WITH
--     ACCESS VALUES AND FUNCTION CALLS AS THE PREFIXES.

-- HISTORY:
--     L.BROWN   08/05/86
--     DWC  07/24/87  DELETED BLANK AT END OF TEST DESCRIPTION.

with Report; use Report;

procedure C36204b is

begin
   Test
     ("C36204B",
      "ARRAY ATTRIBUTES RETURN CORRECT VALUES " &
      "FOR ACCESS VALUES AND FUNCTION CALLS AS " & "PREFIXES");
   declare
      type Arr1 is
        array (Integer range Ident_Int (1) .. Ident_Int (10)) of Integer;
      type Arr2 is
        array
          (Boolean, Integer range Ident_Int (1) .. Ident_Int (3)) of Integer;

      type Ptr1 is access Arr1;
      type Ptr2 is access Arr2;

      Pt1 : Ptr1 := new Arr1'(Arr1'Range => 0);
      Pt2 : Ptr2 := new Arr2'(Arr2'Range (1) => (Arr2'Range (2) => 0));
      subtype Arr1_Range is Integer range Pt1'Range;
   begin
      if Pt1'First /= Ident_Int (1) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
            "ARRAY USING ACCESS TYPES AS PREFIXES 1");
      end if;

      if Pt2'First (2) /= Ident_Int (1) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR TWO-DIM " &
            "ARRAY USING ACCESS TYPES AS PREFIXES 1");
      end if;

      if Arr1_Range'First /= Ident_Int (1) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
            "ARRAY USING ACCESS TYPES AS PREFIXES 2");
      end if;

      if Pt1'Last /= Ident_Int (10) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
            "ARRAY USING ACCESS TYPES AS PREFIXES 3");
      end if;

      if Pt2'Last (2) /= Ident_Int (3) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR TWO-DIM " &
            "ARRAY USING ACCESS TYPES AS PREFIXES 2");
      end if;

      if Arr1_Range'Last /= Ident_Int (10) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
            "ARRAY USING ACCESS TYPES AS PREFIXES 4");
      end if;

      if Pt1'Length /= Ident_Int (10) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
            "ARRAY USING ACCESS TYPES AS PREFIXES 5");
      end if;

      if Pt2'Length (2) /= Ident_Int (3) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR TWO-DIM " &
            "ARRAY USING ACCESS TYPES AS PREFIXES 3");
      end if;

   end;

   declare

      type Uncon is array (Integer range <>) of Integer;
      type Uncon2 is array (Integer range <>, Integer range <>) of Integer;

      Ary1 : String (Ident_Int (5) .. Ident_Int (8));
      F    : Integer := Ident_Int (1);
      L    : Integer := Ident_Int (3);

      function Fun (Lo, Hi : Integer) return Uncon is
         Arr : Uncon (Ident_Int (Lo) .. Ident_Int (Hi));
      begin
         Arr := (Arr'Range => 0);
         return Arr;
      end Fun;

      function Fun2 (Lo, Hi : Integer) return Uncon2 is
         Ar2 : Uncon2 (Ident_Int (Lo) .. Ident_Int (Hi),
            Ident_Int (Lo) .. Ident_Int (Hi));
      begin
         Ar2 := (Ar2'Range (1) => (Ar2'Range (2) => 0));
         return Ar2;
      end Fun2;
   begin

      Ary1 := (Ary1'Range => 'A');

      if Fun (F, L)'First /= Ident_Int (1) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
            "ARRAY USING FUNCTION RESULTS AS " & "PREFIXES 1");
      end if;

      if Fun2 (F, L)'First (2) /= Ident_Int (1) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR TWO-DIM " &
            "ARRAY USING FUNCTION RESULTS AS " & "PREFIXES 1");
      end if;

      if "&" (Ary1, "XX")'First /= Ident_Int (5) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
            "ARRAY USING FUNCTION RESULTS AS " & "PREFIXES 2");
      end if;

      if Fun (F, L)'Last /= Ident_Int (3) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
            "ARRAY USING FUNCTION RESULTS AS " & "PREFIXES 3");
      end if;

      if Fun2 (F, L)'Last (2) /= Ident_Int (3) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR TWO-DIM " &
            "ARRAY USING FUNCTION RESULTS AS " & "PREFIXES 2");
      end if;

      if "&" (Ary1, "YY")'Last /= Ident_Int (10) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
            "ARRAY USING FUNCTION RESULTS AS " & "PREFIXES 4");
      end if;

      if Fun (F, L)'Length /= Ident_Int (3) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
            "ARRAY USING FUNCTION RESULTS AS " & "PREFIXES 5");
      end if;

      if Fun2 (F, L)'Length (2) /= Ident_Int (3) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR TWO-DIM " &
            "ARRAY USING FUNCTION RESULTS AS " & "PREFIXES 3");
      end if;

      if "&" (Ary1, "XX")'Length /= Ident_Int (6) then
         Failed
           ("INCORRECT ATTRIBUTE VALUE FOR ONE-DIM " &
            "ARRAY USING FUNCTION RESULTS AS " & "PREFIXES 6");
      end if;

      declare

         subtype Smin is Integer range Fun (F, L)'Range;
         subtype Smin2 is Integer range Fun2 (F, L)'Range (2);
         subtype Smin3 is Integer range "&" (Ary1, "YY")'Range;

      begin
         if Smin'First /= Ident_Int (1) then
            Failed
              ("INCORRECT ATTRIBUTE VALUE FOR " &
               "ONE-DIM ARRAY USING FUNCTION " & "RESULTS AS PREFIXES 7");
         end if;

         if Smin2'First /= Ident_Int (1) then
            Failed
              ("INCORRECT ATTRIBUTE VALUE FOR " &
               "TWO-DIM ARRAY USING FUNCTION " & "RESULTS AS PREFIXES 4");
         end if;

         if Smin3'First /= Ident_Int (5) then
            Failed
              ("INCORRECT ATTRIBUTE VALUE FOR " &
               "ONE-DIM ARRAY USING FUNCTION " & "RESULTS AS PREFIXES 8");
         end if;

         if Smin'Last /= Ident_Int (3) then
            Failed
              ("INCORRECT ATTRIBUTE VALUE FOR " &
               "ONE-DIM ARRAY USING FUNCTION " & "RESULTS AS PREFIXES 9");
         end if;

         if Smin2'Last /= Ident_Int (3) then
            Failed
              ("INCORRECT ATTRIBUTE VALUE FOR " &
               "TWO-DIM ARRAY USING FUNCTION " & "RESULTS AS PREFIXES 5");
         end if;

         if Smin3'Last /= Ident_Int (10) then
            Failed
              ("INCORRECT ATTRIBUTE VALUE FOR " &
               "ONE-DIM ARRAY USING FUNCTION " & "RESULTS AS PREFIXES 10");
         end if;

      end;

   end;

   Result;

end C36204b;
