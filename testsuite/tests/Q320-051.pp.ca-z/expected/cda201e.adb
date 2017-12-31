-- CDA201E.ADA

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
--     CHECK THAT UNCHECKED_CONVERSION CAN BE INSTANTIATED FOR THE
--     CONVERSION OF AN ENUMERATION TYPE WITH A REPRESENTATION CLAUSE TO
--     INTEGER.

-- HISTORY:
--     JET 09/23/88  CREATED ORIGINAL TEST.
--     DHH 05/17/89  CHANGED FROM '.DEP' TEST TO '.ADA' TEST.
--     RJW 02/28/90  ADDED SIZE CLAUSE FOR TYPE STOOGE.
--     LDC 09/20/90  ADDED CHECK FOR CONVERSION FROM INT TO STOOGE,
--                   ADDED COMMENT WHEN SIZES AREN'T EQUAL.

with Report; use Report;
with Unchecked_Conversion;
procedure Cda201e is

   type Stooge is (Curly, Moe, Larry);
   for Stooge use (Curly => -5, Moe => 13, Larry => 127);
   for Stooge'Size use 8;

   type Int is range -128 .. 127;
   for Int'Size use 8;

   I    : Int    := 0;
   Name : Stooge := Curly;

   function E_To_I is new Unchecked_Conversion (Stooge, Int);
   function I_To_E is new Unchecked_Conversion (Int, Stooge);

   function Id (E : Stooge) return Stooge is
   begin
      return Stooge'Val (Stooge'Pos (E) + Ident_Int (0));
   end Id;

   function Id_Int (X : Int) return Int is
      A : Integer := Ident_Int (3);
   begin
      if Equal (A, Ident_Int (3)) then    -- ALWAYS EQUAL.
         return X;                     -- ALWAYS EXECUTED.
      end if;
      return 0;                          -- NEVER EXECUTED.
   end Id_Int;

begin
   Test
     ("CDA201E",
      "CHECK THAT UNCHECKED_CONVERSION CAN BE " &
      "INSTANTIATED FOR THE CONVERSION OF AN " &
      "ENUMERATION TYPE WITH A REPRESENTATION " & "CLAUSE TO INTEGER");

   if I'Size /= Name'Size then
      Comment
        ("UNCHECKED_CONVERSION MIGHT BE INSTANTIATED WITH " &
         "DIFFERNT SIZES");
   end if;

   begin
      I := E_To_I (Id (Curly));
      if I /= -5 then
         Failed ("INCORRECT VALUE OF CURLY: " & Int'Image (I));
      end if;

      I := E_To_I (Id (Moe));
      if I /= 13 then
         Failed ("INCORRECT VALUE OF MOE: " & Int'Image (I));
      end if;

      I := E_To_I (Id (Larry));
      if I /= 127 then
         Failed ("INCORRECT VALUE OF LARRY: " & Int'Image (I));
      end if;
   exception
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED BY CONVERSION");
   end;

   begin     -- 2
      Name := I_To_E (Id_Int (-5));
      if Name /= Curly then
         Failed ("INCORRECT VALUE OF -5 : " & Stooge'Image (Name));
      end if;

      Name := I_To_E (Id_Int (13));
      if Name /= Moe then
         Failed ("INCORRECT VALUE OF 13: " & Stooge'Image (Name));
      end if;

      Name := I_To_E (Id_Int (127));
      if Name /= Larry then
         Failed ("INCORRECT VALUE OF 127: " & Stooge'Image (Name));
      end if;
   exception
      when others =>
         Failed ("UNEXPECTED EXCEPTION RAISED BY CONVERSION - 2");
   end;

   Result;
end Cda201e;
