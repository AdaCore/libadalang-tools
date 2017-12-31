--
-- C354002.A
--
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
--
-- OBJECTIVE:
--      Check that the attributes of modular types yield
--      correct values/results.  The attributes checked are:
--
--      First, Last, Range, Base, Min, Max, Succ, Pred,
--      Image, Width, Value, Pos, and Val
--
-- TEST DESCRIPTION:
--      This test defines several modular types.  One type defined at
--      each of System.Max_Binary_Modulus, System.Max_Nonbinary_Modulus,
--      a power of two half that of System.Max_Binary_Modulus, one less
--      than that power of two; one more than that power of two, two
--      less than a (large) power of two.  For each of these types,
--      determine the correct operation of the following attributes:
--
--      First, Last, Range, Base, Min, Max, Succ, Pred, Image, Width,
--      Value, Pos, Val, and Modulus
--
--      The attributes Wide_Image and Wide_Value are deferred to C354003.
--
--
--
-- CHANGE HISTORY:
--      08 SEP 94   SAIC    Initial version
--      17 NOV 94   SAIC    Revised version
--      13 DEC 94   SAIC    split off Wide_String attributes into C354003
--      06 JAN 95   SAIC    Promoted to next release
--      19 APR 95   SAIC    Revised in accord with reviewer comments
--      27 JAN 96   SAIC    Eliminated 32/64 bit potential conflict for 2.1
--
--!

with Report;
with System;
with Tctouch;
procedure C354002 is

   function Id (Local_Value : Integer) return Integer renames Report.Ident_Int;
   function Id (Local_Value : String) return String renames Report.Ident_Str;

   Power_2_Bits          : constant := System.Storage_Unit;
   Half_Max_Binary_Value : constant := System.Max_Binary_Modulus / 2;

   type Max_Binary is mod System.Max_Binary_Modulus;
   type Max_Nonbinary is mod System.Max_Nonbinary_Modulus;
   type Half_Max_Binary is mod Half_Max_Binary_Value;

   type Medium is mod 2_048;
   type Medium_Plus is mod 2_042;
   type Medium_Minus is mod 2_111;

   type Small is mod 2;
   type Finger is mod 5;

   Mbl  : constant := Max_Nonbinary'Last;
   Mnbm : constant := Max_Nonbinary'Modulus;

   Ones_Complement_Permission : constant Boolean := Mbl = Mnbm;

   type Finger_Id is (Thumb, Index, Middle, Ring, Pinkie);

   subtype Midrange is Medium_Minus range 222 .. 1_111;

-- a few numbers for testing purposes
   Max_Binary_Mod_Over_3      : constant := Max_Binary'Modulus / 3;
   Max_Nonbinary_Mod_Over_4   : constant := Max_Nonbinary'Modulus / 4;
   System_Max_Bin_Mod_Pred    : constant := System.Max_Binary_Modulus - 1;
   System_Max_Nonbin_Mod_Pred : constant := System.Max_Nonbinary_Modulus - 1;
   Half_Max_Bin_Value_Pred    : constant := Half_Max_Binary_Value - 1;

   Amb, Bmb   : Max_Binary;
   Ahmb, Bhmb : Half_Max_Binary;
   Am, Bm     : Medium;
   Amp, Bmp   : Medium_Plus;
   Amm, Bmm   : Medium_Minus;
   As, Bs     : Small;
   Af, Bf     : Finger;

   Tc_Pass_Case : Boolean := True;

   procedure Value_Fault (S : String) is
   -- check 'Value for failure modes
   begin
      -- the evaluation of the 'Value expression should raise C_E
      Tctouch.Assert_Not (Midrange'Value (S) = 0, "Value_Fault");
      if Midrange'Value (S) not in Midrange'Base then
         Report.Failed ("'Value(" & S & ") raised no exception");
      end if;
   exception
      when Constraint_Error =>
         null; -- expected case
      when others =>
         Report.Failed ("'Value(" & S & ") raised wrong exception");
   end Value_Fault;

begin  -- Main test procedure.

   Report.Test ("C354002", "Check attributes of modular types");

-- Base
   Tctouch.Assert (Midrange'Base'First = 0, "Midrange'Base'First");
   Tctouch.Assert
     (Midrange'Base'Last = Medium_Minus'Last, "Midrange'Base'Last");

-- First
   Tctouch.Assert (Max_Binary'First = 0, "Max_Binary'First");
   Tctouch.Assert (Max_Nonbinary'First = 0, "Max_NonBinary'First");
   Tctouch.Assert (Half_Max_Binary'First = 0, "Half_Max_Binary'First");

   Tctouch.Assert (Medium'First = Medium (Id (0)), "Medium'First");
   Tctouch.Assert
     (Medium_Plus'First = Medium_Plus (Id (0)), "Medium_Plus'First");
   Tctouch.Assert
     (Medium_Minus'First = Medium_Minus (Id (0)), "Medium_Minus'First");

   Tctouch.Assert (Small'First = Small (Id (0)), "Small'First");
   Tctouch.Assert (Finger'First = Finger (Id (0)), "Finger'First");
   Tctouch.Assert (Midrange'First = Midrange (Id (222)), "Midrange'First");

-- Image
   Tctouch.Assert
     (Half_Max_Binary'Image (255) = " 255", "Half_Max_Binary'Image");
   Tctouch.Assert (Medium'Image (0) = Id (" 0"), "Medium'Image");
   Tctouch.Assert
     (Medium_Plus'Image (Medium_Plus'Last) = " 2041", "Medium_Plus'Image");
   Tctouch.Assert
     (Medium_Minus'Image (Medium_Minus (Id (1_024))) = " 1024",
      "Medium_Minus'Image");
   Tctouch.Assert (Small'Image (Small (Id (1))) = " 1", "Small'Image");
   Tctouch.Assert
     (Midrange'Image (Midrange (Id (333))) = " 333", "Midrange'Image");

-- Last
   Tctouch.Assert
     (Max_Binary'Last = System_Max_Bin_Mod_Pred, "Max_Binary'Last");
   if Ones_Complement_Permission then
      Tctouch.Assert
        (Max_Nonbinary'Last >= System_Max_Nonbin_Mod_Pred,
         "Max_NonBinary'Last (ones comp)");
   else
      Tctouch.Assert
        (Max_Nonbinary'Last = System_Max_Nonbin_Mod_Pred,
         "Max_NonBinary'Last");
   end if;
   Tctouch.Assert
     (Half_Max_Binary'Last = Half_Max_Bin_Value_Pred, "Half_Max_Binary'Last");

   Tctouch.Assert (Medium'Last = Medium (Id (2_047)), "Medium'Last");
   Tctouch.Assert
     (Medium_Plus'Last = Medium_Plus (Id (2_041)), "Medium_Plus'Last");
   Tctouch.Assert
     (Medium_Minus'Last = Medium_Minus (Id (2_110)), "Medium_Minus'Last");
   Tctouch.Assert (Small'Last = Small (Id (1)), "Small'Last");
   Tctouch.Assert (Finger'Last = Finger (Id (4)), "Finger'Last");
   Tctouch.Assert (Midrange'Last = Midrange (Id (1_111)), "Midrange'Last");

-- Max
   Tctouch.Assert
     (Max_Binary'Max (Power_2_Bits, Max_Binary'Last) = Max_Binary'Last,
      "Max_Binary'Max");
   Tctouch.Assert
     (Max_Nonbinary'Max (100, 2_000) = 2_000, "Max_NonBinary'Max");
   Tctouch.Assert
     (Half_Max_Binary'Max (123, 456) = 456, "Half_Max_Binary'Max");

   Tctouch.Assert (Medium'Max (0, 2_040) = 2_040, "Medium'Max");
   Tctouch.Assert (Medium_Plus'Max (0, 1) = 1, "Medium_Plus'Max");
   Tctouch.Assert
     (Medium_Minus'Max (2_001, 1_995) = 2_001, "Medium_Minus'Max");
   Tctouch.Assert (Small'Max (1, 0) = 1, "Small'Max");
   Tctouch.Assert (Finger'Max (Finger'Last + 1, 4) = 4, "Finger'Max");
   Tctouch.Assert
     (Midrange'Max (Midrange'First + 1, 222) = Midrange'First + 1,
      "Midrange'Max");

-- Min
   Tctouch.Assert
     (Max_Binary'Min (Power_2_Bits, Max_Binary'Last) = Power_2_Bits,
      "Max_Binary'Min");
   Tctouch.Assert (Max_Nonbinary'Min (100, 2_000) = 100, "Max_NonBinary'Min");
   Tctouch.Assert
     (Half_Max_Binary'Min (123, 456) = 123, "Half_Max_Binary'Min");

   Tctouch.Assert (Medium'Min (0, Medium (Id (2_040))) = 0, "Medium'Min");
   Tctouch.Assert (Medium_Plus'Min (0, 1) = 0, "Medium_Plus'Min");
   Tctouch.Assert
     (Medium_Minus'Min (2_001, 1_995) = 1_995, "Medium_Minus'Min");
   Tctouch.Assert (Small'Min (1, 0) = 0, "Small'Min");
   Tctouch.Assert (Finger'Min (Finger'Last + 1, 4) /= 4, "Finger'Min");
   Tctouch.Assert
     (Midrange'Min (Midrange'First + 1, 222) = 222, "Midrange'Min");
-- Modulus
   Tctouch.Assert
     (Max_Binary'Modulus = System.Max_Binary_Modulus, "Max_Binary'Modulus");
   Tctouch.Assert
     (Max_Nonbinary'Modulus = System.Max_Nonbinary_Modulus,
      "Max_NonBinary'Modulus");
   Tctouch.Assert
     (Half_Max_Binary'Modulus = Half_Max_Binary_Value,
      "Half_Max_Binary'Modulus");

   Tctouch.Assert (Medium'Modulus = 2_048, "Medium'Modulus");
   Tctouch.Assert (Medium_Plus'Modulus = 2_042, "Medium_Plus'Modulus");
   Tctouch.Assert (Medium_Minus'Modulus = 2_111, "Medium_Minus'Modulus");
   Tctouch.Assert (Small'Modulus = 2, "Small'Modulus");
   Tctouch.Assert (Finger'Modulus = 5, "Finger'Modulus");
   Tctouch.Assert (Midrange'Modulus = Id (2_111), "Midrange'Modulus");

-- Pos
   declare
      Int : Natural := 222;
   begin
      for I in Midrange loop
         Tc_Pass_Case := Tc_Pass_Case and Midrange'Pos (I) = Int;

         Int := Int + 1;
      end loop;
   end;

   Tctouch.Assert (Tc_Pass_Case, "Midrange'Pos");

-- Pred
   Tctouch.Assert
     (Max_Binary'Pred (0) = System_Max_Bin_Mod_Pred, "Max_Binary'Pred(0)");
   if Ones_Complement_Permission then
      Tctouch.Assert
        (Max_Nonbinary'Pred (0) >= System_Max_Nonbin_Mod_Pred,
         "Max_NonBinary'Pred(0) (ones comp)");
   else
      Tctouch.Assert
        (Max_Nonbinary'Pred (0) = System_Max_Nonbin_Mod_Pred,
         "Max_NonBinary'Pred(0)");
   end if;
   Tctouch.Assert
     (Half_Max_Binary'Pred (0) = Half_Max_Bin_Value_Pred,
      "Half_Max_Binary'Pred(0)");

   Tctouch.Assert (Medium'Pred (Medium (Id (0))) = 2_047, "Medium'Pred(0)");
   Tctouch.Assert (Medium_Plus'Pred (0) = 2_041, "Medium_Plus'Pred(0)");
   Tctouch.Assert (Medium_Minus'Pred (0) = 2_110, "Medium_Minus'Pred(0)");
   Tctouch.Assert (Small'Pred (0) = 1, "Small'Pred(0)");
   Tctouch.Assert (Finger'Pred (Finger (Id (0))) = 4, "Finger'Pred(0)");
   Tctouch.Assert (Midrange'Pred (222) = 221, "Midrange'Pred('First)");

-- Range
   for I in Midrange'Range loop
      if I not in Midrange then
         Report.Failed ("Midrange loop test");
      end if;
   end loop;
   for I in Medium'Range loop
      if I not in Medium then
         Report.Failed ("Medium loop test");
      end if;
   end loop;
   for I in Medium_Minus'Range loop
      if I not in 0 .. 2_110 then
         Report.Failed ("Medium loop test");
      end if;
   end loop;

-- Succ
   Tctouch.Assert
     (Max_Binary'Succ (System_Max_Bin_Mod_Pred) = 0, "Max_Binary'Succ('Last)");
   if Ones_Complement_Permission then
      Tctouch.Assert
        ((Max_Nonbinary'Succ (System_Max_Nonbin_Mod_Pred) = 0) or
         (Max_Nonbinary'Succ (System_Max_Nonbin_Mod_Pred) =
          Max_Nonbinary'Last),
         "Max_NonBinary'Succ('Last) (ones comp)");
   else
      Tctouch.Assert
        (Max_Nonbinary'Succ (System_Max_Nonbin_Mod_Pred) = 0,
         "Max_NonBinary'Succ('Last)");
   end if;
   Tctouch.Assert
     (Half_Max_Binary'Succ (Half_Max_Bin_Value_Pred) = 0,
      "Half_Max_Binary'Succ('Last)");

   Tctouch.Assert (Medium'Succ (2_047) = 0, "Medium'Succ('Last)");
   Tctouch.Assert (Medium_Plus'Succ (2_041) = 0, "Medium_Plus'Succ('Last)");
   Tctouch.Assert (Medium_Minus'Succ (2_110) = 0, "Medium_Minus'Succ('Last)");
   Tctouch.Assert (Small'Succ (1) = 0, "Small'Succ('Last)");
   Tctouch.Assert (Finger'Succ (4) = 0, "Finger'Succ('Last)");
   Tctouch.Assert
     (Midrange'Succ (Midrange (Id (1_111))) = 1_112, "Midrange'Succ('Last)");

-- Val
   for I in Natural range Id (222) .. Id (1_111) loop
      Tctouch.Assert (Midrange'Val (I) = Medium_Minus (I), "Midrange'Val");
   end loop;

-- Value

   Tctouch.Assert
     (Half_Max_Binary'Value ("255") = 255, "Half_Max_Binary'Value");

   Tctouch.Assert (Medium'Value (" 1e2") = 100, "Medium'Value(""1e2"")");
   Tctouch.Assert (Medium'Value (" 0 ") = 0, "Medium'Value");
   Tctouch.Assert
     (Medium_Plus'Value (Id ("2041")) = 2_041, "Medium_Plus'Value");
   Tctouch.Assert
     (Medium_Minus'Value (Id ("+10_24")) = 1_024, "Medium_Minus'Value");

   Tctouch.Assert (Small'Value ("+1") = 1, "Small'Value");
   Tctouch.Assert (Midrange'Value (Id ("333")) = 333, "Midrange'Value");
   Tctouch.Assert (Midrange'Value ("1E3") = 1_000, "Midrange'Value(""1E3"")");

   Value_Fault ("bad input");
   Value_Fault ("-333");
   Value_Fault ("9999");
   Value_Fault (".1");
   Value_Fault ("1e-1");

-- Width
   Tctouch.Assert (Medium'Width = 5, "Medium'Width");
   Tctouch.Assert (Medium_Plus'Width = 5, "Medium_Plus'Width");
   Tctouch.Assert (Medium_Minus'Width = 5, "Medium_Minus'Width");
   Tctouch.Assert (Small'Width = 2, "Small'Width");
   Tctouch.Assert (Finger'Width = 2, "Finger'Width");
   Tctouch.Assert (Midrange'Width = 5, "Midrange'Width");

   Report.Result;

end C354002;
