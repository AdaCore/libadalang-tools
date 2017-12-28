-- C433003.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
--  OBJECTIVE:
--     Check that for each association with a <> in an array aggregate, the
--     component is initialized by default. (Scalar types with Default_Values.)
--
--  TEST DESCRIPTION:
--     This test tries various aggregates containing <> for a number
--     of interesting types.
--
-- CHANGE HISTORY:
--     21 Nov 2014 RLB Created test based on ideas from C433A04.
--
--!
with Report;
with Ada.Finalization;
procedure C433003 is
   type Color_Type is
     (Unknown, Red, Orange, Yellow, Green, Blue, Indigo, Violet) with
      Default_Value => Unknown;

   type Default_To_Zero_Type is range -10_000 .. 10_000 with
      Default_Value => 0;

   type Half_Type is digits 4 with
      Default_Value => 0.5;

   type Ta is array (1 .. 3) of Color_Type;
   type Ta2 is array (1 .. 2, 1 .. 4) of Color_Type;

   type Tb is array (1 .. 3) of Default_To_Zero_Type;
   type Tb2 is array (1 .. 2, 1 .. 4) of Default_To_Zero_Type;
   type Tbs is array (1 .. 2) of Tb;
   type Tc is array (1 .. 5) of Half_Type;

begin
   Report.Test
     ("C433003",
      "Check that for each association with a <> in an " &
      "array aggregate, the component is initialized by " &
      "default. (Scalar types with Default_Values.)");

   declare
      O1     : Ta := (others => <>);
      Result : Ta := (others => Unknown);
   begin
      if O1 /= Result then
         Report.Failed ("Wrong value: O1");
      end if;
   end;

   declare
      O2     : Ta := Ta'(1 => Green, 2 | 3 => <>);
      Result : Ta := (Green, others => Unknown);
   begin
      if O2 /= Result then
         Report.Failed ("Wrong value: O2");
      end if;
   end;

   declare
      O3     : Ta := (1 .. 2 => <>, 3 => Unknown);
      Result : Ta := (others => Unknown);
   begin
      if O3 /= Result then
         Report.Failed ("Wrong value: O3");
      end if;
   end;

   declare
      O4     : Ta := Ta'(2 => <>, 1 | 3 => Blue);
      Result : Ta := (Blue, Unknown, Blue);
   begin
      if O4 /= Result then
         Report.Failed ("Wrong value: O4");
      end if;
   end;

   declare
      O5 : Ta2 := (others => (others => <>));
   begin
      if O5 /= (Ta2'Range (1) => (Ta2'Range (2) => Unknown)) then
         Report.Failed ("Wrong value: O5");
      end if;
   end;

   declare
      O6 : Ta2 := (1 => (1 .. 4 => <>), 2 => (others => Unknown));
   begin
      if O6 /= (Ta2'Range (1) => (Ta2'Range (2) => Unknown)) then
         Report.Failed ("Wrong value: O6");
      end if;
   end;

   declare
      O7 : Ta2 := Ta2'(1 .. 2 => (1 | 3 => <>, 2 | 4 => Unknown));
   begin
      if O7 /= (Ta2'Range (1) => (Ta2'Range (2) => Unknown)) then
         Report.Failed ("Wrong value: O7");
      end if;
   end;

   declare
      O11 : Tb;
      O12 : Tb;
      O13 : Tb;
      O14 : Tb;
      O15 : Tb2;
      O16 : Tb2;
      O17 : Tb2;
      O18 : Tbs;
      O19 : Tbs;
   begin
      O11 := (others => <>);
      O12 := Tb'(1 => 12, 2 | 3 => <>);
      O13 := (1 .. 2 => <>, 3 => 87);
      O14 := Tb'(2 => <>, 1 | 3 => 52);
      if O11 /= Tb'(others => 0) then
         Report.Failed ("Wrong value: O11");
      end if;
      if O12 (2) /= 0 or O12 (3) /= 0 then
         Report.Failed ("Wrong value: O12");
      end if;
      if O13 (1) /= 0 or O13 (2) /= 0 then
         Report.Failed ("Wrong value: O13");
      end if;
      if O14 (2) /= 0 then
         Report.Failed ("Wrong value: O14");
      end if;
      O15 := (others => (others => <>));
      O16 := Tb2'(1 => (1 .. 4 => <>), 2 => (others => 0));
      O17 := (1 .. 2 => (1 | 3 => <>, 2 | 4 => 0));
      if O15 /= (Tb2'Range (1) => (Tb2'Range (2) => 0)) then
         Report.Failed ("Wrong value: O15");
      end if;
      if O16 /= (Tb2'Range (1) => (Tb2'Range (2) => 0)) then
         Report.Failed ("Wrong value: O16");
      end if;
      if O17 /= (Tb2'Range (1) => (Tb2'Range (2) => 0)) then
         Report.Failed ("Wrong value: O17");
      end if;
      O18 := (others => <>);
      O19 := Tbs'(1 => <>, 2 => (others => 0));
      if O18 /= (Tbs'Range => (Tb'Range => 0)) then
         Report.Failed ("Wrong value: O18");
      end if;
      if O19 /= (Tbs'Range => (Tb'Range => 0)) then
         Report.Failed ("Wrong value: O19");
      end if;
   end;

   declare
      O31 : Tc;
      O32 : Tc;
      O33 : Tc;
      O34 : Tc;
      O35 : Tc;
   begin
      O31 := (others => <>);
      O32 := Tc'(1 => 0.25, 2 .. 5 => <>);
      O33 := (1 | 2 | 5 => <>, 3 | 4 => 0.75);
      O34 := Tc'(1 => <>, 2 .. 5 => 2.25);
      O35 := (1 .. 5 => <>);
      if O31 /= (1 .. 5 => 0.5) then
         Report.Failed ("Wrong value: O31");
      end if;
      if O32 /= Tc'(0.25, others => 0.5) then
         Report.Failed ("Wrong value: O32");
      end if;
      if O33 /= (0.5, 0.5, 0.75, 0.75, 0.5) then
         Report.Failed ("Wrong value: O33");
      end if;
      if O34 /= (0.5, 2.25, 2.25, 2.25, 2.25) then
         Report.Failed ("Wrong value: O34");
      end if;
      if O35 /= (1 .. 5 => 0.5) then
         Report.Failed ("Wrong value: O35");
      end if;
   end;

   Report.Result;
end C433003;
