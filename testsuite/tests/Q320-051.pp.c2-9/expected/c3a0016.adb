-- C3A0016.A
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
-- OBJECTIVE:
--     Check that a constant access-to-variable value can be used to
--     modify the designated object.
--
-- TEST DESCRIPTION:
--     Create various constant objects of various access-to-variable types
--     (named pool-specific, named general, anonymous), and modify the
--     designated object.
--
-- CHANGE HISTORY:
--     26 Mar 2008 RLB Created test to replace C38006A.
--
with Report;
procedure C3a0016 is

   type Ai is access Integer;

   Cai : constant Ai := new Integer'(0);

   Fo : aliased Float := 1.0;

   type Rec is record
      Af_Comp : access Float := Fo'Access;
      Val     : Natural      := 1;
   end record;

   Ro : aliased Rec;

   Cr : constant Rec := (others => <>);

   type Gar is access all Rec;

   Car : constant Gar := Ro'Access;

   Bo : aliased Boolean := False;

   Cab : constant access Boolean := Bo'Access;
   -- Note: "access constant" is something very different!

begin
   Report.Test
     ("C3A0016",
      "Check that a constant access-to-variable value " &
      "can be used to modify the designated object");

   for I in 1 .. 10 loop
      Cai.all        := Cai.all + 1;
      Cab.all        := not Cab.all;
      Car.Val        := Report.Ident_Int (I);
      Cr.Af_Comp.all := Float (Car.Val);
      if Cai.all /= I then
         Report.Failed ("Integer object accessed thru constant not changed");
         exit;
      end if;
      if Cab.all /= (I mod 2 /= 0) then
         Report.Failed ("Boolean object accessed thru constant not changed");
         exit;
      end if;
      if Car.Val /= I then
         Report.Failed ("Record component accessed thru constant not changed");
         exit;
      end if;
      if Cr.Af_Comp.all /= Float (I) then
         Report.Failed
           ("Float object accessed thru constant record component " &
            "not changed");
         exit;
      end if;
   end loop;

   Report.Result;

end C3a0016;
