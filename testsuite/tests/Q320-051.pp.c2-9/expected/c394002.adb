-- C394002.A
--
--                            Grant of Unlimited Rights
--
--    AdaCore holds unlimited rights in the software and documentation
--    contained herein. Unlimited rights are the same as those granted
--    by the U.S. Government for older parts of the Ada Conformity
--    Assessment Test Suite, and are defined in DFAR 252.227-7013(a)(19).
--    By making this public release, AdaCore intends to confer upon all
--    recipients unlimited rights equal to those held by the Ada Conformity
--    Assessment Authority. These rights include rights to use, duplicate,
--    release or disclose the released technical data and computer software
--    in whole or in part, in any manner and for any purpose whatsoever,
--    and to have or permit others to do so.
--
--                                   DISCLAIMER
--
--    ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--    DISCLOSED ARE AS IS. ADACORE MAKES NO EXPRESS OR IMPLIED WARRANTY AS
--    TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE SOFTWARE,
--    DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE OR
--    DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--    PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                    Notice
--
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--
--    This test is based on one submitted by AdaCore; AdaCore retains the
--    copyright on the test.
--*
--  OBJECTIVE:
--      Check that an interface inherits primitive subprograms from
--      each progenitor. Case 1A: Limited interfaces, simple primitive
--      subprograms.
--
--  CHANGE HISTORY:
--     29 Jul 2004 JM  Initial version.
--     30 Oct 2007 RLB Converted to ACATS test.
--!
with Report; use Report;
procedure C394002 is

   package Pkg1 is
      type I1 is limited interface;
      procedure P (A : in I1; R : out Natural) is abstract;
      function F (X : in I1) return Natural is abstract;

      type I2 is limited interface;
      procedure Q (B : in out I2; R : out Natural) is abstract;

      type I3 is limited interface and I1;
      procedure R (X : I3; R1 : out Natural) is abstract;

      type I4 is limited interface and I1 and I2;

      --  Case 1: Abstract derivation

      type A_10 is abstract new I1 with null record;
      type A_11 is abstract new I1 and I2 with null record;

      type Da_10 is new A_10 with null record;
      procedure P (X : in Da_10; R : out Natural);
      function F (X : in Da_10) return Natural;

      --  Case 2: Interface derivations

      type D2 is new I1 and I2 with null record;
      procedure P (X : D2; R : out Natural);
      function F (X : D2) return Natural;
      procedure Q (X : in out D2; R : out Natural);

      type D3 is new I1 and I3 with null record;
      procedure P (X : D3; R : out Natural);
      function F (X : D3) return Natural;
      procedure R (X : D3; R : out Natural);

      type D4 is new I4 with null record;
      procedure P (X : D4; R : out Natural);
      function F (X : D4) return Natural;
      procedure Q (X : in out D4; R : out Natural);

      --  Case 3: Derivation of a tagged record without components

      type T1 is tagged null record;
      type Dt1_0 is new T1 and I1 and I2 with null record;
      procedure Q (X : in out Dt1_0; R : out Natural);
      procedure P (X : Dt1_0; R : out Natural);
      function F (X : Dt1_0) return Natural;

      type Dt1_1 is new Dt1_0 with null record;
      --  Inherits all the primitive operations and interfaces from the
      --  ancestor

      --  Case 4: Derivation of a tagged record with components

      type T2 is tagged record
         Data : Integer := 2_005;
      end record;

      type Dt2_0 is new T2 and I1 and I2 with record
         More_Data : Integer := 2_006;
      end record;

      procedure Q (X : in out Dt2_0; R : out Natural);
      procedure P (X : Dt2_0; R : out Natural);
      function F (X : Dt2_0) return Natural;

      type Dt2_1 is new Dt2_0 with null record;                           -- OK
      --  Inherits all the primitive operations and interfaces from the
      --  ancestor

   end Pkg1;

   package body Pkg1 is

      procedure P (X : Da_10; R : out Natural) is
      begin
         R := 10;
      end P;

      function F (X : Da_10) return Natural is
      begin
         return 11;
      end F;

      --  Subprograms of D2

      procedure P (X : D2; R : out Natural) is
      begin
         R := 20;
      end P;

      function F (X : D2) return Natural is
      begin
         return 21;
      end F;

      procedure Q (X : in out D2; R : out Natural) is
      begin
         R := 22;
      end Q;

      --  Subprograms of D3

      procedure P (X : D3; R : out Natural) is
      begin
         R := 20;
      end P;

      function F (X : D3) return Natural is
      begin
         return 21;
      end F;

      procedure R (X : D3; R : out Natural) is
      begin
         R := 29;
      end R;

      --  Subprograms of D4

      procedure P (X : D4; R : out Natural) is
      begin
         R := 20;
      end P;

      function F (X : D4) return Natural is
      begin
         return 21;
      end F;

      procedure Q (X : in out D4; R : out Natural) is
      begin
         R := 22;
      end Q;

      --  Subprograms of DT1

      procedure P (X : Dt1_0; R : out Natural) is
      begin
         R := 30;
      end P;

      function F (X : Dt1_0) return Natural is
      begin
         return 31;
      end F;

      procedure Q (X : in out Dt1_0; R : out Natural) is
      begin
         R := 32;
      end Q;

      --  Subprograms of DT2

      procedure P (X : Dt2_0; R : out Natural) is
      begin
         if X.Data /= 2_005 or X.More_Data /= 2_006 then
            Report.Failed ("P-DT2_0 unusual data values");
         end if;
         R := 40;
      end P;

      function F (X : Dt2_0) return Natural is
      begin
         if X.Data /= 2_005 or X.More_Data /= 2_006 then
            Report.Failed ("F-DT2_0 unusual data values");
         end if;
         return 41;
      end F;

      procedure Q (X : in out Dt2_0; R : out Natural) is
      begin
         if X.Data /= 2_005 or X.More_Data /= 2_006 then
            Report.Failed ("Q-DT2_0 unusual data values");
         end if;
         R := 42;
      end Q;

   end Pkg1;

   use Pkg1;

   procedure I1w_P_Test (Iw : in I1'Class; R : out Natural) is
   begin
      P (Iw, R);
   end I1w_P_Test;

   function I1w_F_Test (Iw : in I1'Class) return Natural is
   begin
      return F (Iw);
   end I1w_F_Test;

   procedure I2w_Q_Test (Iw : in out I2'Class; R : out Natural) is
   begin
      Q (Iw, R);
   end I2w_Q_Test;

   procedure I3w_P_Test (Iw : in I3'Class; R : out Natural) is
   begin
      P (Iw, R);
   end I3w_P_Test;

   function I3w_F_Test (Iw : in I3'Class) return Natural is
   begin
      return F (Iw);
   end I3w_F_Test;

   procedure I3w_R_Test (Iw : in I3'Class; R1 : out Natural) is
   begin
      R (Iw, R1);
   end I3w_R_Test;

   procedure I4w_P_Test (Iw : in I4'Class; R : out Natural) is
   begin
      P (Iw, R);
   end I4w_P_Test;

   function I4w_F_Test (Iw : in I4'Class) return Natural is
   begin
      return F (Iw);
   end I4w_F_Test;

   procedure I4w_Q_Test (Iw : in out I4'Class; R : out Natural) is
   begin
      Q (Iw, R);
   end I4w_Q_Test;

   O_T1   : T1;     --  tagged null record
   O_D2   : D2;     --  new I1 and I2 with null record
   O_D3   : D3;     --  new I1 and I3, being I3 a derivation of I1
   O_D4   : D4;     --  new I4; being I4 a derivation of I1 and I2
   O_Dt1  : Dt1_0;  --  new T1 and I1 and I2
   O_Dt11 : Dt1_1;  --  new DT1_0
   O_Dt2  : Dt2_0;  --  new T2 and I1 and I2
   O_Dt22 : Dt2_1;  --  new DT2_0
   R      : Natural;
begin
   Report.Test
     ("C394002",
      "Check that an interface inherits primitive " &
      "subprograms from each progenitor. Case 1A: " &
      "Limited interfaces, simple primitive subprograms");

   --  ----------------------------------------------------------------------

   I1w_P_Test (O_D2, R);

   if R /= 20 then
      Report.Failed ("Subtest 1 failed: " & Integer'Image (R));
   end if;

   ---

   R := I1w_F_Test (O_D2);

   if R /= 21 then
      Report.Failed ("Subtest 2 failed: " & Integer'Image (R));
   end if;

   ---

   I2w_Q_Test (O_D2, R);

   if R /= 22 then
      Report.Failed ("Subtest 3 failed: " & Integer'Image (R));
   end if;

   --  ----------------------------------------------------------------------

   I1w_P_Test (O_D3, R);

   if R /= 20 then
      Report.Failed ("Subtest 4 failed: " & Integer'Image (R));
   end if;

   ---

   R := I1w_F_Test (O_D3);

   if R /= 21 then
      Report.Failed ("Subtest 5 failed: " & Integer'Image (R));
   end if;

   ---

   I3w_P_Test (O_D3, R);

   if R /= 20 then
      Report.Failed ("Subtest 6 failed: " & Integer'Image (R));
   end if;

   ---

   R := I3w_F_Test (O_D3);

   if R /= 21 then
      Report.Failed ("Subtest 7 failed: " & Integer'Image (R));
   end if;

   ---

   I3w_R_Test (O_D3, R);

   if R /= 29 then
      Report.Failed ("Subtest 8 failed: " & Integer'Image (R));
   end if;

   --  ----------------------------------------------------------------------

   I1w_P_Test (O_D4, R);

   if R /= 20 then
      Report.Failed ("Subtest 9 failed: " & Integer'Image (R));
   end if;

   ---

   R := I1w_F_Test (O_D4);

   if R /= 21 then
      Report.Failed ("Subtest 10 failed: " & Integer'Image (R));
   end if;

   ---

   I2w_Q_Test (O_D4, R);

   if R /= 22 then
      Report.Failed ("Subtest 11 failed: " & Integer'Image (R));
   end if;

   ---

   I4w_P_Test (O_D4, R);

   if R /= 20 then
      Report.Failed ("Subtest 12 failed: " & Integer'Image (R));
   end if;

   ---

   R := I4w_F_Test (O_D4);

   if R /= 21 then
      Report.Failed ("Subtest 13 failed: " & Integer'Image (R));
   end if;

   ---

   I4w_Q_Test (O_D4, R);

   if R /= 22 then
      Report.Failed ("Subtest 14 failed: " & Integer'Image (R));
   end if;

   --  ----------------------------------------------------------------------

   I1w_P_Test (O_Dt1, R);

   if R /= 30 then
      Report.Failed ("Subtest 15 failed: " & Integer'Image (R));
   end if;

   ---

   R := I1w_F_Test (O_Dt1);

   if R /= 31 then
      Report.Failed ("Subtest 16 failed: " & Integer'Image (R));
   end if;

   ---

   I2w_Q_Test (O_Dt1, R);

   if R /= 32 then
      Report.Failed ("Subtest 17 failed: " & Integer'Image (R));
   end if;

   --  ---------------------------------------------------------------------

   I1w_P_Test (O_Dt11, R);

   if R /= 30 then
      Report.Failed ("Subtest 18 failed: " & Integer'Image (R));
   end if;

   ---

   R := I1w_F_Test (O_Dt11);

   if R /= 31 then
      Report.Failed ("Subtest 19 failed: " & Integer'Image (R));
   end if;

   ---

   I2w_Q_Test (O_Dt11, R);

   if R /= 32 then
      Report.Failed ("Subtest 20 failed: " & Integer'Image (R));
   end if;

   --  ---------------------------------------------------------------------

   I1w_P_Test (O_Dt2, R);

   if R /= 40 then
      Report.Failed ("Subtest 21 failed: " & Integer'Image (R));
   end if;

   ---

   R := I1w_F_Test (O_Dt2);

   if R /= 41 then
      Report.Failed ("Subtest 22 failed: " & Integer'Image (R));
   end if;

   ---

   I2w_Q_Test (O_Dt2, R);

   if R /= 42 then
      Report.Failed ("Subtest 23 failed: " & Integer'Image (R));
   end if;

   --  ---------------------------------------------------------------------

   I1w_P_Test (O_Dt22, R);

   if R /= 40 then
      Report.Failed ("Subtest 24 failed: " & Integer'Image (R));
   end if;

   ---

   R := I1w_F_Test (O_Dt22);

   if R /= 41 then
      Report.Failed ("Subtest 25 failed: " & Integer'Image (R));
   end if;

   ---

   I2w_Q_Test (O_Dt22, R);

   if R /= 42 then
      Report.Failed ("Subtest 26 failed: " & Integer'Image (R));
   end if;

   --  ------------------

   Report.Result;
end C394002;
