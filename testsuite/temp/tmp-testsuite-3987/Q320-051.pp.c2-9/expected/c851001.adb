-- C851001.A
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
--     Check that when renaming an object that excludes null, the renamed
--     object still excludes null even if the renaming_declaration does not
--     include a null_exclusion.
--
--     Check that an object renaming with an anonymous access-to-object type
--     with no null exclusion can rename an object with an anonymous
--     access-to-object with a matching designated subtype and a null exclusion.
--
-- CHANGE HISTORY:
--     21 Jul 2008 RLB Created test.
--
with Report;
procedure C851001 is

   type Int_Ptr is access all Integer;

   subtype Nn_Int_Ptr is not null Int_Ptr;

   type Nn_Int_Ptr_Ptr is access Nn_Int_Ptr;

   type Nn_Int_Ptr_Array is array (1 .. 3) of not null Int_Ptr;

   type Acc_Ptr is access function (X : Float) return Float;

   subtype Nn_Acc_Ptr is not null Acc_Ptr;

   function Null_Init return Int_Ptr is
   begin
      return null;
   end Null_Init;

   Aux : aliased Integer := 99;
   function Non_Null_Init return Int_Ptr is
   begin
      return Aux'Access;
   end Non_Null_Init;

   function Double (X : Float) return Float is
   begin
      return 2.0 * X;
   end Double;

   procedure Use_It (Obj : in Int_Ptr) is
   -- Use Obj so that the compiler cannot remove the actual from
   -- the program as a dead object.
   begin
      if Obj = null then
         null;
      elsif not Report.Equal (Obj.all, Obj.all) then
         Report.Comment ("Don't optimize Obj");
      end if;
   end Use_It;

   procedure Use_It_2 (Obj : access Integer) is
   -- Use Obj so that the compiler cannot remove the actual from
   -- the program as a dead object.
   begin
      if Obj = null then
         null;
      elsif not Report.Equal (Obj.all, Obj.all) then
         Report.Comment ("Don't optimize Obj");
      end if;
   end Use_It_2;

   procedure Use_It_3 (Obj : access function (X : Float) return Float) is
   -- Use Obj so that the compiler cannot remove the actual from
   -- the program as a dead object.
   begin
      if Obj = null then
         null;
      elsif Report.Equal (Integer (Obj (2.0)), 0) then
         Report.Comment ("Don't optimize Obj");
      end if;
   end Use_It_3;

   -- Objects:
   Obj1 : Int_Ptr                 := Non_Null_Init;
   Obj2 : Nn_Int_Ptr_Array        := (others => Non_Null_Init);
   Obj3 : not null Int_Ptr        := Non_Null_Init;
   Obj4 : Nn_Int_Ptr              := Non_Null_Init;
   Obj5 : access Integer          := Aux'Access;
   Obj6 : not null access Integer := Aux'Access;
   Obj7 : Nn_Int_Ptr_Ptr          := new Int_Ptr'(Non_Null_Init);
   Obj8 : Acc_Ptr                 := Double'Access;
   Obj9 : not null Acc_Ptr        := Double'Access;
   Obja : Nn_Acc_Ptr              := Double'Access;
   Objb : not null access function (X : Float) return Float := Double'Access;

   type Rec_Type is record
      Comp1 : Int_Ptr          := Non_Null_Init;
      Comp2 : not null Int_Ptr := Non_Null_Init;
      Comp3 : Nn_Int_Ptr       := Non_Null_Init;
   end record;

   Rec : Rec_Type;

   procedure Test_Param
     (Param1 : in out not null Int_Ptr;
      Param2 : in out Nn_Int_Ptr;
      Param3 : in out not null Acc_Ptr)
   is
      Renp1 : Int_Ptr renames Param1;
      Renp2 : Int_Ptr renames Param2;
      Renp3 : Acc_Ptr renames Param3;
   begin
      begin
         Renp1 := Null_Init; -- Raises Constraint_Error.
         Report.Failed ("Constraint_Error not raised (P1)");
         Use_It (Renp1);
      exception
         when Constraint_Error =>
            null;
      end;
      begin
         Renp2 := Null_Init; -- Raises Constraint_Error.
         Report.Failed ("Constraint_Error not raised (P2)");
         Use_It (Renp2);
      exception
         when Constraint_Error =>
            null;
      end;
      begin
         Renp3 := null; -- Raises Constraint_Error.
         Report.Failed ("Constraint_Error not raised (P3)");
         Use_It_3 (Renp3);
      exception
         when Constraint_Error =>
            null;
      end;
   end Test_Param;

begin
   Report.Test
     ("C851001",
      "Check that when renaming an object that excludes " &
      "null, the renamed object still excludes null even " &
      "if the renaming_declaration does not include " &
      "a null_exclusion");

   declare
      Ren1 : Int_Ptr renames Obj1;
   begin
      Ren1 := Null_Init; -- OK.
      Use_It (Ren1);
   exception
      when Constraint_Error =>
         Report.Failed ("Can't assign null (1)");
   end;

   declare
      Ren2 : Int_Ptr renames Obj2 (2);
   begin
      Ren2 := Null_Init; -- Raises Constraint_Error.
      Report.Failed ("Constraint_Error not raised (2)");
      Use_It (Ren2);
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      Ren3 : Int_Ptr renames Obj3;
   begin
      Ren3 := Null_Init; -- Raises Constraint_Error.
      Report.Failed ("Constraint_Error not raised (3)");
      Use_It (Ren3);
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      Ren4 : Int_Ptr renames Obj4;
   begin
      Ren4 := Null_Init; -- Raises Constraint_Error.
      Report.Failed ("Constraint_Error not raised (4)");
      Use_It (Ren4);
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      Ren5 : access Integer renames Obj5;
   begin
      Ren5 := Null_Init; -- OK.
      Use_It_2 (Ren5);
   exception
      when Constraint_Error =>
         Report.Failed ("Can't assign null (5)");
   end;

   declare
      Ren6 : access Integer renames Obj6;
   begin
      Ren6 := Null_Init; -- Raises Constraint_Error.
      Report.Failed ("Constraint_Error not raised (6)");
      Use_It_2 (Ren6);
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      Ren7 : Int_Ptr renames Obj7.all;
   begin
      Ren7 := Null_Init; -- Raises Constraint_Error.
      Report.Failed ("Constraint_Error not raised (7)");
      Use_It (Ren7);
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      Ren8 : Acc_Ptr renames Obj8;
   begin
      Ren8 := null; -- OK.
      Use_It_3 (Ren8);
   exception
      when Constraint_Error =>
         Report.Failed ("Can't assign null (8)");
   end;

   declare
      Ren9 : Acc_Ptr renames Obj9;
   begin
      Ren9 := null; -- Raises Constraint_Error.
      Report.Failed ("Constraint_Error not raised (9)");
      Use_It_3 (Ren9);
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      Rena : Acc_Ptr renames Obja;
   begin
      Rena := null; -- Raises Constraint_Error.
      Report.Failed ("Constraint_Error not raised (A)");
      Use_It_3 (Rena);
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      Renb : access function (X : Float) return Float renames Objb;
   begin
      Renb := null; -- Raises Constraint_Error.
      Report.Failed ("Constraint_Error not raised (B)");
      Use_It_3 (Renb);
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      Renc1 : Int_Ptr renames Rec.Comp1;
   begin
      Renc1 := Null_Init; -- OK.
      Use_It (Renc1);
   exception
      when Constraint_Error =>
         Report.Failed ("Can't assign null (C1)");
   end;

   declare
      Renc2 : Int_Ptr renames Rec.Comp2;
   begin
      Renc2 := Null_Init; -- Raises Constraint_Error.
      Report.Failed ("Constraint_Error not raised (C2)");
      Use_It (Renc2);
   exception
      when Constraint_Error =>
         null;
   end;

   declare
      Renc3 : Int_Ptr renames Rec.Comp3;
   begin
      Renc3 := Null_Init; -- Raises Constraint_Error.
      Report.Failed ("Constraint_Error not raised (C3)");
      Use_It (Renc3);
   exception
      when Constraint_Error =>
         null;
   end;

   Test_Param (Obj3, Obj4, Obja);

   Report.Result;

end C851001;
