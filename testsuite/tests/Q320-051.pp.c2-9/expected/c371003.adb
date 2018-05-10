-- C371003.A
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
--      Check that if a discriminant constraint depends on a discriminant,
--      the evaluation of the expressions in the constraint is deferred
--      until an object of the subtype is created.  Check for cases of
--      records where the component containing the constraint is present
--      in the subtype.
--
-- TEST DESCRIPTION:
--      This transition test defines record types with discriminant components
--      which depend on the discriminants.  The discriminants are calculated
--      by function calls.  The test verifies that Constraint_Error is raised
--      during the object creations when values of discriminants are
--      incompatible with the subtypes.  Also check for cases, where the
--      component is absent.
--
--      Inspired by C37213E.ADA, C37213G.ADA, C37215E.ADA, and C37215G.ADA.
--
--
-- CHANGE HISTORY:
--      10 Apr 96   SAIC    Initial version for ACVC 2.1.
--      14 Jul 96   SAIC    Modified test description.  Added exception handler
--                          for VObj_10 assignment.
--      26 Oct 96   SAIC    Added LM references.
--
--!

with Report;

procedure C371003 is

   subtype Small_Int is Integer range 1 .. 10;

   type Rec_W_Disc (Disc1, Disc2 : Small_Int) is record
      Str1 : String (1 .. Disc1) := (others => '*');
      Str2 : String (1 .. Disc2) := (others => '*');
   end record;

   type My_Array is array (Small_Int range <>) of Integer;

   Func1_Cons : Integer := 0;

   ---------------------------------------------------------
   function Chk (Cons : Integer; Value : Integer;
      Message         : String) return Boolean
   is
   begin
      if Cons /= Value then
         Report.Failed
           (Message & ": Func1_Cons is " & Integer'Image (Func1_Cons));
      end if;
      return True;
   end Chk;

   ---------------------------------------------------------
   function Func1 return Integer is
   begin
      Func1_Cons := Func1_Cons + Report.Ident_Int (1);
      return Func1_Cons;
   end Func1;

begin
   Report.Test
     ("C371003",
      "Check that if a discriminant constraint " &
      "depends on a discriminant, the evaluation of the " &
      "expressions in the constraint is deferred until " &
      "object declarations");

   ---------------------------------------------------------
   declare
      type Vrec_01 (D3 : Integer) is record
         case D3 is
            when -5 .. 10 =>
               C1 : Rec_W_Disc (D3, Func1);    -- Func1 evaluated, value 1.
            when others =>
               C2 : Integer := Report.Ident_Int (0);
         end case;
      end record;

      Chk1 : Boolean := Chk (Func1_Cons, 1, "Func1 not evaluated for VRec_01");

      Vobj_1 : Vrec_01 (1);                     -- Func1 not evaluated again
      Vobj_2 : Vrec_01 (2);                     -- Func1 not evaluated again

      Chk2 : Boolean := Chk (Func1_Cons, 1, "Func1 evaluated too many times");

   begin
      if Vobj_1 /=
        (D3 => 1,
         C1 =>
           (Disc1 => 1, Disc2 => 1, Str1 => (others => '*'),
            Str2  => (others => '*'))) or
        Vobj_2 /=
          (D3 => 2,
           C1 =>
             (Disc1 => 2, Disc2 => 1, Str1 => (others => '*'),
              Str2  => (others => '*')))
      then
         Report.Failed ("VObj_1 & VObj_2 - Discriminant values not correct");
      end if;
   end;

   ---------------------------------------------------------
   Func1_Cons := -11;

   declare
      type Vrec_Of_Vrec_01 (D3 : Integer) is record
         case D3 is
            when -5 .. 10 =>
               C1 : Rec_W_Disc (Func1, D3);   -- Func1 evaluated, value -10.
            when others =>                    -- Constraint_Error not raised.
               C2 : Integer := Report.Ident_Int (0);
         end case;
      end record;

      type Vrec_Of_Vrec_02 (D3 : Integer) is record
         case D3 is
            when -5 .. 10 =>
               C1 : Rec_W_Disc (1, D3);
            when others =>
               C2 : Integer := Report.Ident_Int (0);
         end case;
      end record;

      type Vrec_Of_Myarr_01 (D3 : Integer) is record
         case D3 is
            when -5 .. 10 =>
               C1 : My_Array (Func1 .. D3);    -- Func1 evaluated, value -9.
            when others =>                    -- Constraint_Error not raised.
               C2 : Integer := Report.Ident_Int (0);
         end case;
      end record;

      type Vrec_Of_Myarr_02 (D3 : Integer) is record
         case D3 is
            when -5 .. 10 =>
               C1 : My_Array (D3 .. 1);
            when others =>
               C2 : Integer := Report.Ident_Int (0);
         end case;
      end record;

   begin

      ---------------------------------------------------------
      -- Component containing the constraint is present.
      begin
         declare
            Vobj_3 : Vrec_Of_Vrec_01 (1);        -- Constraint_Error raised.
         begin
            Report.Failed ("VObj_3 - Constraint_Error should be raised");
            if Vobj_3 /= (1, (1, 1, others => (others => '*'))) then
               Report.Comment ("VObj_3 - Shouldn't get here");
            end if;
         end;

      exception
         when Constraint_Error =>               -- Exception expected.
            null;
         when others =>
            Report.Failed ("VObj_3 - unexpected exception raised");
      end;

      ---------------------------------------------------------
      -- Component containing the constraint is present.
      begin
         declare
            subtype Subtype_Vrec is             -- No Constraint_Error raised.
              Vrec_Of_Vrec_01 (Report.Ident_Int (1));
         begin
            declare
               Vobj_4 : Subtype_Vrec;           -- Constraint_Error raised.
            begin
               Report.Failed ("VObj_4 - Constraint_Error should be raised");
               if Vobj_4 /=
                 (D3 => 1,
                  C1 =>
                    (Disc1 => 1, Disc2 => 1, Str1 => (others => '*'),
                     Str2  => (others => '*')))
               then
                  Report.Comment ("VObj_4 - Shouldn't get here");
               end if;
            end;

         exception
            when Constraint_Error =>            -- Exception expected.
               null;
            when others =>
               Report.Failed ("VObj_4 - unexpected exception raised");
         end;

      exception
         when Constraint_Error =>
            Report.Failed ("Subtype_VRec - Constraint_Error raised");
         when others =>
            Report.Failed ("Subtype_VRec - unexpected exception raised");
      end;

      ---------------------------------------------------------
      -- Component containing the constraint is absent.
      begin
         declare
            type Arr is
              array
                (1 ..
                     5) of Vrec_Of_Vrec_01
                (Report.Ident_Int (-6)); -- No Constraint_Error
            Vobj_5 : Arr;                            -- for either declaration.

         begin
            if Vobj_5 /= (1 .. 5 => (-6, 0)) then
               Report.Comment ("VObj_5 - wrong values");
            end if;
         end;

      exception
         when others =>
            Report.Failed ("Arr - unexpected exception raised");
      end;

      ---------------------------------------------------------
      -- Component containing the constraint is present.
      begin
         declare
            type Rec_Of_Rec_Of_Myarr is record
               C1 : Vrec_Of_Myarr_01 (1);    -- No Constraint_Error raised.
            end record;
         begin
            declare
               Obj_6 : Rec_Of_Rec_Of_Myarr;   -- Constraint_Error raised.
            begin
               Report.Failed ("Obj_6 - Constraint_Error should be raised");
               if Obj_6 /= (C1 => (1, (1, 1))) then
                  Report.Comment ("Obj_6 - Shouldn't get here");
               end if;
            end;

         exception
            when Constraint_Error =>         -- Exception expected.
               null;
            when others =>
               Report.Failed ("Obj_6 - unexpected exception raised");
         end;

      exception
         when Constraint_Error =>
            Report.Failed ("Rec_Of_Rec_Of_MyArr - Constraint_Error raised");
         when others =>
            Report.Failed
              ("Rec_Of_Rec_Of_MyArr - unexpected exception " & "raised");
      end;

      ---------------------------------------------------------
      -- Component containing the constraint is absent.
      begin
         declare
            type New_Vrec_Arr is
              new Vrec_Of_Myarr_01 (11);       -- No Constraint_Error raised
            Obj_7 : New_Vrec_Arr;             -- for either declaration.

         begin
            if Obj_7 /= (11, 0) then
               Report.Failed ("Obj_7 - value incorrect");
            end if;
         end;

      exception
         when others =>
            Report.Failed ("New_VRec_Arr - unexpected exception raised");
      end;

      ---------------------------------------------------------
      -- Component containing the constraint is present.
      begin
         declare
            type New_Vrec is
              new Vrec_Of_Vrec_02
                (Report.Ident_Int (0)); -- No Constraint_Error
            -- raised.
         begin
            declare
               Vobj_8 : New_Vrec;                  -- Constraint_Error raised.
            begin
               Report.Failed ("VObj_8 - Constraint_Error should be raised");
               if Vobj_8 /= (1, (1, 1, others => (others => '*'))) then
                  Report.Comment ("VObj_8 - Shouldn't get here");
               end if;
            end;

         exception
            when Constraint_Error =>               -- Exception expected.
               null;
            when others =>
               Report.Failed ("VObj_8 - unexpected exception raised");
         end;

      exception
         when Constraint_Error =>
            Report.Failed ("New_VRec - Constraint_Error raised");
         when others =>
            Report.Failed ("New_VRec - unexpected exception raised");
      end;

      ---------------------------------------------------------
      -- Component containing the constraint is absent.
      begin
         declare
            subtype Sub_Vrec is
              Vrec_Of_Vrec_02 (Report.Ident_Int (11)); -- No Constraint_Error
            Vobj_9 : Sub_Vrec;                       -- raised for either
            -- declaration.
         begin
            if Vobj_9 /= (11, 0) then
               Report.Comment ("VObj_9 - wrong values");
            end if;
         end;

      exception
         when others =>
            Report.Failed ("Sub_VRec - unexpected exception raised");
      end;

      ---------------------------------------------------------
      -- Component containing the constraint is present.
      begin
         declare
            type Acc_Vrec_01 is
              access Vrec_Of_Vrec_02
                (Report.Ident_Int (0));  -- No Constraint_Error
            -- raised.
         begin
            declare
               Vobj_10 : Acc_Vrec_01;                -- No Constraint_Error
               -- raised.
            begin
               Vobj_10 :=
                 new Vrec_Of_Vrec_02
                   (Report.Ident_Int (0));   -- Constraint_Error
               -- raised.
               Report.Failed ("VObj_10 - Constraint_Error should be raised");
               if Vobj_10.all /= (1, (1, 1, others => (others => '*'))) then
                  Report.Comment ("VObj_10 - Shouldn't get here");
               end if;

            exception
               when Constraint_Error =>              -- Exception expected.
                  null;
               when others =>
                  Report.Failed ("VObj_10 - unexpected exception raised");
            end;

         exception
            when Constraint_Error =>
               Report.Failed ("VObj_10 - Constraint_Error exception raised");
            when others =>
               Report.Failed
                 ("VObj_10 - unexpected exception raised at " & "declaration");
         end;

      exception
         when Constraint_Error =>
            Report.Failed ("Acc_VRec_01 - Constraint_Error raised");
         when others =>
            Report.Failed ("Acc_VRec_01 - unexpected exception raised");
      end;

      ---------------------------------------------------------
      -- Component containing the constraint is absent.
      begin
         declare
            type Acc_Vrec_02 is
              access Vrec_Of_Vrec_02
                (11);                  -- No Constraint_Error
            -- raised for either
            Vobj_11 : Acc_Vrec_02;                 -- declaration.

         begin
            Vobj_11 := new Vrec_Of_Vrec_02 (11);
            if Vobj_11.all /= (11, 0) then
               Report.Comment ("VObj_11 - wrong values");
            end if;
         end;

      exception
         when others =>
            Report.Failed ("Acc_VRec_02 - unexpected exception raised");
      end;

      ---------------------------------------------------------
      -- Component containing the constraint is present.
      begin
         declare
            type Acc_Vrec_03 is
              access Vrec_Of_Myarr_02;                    -- No Constraint_Error
            -- raised for either
            Vobj_12 : Acc_Vrec_03;                 -- declaration.
         begin
            Vobj_12 :=
              new Vrec_Of_Myarr_02
                (Report.Ident_Int (0)); -- Constraint_Error raised.

            Report.Failed ("VObj_12 - Constraint_Error should be raised");
            if Vobj_12.all /= (1, (1, 1)) then
               Report.Comment ("VObj_12 - Shouldn't get here");
            end if;

         exception
            when Constraint_Error =>              -- Exception expected.
               null;
            when others =>
               Report.Failed ("VObj_12 - unexpected exception raised");
         end;

      exception
         when Constraint_Error =>
            Report.Failed ("Acc_VRec_03 - Constraint_Error raised");
         when others =>
            Report.Failed ("Acc_VRec_03 - unexpected exception raised");
      end;

      ---------------------------------------------------------
      -- Component containing the constraint is absent.
      begin
         declare
            type Acc_Vrec_04 is
              access Vrec_Of_Myarr_02
                (11);                 -- No Constraint_Error
            -- raised for either
            Vobj_13 : Acc_Vrec_04;                 -- declaration.

         begin
            Vobj_13 := new Vrec_Of_Myarr_02 (11);
            if Vobj_13.all /= (11, 0) then
               Report.Comment ("VObj_13 - wrong values");
            end if;
         end;

      exception
         when others =>
            Report.Failed ("Acc_VRec_04 - unexpected exception raised");
      end;

   end;

   Report.Result;

exception
   when others =>
      Report.Failed ("Discriminant value checked too soon");
      Report.Result;

end C371003;
