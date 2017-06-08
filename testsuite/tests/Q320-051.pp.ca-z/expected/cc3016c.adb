-- CC3016C.ADA

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
--  CHECK THAT AN INSTANCE OF A GENERIC PACKAGE MUST DECLARE A PACKAGE. CHECK
--  THAT THE STATEMENTS IN AN INSTANTIATED GENERIC PACKAGE BODY ARE EXECUTED
--  AFTER THE ELABORATION OF THE DECLARATIONS (IN SPEC AND IN BODY).

-- HISTORY:
--         EDWARD V. BERARD, 8 AUGUST 1990

with Report;

procedure Cc3016c is

   generic

      type Some_Type is private;
      First_Initial_Value : in Some_Type;
      Second_Initial_Value : in Some_Type;
      with procedure Change (First : in Some_Type; Result : out Some_Type);
      with procedure Second_Change
        (First  : in     Some_Type;
         Result :    out Some_Type);
      with procedure Third_Change
        (First  : in     Some_Type;
         Result :    out Some_Type);
      First_Expected_Result : in Some_Type;
      Second_Expected_Result : in Some_Type;
      Third_Expected_Result : in Some_Type;
      Fourth_Expected_Result : in Some_Type;
      Fifth_Expected_Result : in Some_Type;
      Sixth_Expected_Result : in Some_Type;

   package Outer is

      Variable : Some_Type := First_Initial_Value;

      function Inner_Variable return Some_Type;

      generic

         Initial_Value : in Some_Type;
         with procedure Change (First : in Some_Type; Result : out Some_Type);
         with procedure Second_Change
           (First  : in     Some_Type;
            Result :    out Some_Type);
         First_Expected_Result : in Some_Type;
         Second_Expected_Result : in Some_Type;
         Third_Expected_Result : in Some_Type;
         Fourth_Expected_Result : in Some_Type;

      package Inner is
         Variable : Some_Type := Initial_Value;
      end Inner;

   end Outer;

   package body Outer is

      Another_Variable : Some_Type := First_Initial_Value;

      package body Inner is
         Another_Variable : Some_Type := Initial_Value;
      begin  -- INNER

         Change (First => Variable, Result => Variable);
         Change (First => Another_Variable, Result => Another_Variable);
         Outer.Second_Change
           (First  => Outer.Variable,
            Result => Outer.Variable);
         Outer.Change
           (First  => Outer.Another_Variable,
            Result => Outer.Another_Variable);

         if (Variable /= First_Expected_Result) or
           (Another_Variable /= Second_Expected_Result) or
           (Outer.Variable /= Third_Expected_Result) or
           (Outer.Another_Variable /= Fourth_Expected_Result)
         then
            Report.Failed ("ASSIGNED VALUES INCORRECT - BODY OF INNER");
         end if;

      end Inner;

      package New_Inner is new Inner
        (Initial_Value          => Second_Initial_Value,
         Change                 => Change,
         Second_Change          => Third_Change,
         First_Expected_Result  => First_Expected_Result,
         Second_Expected_Result => Second_Expected_Result,
         Third_Expected_Result  => Third_Expected_Result,
         Fourth_Expected_Result => Fourth_Expected_Result);

      function Inner_Variable return Some_Type is
      begin
         return New_Inner.Variable;
      end Inner_Variable;

   begin  -- OUTER

      Second_Change (First => Variable, Result => Variable);
      Second_Change (First => Another_Variable, Result => Another_Variable);

      if (Variable /= Fifth_Expected_Result) or
        (Another_Variable /= Sixth_Expected_Result) or
        (New_Inner.Variable /= First_Expected_Result)
      then
         Report.Failed ("ASSIGNED VALUES INCORRECT - BODY OF OUTER");
      end if;

   end Outer;

   procedure Double (This_Value : in Integer; Giving_This_Result : out Integer)
   is
   begin -- DOUBLE
      Giving_This_Result := 2 * This_Value;
   end Double;

   procedure Add_20
     (To_This_Value      : in     Integer;
      Giving_This_Result :    out Integer)
   is
   begin -- ADD_20
      Giving_This_Result := To_This_Value + 20;
   end Add_20;

   procedure Times_Five
     (This_Value         : in     Integer;
      Giving_This_Result :    out Integer)
   is
   begin -- TIMES_FIVE
      Giving_This_Result := 5 * This_Value;
   end Times_Five;

begin -- CC3016C

   Report.Test
     ("CC3016C",
      "CHECK THAT AN INSTANCE OF A GENERIC PACKAGE " &
      "MUST DECLARE A PACKAGE. CHECK THAT THE STATEMENTS IN AN " &
      "INSTANTIATED GENERIC PACKAGE BODY ARE EXECUTED AFTER THE " &
      "ELABORATION OF THE DECLARATIONS (IN SPEC AND IN BODY).");

   Local_Block :
 declare

      package New_Outer is new Outer
        (Some_Type              => Integer,
         First_Initial_Value    => 7,
         Second_Initial_Value   => 11,
         Change                 => Double,
         Second_Change          => Add_20,
         Third_Change           => Times_Five,
         First_Expected_Result  => 22,
         Second_Expected_Result => 22,
         Third_Expected_Result  => 27,
         Fourth_Expected_Result => 14,
         Fifth_Expected_Result  => 47,
         Sixth_Expected_Result  => 34);

   begin  -- LOCAL_BLOCK

      if (New_Outer.Variable /= 47) or (New_Outer.Inner_Variable /= 22) then
         Report.Failed
           ("ASSIGNED VALUES INCORRECT - " & "BODY OF MAIN PROGRAM");
      end if;

   end Local_Block;

   Report.Result;

end Cc3016c;
