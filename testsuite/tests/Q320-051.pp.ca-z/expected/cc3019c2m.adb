-- CC3019C2M.ADA

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
--   CHECK INSTANTIATIONS OF UNITS WITHIN GENERIC UNITS, E.G.
--   TO SUPPORT ITERATORS.

--  THIS TEST SPECIFICALLY CHECKS THAT A NESTING LEVEL OF 3 IS SUPPORTED FOR
--  GENERICS:
--       INSTANTIATE CC3019C1_NESTED_GENERICS IN THE MAIN
--       PROCEDURE, THE INSTANTIATION OF CC3019C0_LIST_CLASS
--       IN GENERIC PACKAGE CC3019C1_NESTED_GENERICS, AND
--       THE INSTANTIATION OF NEW_LIST_CLASS.ITERATE IN
--       PROCEDURE ITERATE IN PACKAGE BODY STACK_CLASS.
--
--  *** THIS IS THE MAIN PROGRAM. IT MUST BE COMPILED AFTER THE *** SOURCE CODE
--  IN FILES CC3019C0.ADA AND CC3019C1.ADA HAVE *** BEEN COMPILED.
--
-- HISTORY:
--         EDWARD V. BERARD, 31 AUGUST 1990

with Report;
with Cc3019c1_Nested_Generics;

procedure Cc3019c2m is

   type Month_Type is
     (Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec);
   type Day_Type is range 1 .. 31;
   type Year_Type is range 1_904 .. 2_050;
   type Date is record
      Month : Month_Type;
      Day   : Day_Type;
      Year  : Year_Type;
   end record;

   Store_Date : Date;

   Today : Date := (Month => Aug, Day => 31, Year => 1_990);

   First_Date : Date := (Month => Jun, Day => 4, Year => 1_967);

   Birth_Date : Date := (Month => Oct, Day => 3, Year => 1_949);

   Wall_Date : Date := (Month => Nov, Day => 9, Year => 1_989);

   type Sex is (Male, Female);

   type Person is record
      Birth_Date : Date;
      Gender     : Sex;
      Name       : String (1 .. 10);
   end record;

   First_Person  : Person;
   Second_Person : Person;

   Myself : Person :=
     (Birth_Date => Birth_Date, Gender => Male, Name => "ED        ");

   Friend : Person :=
     (Birth_Date => Date'(Dec, 27, 1_949),
      Gender     => Male,
      Name       => "DENNIS    ");

   Father : Person :=
     (Birth_Date => Date'(Jul, 5, 1_925),
      Gender     => Male,
      Name       => "EDWARD    ");

   Daughter : Person :=
     (Birth_Date => Date'(Dec, 10, 1_980),
      Gender     => Female,
      Name       => "CHRISSY   ");

   procedure Assign
     (The_Value_Of_This_Date : in out Date;
      To_This_Date           : in out Date);

   function Is_Equal (Left : in Date; Right : in Date) return Boolean;

   procedure Assign
     (The_Value_Of_This_Person : in out Person;
      To_This_Person           : in out Person);

   function Is_Equal (Left : in Person; Right : in Person) return Boolean;

--  INSTANTIATE OUTER GENERIC PACKAGE

   package New_Nested_Generics is new Cc3019c1_Nested_Generics
     (Element => Date,
      Assign  => Assign,
      "="     => Is_Equal);

   First_Nng  : New_Nested_Generics.Nested_Generics_Type;
   Second_Nng : New_Nested_Generics.Nested_Generics_Type;

   function "="
     (Left  : in New_Nested_Generics.Nested_Generics_Type;
      Right : in New_Nested_Generics.Nested_Generics_Type)
      return Boolean renames
     New_Nested_Generics."=";

--  INSTANTIATE NESTED TASK PACKAGE

   package New_Generic_Task is new New_Nested_Generics.Generic_Task
     (Element => Person,
      Assign  => Assign);

   First_Generic_Task  : New_Generic_Task.Protected_Area;
   Second_Generic_Task : New_Generic_Task.Protected_Area;

--  INSTANTIATE NESTED STACK PACKAGE

   package Person_Stack is new New_Nested_Generics.Stack_Class
     (Element => Person,
      Assign  => Assign,
      "="     => Is_Equal);

   First_Person_Stack  : Person_Stack.Stack;
   Second_Person_Stack : Person_Stack.Stack;
   Third_Person_Stack  : Person_Stack.Stack;

   function "="
     (Left  : in Person_Stack.Stack;
      Right : in Person_Stack.Stack) return Boolean renames
     Person_Stack."=";

   procedure Assign
     (The_Value_Of_This_Date : in out Date;
      To_This_Date           : in out Date)
   is

   begin -- ASSIGN

      To_This_Date := The_Value_Of_This_Date;

   end Assign;

   function Is_Equal (Left : in Date; Right : in Date) return Boolean is

   begin -- IS_EQUAL

      if (Left.Month = Right.Month) and
        (Left.Day = Right.Day) and
        (Left.Year = Right.Year)
      then
         return True;
      else
         return False;
      end if;

   end Is_Equal;

   procedure Assign
     (The_Value_Of_This_Person : in out Person;
      To_This_Person           : in out Person)
   is

   begin -- ASSIGN

      To_This_Person := The_Value_Of_This_Person;

   end Assign;

   function Is_Equal (Left : in Person; Right : in Person) return Boolean is

   begin -- IS_EQUAL

      if (Left.Birth_Date = Right.Birth_Date) and
        (Left.Gender = Right.Gender) and
        (Left.Name = Right.Name)
      then
         return True;
      else
         return False;
      end if;

   end Is_Equal;

begin  -- CC3019C2M

   Report.Test
     ("CC3019C2M",
      "CHECK INSTANTIATIONS OF UNITS WITHIN GENERIC " &
      "UNITS, E.G., TO SUPPORT ITERATORS. THIS TEST " &
      "SPECIFICALLY CHECKS THAT A NESTING LEVEL OF 3 " &
      "IS SUPPORTED FOR GENERICS.");

-- CHECK THE OUTERMOST GENERIC (NEW_NESTED_GENERICS)

   New_Nested_Generics.Set_Element
     (For_This_Ngt_Object => First_Nng,
      To_This_Element     => Today);
   New_Nested_Generics.Set_Number
     (For_This_Ngt_Object => First_Nng,
      To_This_Number      => 1);

   New_Nested_Generics.Set_Element
     (For_This_Ngt_Object => Second_Nng,
      To_This_Element     => First_Date);
   New_Nested_Generics.Set_Number
     (For_This_Ngt_Object => Second_Nng,
      To_This_Number      => 2);

   if First_Nng = Second_Nng then
      Report.Failed
        ("PROBLEMS WITH TESTING EQUALITY FOR " & "OUTERMOST GENERIC");
   end if;

   if
     (New_Nested_Generics.Element_Of (This_Ngt_Object => First_Nng) /=
      Today) or
     (New_Nested_Generics.Element_Of (This_Ngt_Object => Second_Nng) /=
      First_Date)
   then
      Report.Failed
        ("PROBLEMS WITH EXTRACTING ELEMENTS IN " & "OUTERMOST GENERIC");
   end if;

   if (New_Nested_Generics.Number_Of (This_Ngt_Object => First_Nng) /= 1) or
     (New_Nested_Generics.Number_Of (This_Ngt_Object => Second_Nng) /= 2)
   then
      Report.Failed
        ("PROBLEMS WITH EXTRACTING NUMBERS IN " & "OUTERMOST GENERIC");
   end if;

   New_Nested_Generics.Copy (Source => First_Nng, Destination => Second_Nng);

   if First_Nng /= Second_Nng then
      Report.Failed
        ("PROBLEMS WITH COPYING OR TESTING EQUALITY " &
         "IN OUTERMOST GENERIC");
   end if;

-- CHECK THE FIRST NESTED GENERIC (GENERIC_TASK)

   First_Generic_Task.Store (Item => Myself);
   Second_Generic_Task.Store (Item => Friend);

   First_Generic_Task.Get (Item => First_Person);
   Second_Generic_Task.Get (Item => Second_Person);

   if (First_Person /= Myself) or (Second_Person /= Friend) then
      Report.Failed ("PROBLEMS WITH NESTED TASK GENERIC");
   end if;

-- CHECK THE SECOND NESTED GENERIC (STACK_CLASS)

   Person_Stack.Clear (This_Stack => First_Person_Stack);
   if Person_Stack.Number_Of_Elements (On_This_Stack => First_Person_Stack) /=
     0
   then
      Report.Failed ("IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 1");
   end if;

   Person_Stack.Push
     (This_Element     => Myself,
      On_To_This_Stack => First_Person_Stack);
   if Person_Stack.Number_Of_Elements (On_This_Stack => First_Person_Stack) /=
     1
   then
      Report.Failed ("IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 2");
   end if;

   Person_Stack.Push
     (This_Element     => Friend,
      On_To_This_Stack => First_Person_Stack);
   if Person_Stack.Number_Of_Elements (On_This_Stack => First_Person_Stack) /=
     2
   then
      Report.Failed ("IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 3");
   end if;

   Person_Stack.Push
     (This_Element     => Father,
      On_To_This_Stack => First_Person_Stack);
   if Person_Stack.Number_Of_Elements (On_This_Stack => First_Person_Stack) /=
     3
   then
      Report.Failed ("IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 4");
   end if;

   Person_Stack.Pop
     (This_Element   => First_Person,
      Off_This_Stack => First_Person_Stack);
   if Person_Stack.Number_Of_Elements (On_This_Stack => First_Person_Stack) /=
     2
   then
      Report.Failed ("IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 5");
   end if;

   if First_Person /= Father then
      Report.Failed ("IMPROPER VALUE REMOVED FROM STACK - 1");
   end if;

   Person_Stack.Clear (This_Stack => Second_Person_Stack);
   if Person_Stack.Number_Of_Elements (On_This_Stack => Second_Person_Stack) /=
     0
   then
      Report.Failed ("IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 6");
   end if;

   Person_Stack.Copy
     (This_Stack    => First_Person_Stack,
      To_This_Stack => Second_Person_Stack);

   if First_Person_Stack /= Second_Person_Stack then
      Report.Failed ("PROBLEMS WITH COPY OR TEST FOR EQUALITY (STACK)");
   end if;

   Person_Stack.Pop
     (This_Element   => First_Person,
      Off_This_Stack => Second_Person_Stack);
   Person_Stack.Push
     (This_Element     => Daughter,
      On_To_This_Stack => Second_Person_Stack);
   if First_Person_Stack = Second_Person_Stack then
      Report.Failed ("PROBLEMS WITH POP OR TEST FOR EQUALITY (STACK)");
   end if;

   Underflow_Exception_Test :
 begin  -- UNDERFLOW_EXCEPTION_TEST

      Person_Stack.Clear (This_Stack => Third_Person_Stack);
      Person_Stack.Pop
        (This_Element   => First_Person,
         Off_This_Stack => Third_Person_Stack);
      Report.Failed ("UNDERFLOW EXCEPTION NOT RAISED");

   exception

      when Person_Stack.Underflow =>
         null;  -- CORRECT EXCEPTION
      -- RAISED
      when others =>
         Report.Failed
           ("INCORRECT EXCEPTION RAISED IN " & "UNDERFLOW EXCEPTION TEST");

   end Underflow_Exception_Test;

   Overflow_Exception_Test :
 begin  -- OVERFLOW_EXCEPTION_TEST

      Person_Stack.Clear (This_Stack => Third_Person_Stack);
      for Index in 1 .. 10 loop
         Person_Stack.Push
           (This_Element     => Myself,
            On_To_This_Stack => Third_Person_Stack);
      end loop;

      Person_Stack.Push
        (This_Element     => Myself,
         On_To_This_Stack => Third_Person_Stack);
      Report.Failed ("OVERFLOW EXCEPTION NOT RAISED");

   exception

      when Person_Stack.Overflow =>
         null;  -- CORRECT EXCEPTION
      -- RAISED
      when others =>
         Report.Failed
           ("INCORRECT EXCEPTION RAISED IN " & "OVERFLOW EXCEPTION TEST");

   end Overflow_Exception_Test;

   Local_Block :
 declare

      type Person_Table is array (Positive range 1 .. 10) of Person;

      First_Person_Table : Person_Table;

      Table_Index : Positive := 1;

      procedure Gather_People
        (This_Person : in     Person;
         Continue    :    out Boolean);

      procedure Show_People (This_Person : in Person; Continue : out Boolean);

      procedure Gather_Person_Iterate is new Person_Stack.Iterate
        (Process => Gather_People);

      procedure Show_Person_Iterate is new Person_Stack.Iterate
        (Process => Show_People);

      procedure Gather_People
        (This_Person : in     Person;
         Continue    :    out Boolean)
      is
      begin  -- GATHER_PEOPLE

         First_Person_Table (Table_Index) := This_Person;
         Table_Index                      := Table_Index + 1;

         Continue := True;

      end Gather_People;

      procedure Show_People
        (This_Person : in     Person;
         Continue    :    out Boolean)
      is

      begin  -- SHOW_PEOPLE

         Report.Comment
           ("THE BIRTH MONTH IS " &
            Month_Type'Image (This_Person.Birth_Date.Month));
         Report.Comment
           ("THE BIRTH DAY IS " & Day_Type'Image (This_Person.Birth_Date.Day));
         Report.Comment
           ("THE BIRTH YEAR IS " &
            Year_Type'Image (This_Person.Birth_Date.Year));
         Report.Comment ("THE GENDER IS " & Sex'Image (This_Person.Gender));
         Report.Comment ("THE NAME IS " & This_Person.Name);

         Continue := True;

      end Show_People;

   begin  -- LOCAL_BLOCK

      Report.Comment ("CONTENTS OF THE FIRST STACK");
      Show_Person_Iterate (Over_This_Stack => First_Person_Stack);

      Report.Comment ("CONTENTS OF THE SECOND STACK");
      Show_Person_Iterate (Over_This_Stack => Second_Person_Stack);

      Gather_Person_Iterate (Over_This_Stack => First_Person_Stack);
      if (First_Person_Table (1) /= Myself) or
        (First_Person_Table (2) /= Friend)
      then
         Report.Failed ("PROBLEMS WITH ITERATION - 1");
      end if;

      Table_Index := 1;
      Gather_Person_Iterate (Over_This_Stack => Second_Person_Stack);
      if (First_Person_Table (1) /= Myself) or
        (First_Person_Table (2) /= Daughter)
      then
         Report.Failed ("PROBLEMS WITH ITERATION - 2");
      end if;

   end Local_Block;

   Report.Result;

end Cc3019c2m;
