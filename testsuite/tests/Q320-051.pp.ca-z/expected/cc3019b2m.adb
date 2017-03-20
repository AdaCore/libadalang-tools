-- CC3019B2M.ADA

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
--  CHECK INSTANTIATIONS OF UNITS WITHIN GENERIC UNITS, E.G.,
--  TO SUPPORT ITERATORS. THIS TEST SPECIFICALLY CHECKS THAT A
--  NESTING LEVEL OF 2 IS SUPPORTED FOR GENERICS.
--
--  *** THIS IS THE MAIN PROGRAM. IT MUST BE COMPILED AFTER THE
--  *** SOURCE CODE IN FILES CC3019B0.ADA AND CC3019B1.ADA HAVE
--  *** BEEN COMPILED.
--
-- HISTORY:
--         EDWARD V. BERARD, 31 AUGUST 1990

with Report;
with Cc3019b1_Stack_Class;

procedure Cc3019b2m is

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

   procedure Assign
     (The_Value_Of_This_Date : in out Date;
      To_This_Date           : in out Date);

   function Is_Equal (Left : in Date; Right : in Date) return Boolean;

   package Date_Stack is new Cc3019b1_Stack_Class
     (Element => Date,
      Assign  => Assign,
      "="     => Is_Equal);

   First_Date_Stack  : Date_Stack.Stack;
   Second_Date_Stack : Date_Stack.Stack;
   Third_Date_Stack  : Date_Stack.Stack;

   function "="
     (Left  : in Date_Stack.Stack;
      Right : in Date_Stack.Stack) return Boolean renames
     Date_Stack."=";

   procedure Assign
     (The_Value_Of_This_Date : in out Date;
      To_This_Date           : in out Date)
   is

   begin -- ASSIGN

      To_This_Date := The_Value_Of_This_Date;

   end Assign;

   function Is_Equal (Left : in Date; Right : in Date) return Boolean is

   begin -- IS_EQUAL

      return (Left.Month = Right.Month) and
        (Left.Day = Right.Day) and
        (Left.Year = Right.Year);

   end Is_Equal;

begin  -- CC3019B2M

   Report.Test
     ("CC3019B2M",
      "CHECK INSTANTIATIONS OF UNITS WITHIN GENERIC " &
      "UNITS, E.G., TO SUPPORT ITERATORS. THIS TEST " &
      "SPECIFICALLY CHECKS THAT A NESTING LEVEL OF " &
      "2 IS SUPPORTED FOR GENERICS.");

   Date_Stack.Clear (This_Stack => First_Date_Stack);
   if Date_Stack.Number_Of_Elements (On_This_Stack => First_Date_Stack) /=
     0
   then
      Report.Failed ("IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 1");
   end if;

   Date_Stack.Push
     (This_Element     => Today,
      On_To_This_Stack => First_Date_Stack);
   if Date_Stack.Number_Of_Elements (On_This_Stack => First_Date_Stack) /=
     1
   then
      Report.Failed ("IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 2");
   end if;

   Date_Stack.Push
     (This_Element     => First_Date,
      On_To_This_Stack => First_Date_Stack);
   if Date_Stack.Number_Of_Elements (On_This_Stack => First_Date_Stack) /=
     2
   then
      Report.Failed ("IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 3");
   end if;

   Date_Stack.Push
     (This_Element     => Birth_Date,
      On_To_This_Stack => First_Date_Stack);
   if Date_Stack.Number_Of_Elements (On_This_Stack => First_Date_Stack) /=
     3
   then
      Report.Failed ("IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 4");
   end if;

   Date_Stack.Pop
     (This_Element   => Store_Date,
      Off_This_Stack => First_Date_Stack);
   if Date_Stack.Number_Of_Elements (On_This_Stack => First_Date_Stack) /=
     2
   then
      Report.Failed ("IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 5");
   end if;

   if Store_Date /= Birth_Date then
      Report.Failed ("IMPROPER VALUE REMOVED FROM STACK - 1");
   end if;

   Date_Stack.Clear (This_Stack => Second_Date_Stack);
   if Date_Stack.Number_Of_Elements (On_This_Stack => Second_Date_Stack) /=
     0
   then
      Report.Failed ("IMPROPER VALUE RETURNED FROM NUMBER_OF_ELEMENTS - 6");
   end if;

   Date_Stack.Copy
     (This_Stack    => First_Date_Stack,
      To_This_Stack => Second_Date_Stack);

   if First_Date_Stack /= Second_Date_Stack then
      Report.Failed ("PROBLEMS WITH COPY OR TEST FOR EQUALITY");
   end if;

   Date_Stack.Pop
     (This_Element   => Store_Date,
      Off_This_Stack => Second_Date_Stack);
   Date_Stack.Push
     (This_Element     => Wall_Date,
      On_To_This_Stack => Second_Date_Stack);
   if First_Date_Stack = Second_Date_Stack then
      Report.Failed ("PROBLEMS WITH POP OR TEST FOR EQUALITY");
   end if;

   Underflow_Exception_Test :
 begin  -- UNDERFLOW_EXCEPTION_TEST

      Date_Stack.Clear (This_Stack => Third_Date_Stack);
      Date_Stack.Pop
        (This_Element   => Store_Date,
         Off_This_Stack => Third_Date_Stack);
      Report.Failed ("UNDERFLOW EXCEPTION NOT RAISED");

   exception

      when Date_Stack.Underflow =>
         null;  -- CORRECT EXCEPTION
      -- RAISED
      when others =>
         Report.Failed
           ("INCORRECT EXCEPTION RAISED IN " & "UNDERFLOW EXCEPTION TEST");

   end Underflow_Exception_Test;

   Overflow_Exception_Test :
 begin  -- OVERFLOW_EXCEPTION_TEST

      Date_Stack.Clear (This_Stack => Third_Date_Stack);
      for Index in 1 .. 10 loop
         Date_Stack.Push
           (This_Element     => Today,
            On_To_This_Stack => Third_Date_Stack);
      end loop;

      Date_Stack.Push
        (This_Element     => Today,
         On_To_This_Stack => Third_Date_Stack);
      Report.Failed ("OVERFLOW EXCEPTION NOT RAISED");

   exception

      when Date_Stack.Overflow =>
         null;  -- CORRECT EXCEPTION
      -- RAISED
      when others =>
         Report.Failed
           ("INCORRECT EXCEPTION RAISED IN " & "OVERFLOW EXCEPTION TEST");

   end Overflow_Exception_Test;

   Local_Block :
 declare

      type Date_Table is array (Positive range 1 .. 10) of Date;

      First_Date_Table : Date_Table;

      Table_Index : Positive := 1;

      procedure Show_Dates (This_Date : in Date; Continue : out Boolean);

      procedure Store_Dates (This_Date : in Date; Continue : out Boolean);

      procedure Show_Date_Iterate is new Date_Stack.Iterate
        (Process => Show_Dates);

      procedure Store_Date_Iterate is new Date_Stack.Iterate
        (Process => Store_Dates);

      procedure Show_Dates (This_Date : in Date; Continue : out Boolean) is
      begin  -- SHOW_DATES

         Report.Comment ("THE MONTH IS " & Month_Type'Image (This_Date.Month));
         Report.Comment ("THE DAY IS " & Day_Type'Image (This_Date.Day));
         Report.Comment ("THE YEAR IS " & Year_Type'Image (This_Date.Year));

         Continue := True;

      end Show_Dates;

      procedure Store_Dates (This_Date : in Date; Continue : out Boolean) is
      begin  -- STORE_DATES

         First_Date_Table (Table_Index) := This_Date;
         Table_Index                    := Table_Index + 1;

         Continue := True;

      end Store_Dates;

   begin  -- LOCAL_BLOCK

      Report.Comment ("CONTENTS OF THE FIRST STACK");
      Show_Date_Iterate (Over_This_Stack => First_Date_Stack);

      Report.Comment ("CONTENTS OF THE SECOND STACK");
      Show_Date_Iterate (Over_This_Stack => Second_Date_Stack);

      Store_Date_Iterate (Over_This_Stack => First_Date_Stack);
      if (First_Date_Table (1) /= Today) or
        (First_Date_Table (2) /= First_Date)
      then
         Report.Failed ("PROBLEMS WITH ITERATION - 1");
      end if;

      Table_Index := 1;
      Store_Date_Iterate (Over_This_Stack => Second_Date_Stack);
      if (First_Date_Table (1) /= Today) or
        (First_Date_Table (2) /= Wall_Date)
      then
         Report.Failed ("PROBLEMS WITH ITERATION - 2");
      end if;

   end Local_Block;

   Report.Result;

end Cc3019b2m;
