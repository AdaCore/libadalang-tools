-- C94008C.ADA

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
-- CHECK THAT SELECT WITH TERMINATE ALTERNATIVE WORKS CORRECTLY WITH NESTED
-- TASKS.

-- THIS TEST CONTAINS RACE CONDITIONS AND USES A GENERIC INSTANCE THAT CONTAINS
-- TASKS.

-- JEAN-PIERRE ROSEN 24 FEBRUARY 1984 JRK 4/7/86 JBG 8/29/86 ELIMINATED SHARED
-- VARIABLES; ADDED GENERIC UNIT PWN 11/30/94 REMOVED PRAGMA PRIORITY INSTANCES
-- FOR ADA 9X.

with Report; use Report;
with System; use System;
procedure C94008c is

-- GENERIC UNIT FOR DOING UPDATES OF SHARED VARIABLES
   generic
      type Holder_Type is private;
      type Value_Type is private;
      Initial_Value : Holder_Type;
      with procedure Set (Holder :    out Holder_Type;
         Value                   : in     Holder_Type) is <>;
      with procedure Update (Holder : in out Holder_Type;
         Value                      : in     Value_Type) is <>;
   package Shared is
      procedure Set (Value : in Holder_Type);
      procedure Update (Value : in Value_Type);
      function Get return Holder_Type;
   end Shared;

   package body Shared is
      task Share is
         entry Set (Value : in Holder_Type);
         entry Update (Value : in Value_Type);
         entry Read (Value : out Holder_Type);
      end Share;

      task body Share is
         Variable : Holder_Type;
      begin
         loop
            select
               accept Set (Value : in Holder_Type) do
                  Shared.Set (Variable, Value);
               end Set;
            or
               accept Update (Value : in Value_Type) do
                  Shared.Update (Variable, Value);
               end Update;
            or
               accept Read (Value : out Holder_Type) do
                  Value := Variable;
               end Read;
            or
               terminate;
            end select;
         end loop;
      end Share;

      procedure Set (Value : in Holder_Type) is
      begin
         Share.Set (Value);
      end Set;

      procedure Update (Value : in Value_Type) is
      begin
         Share.Update (Value);
      end Update;

      function Get return Holder_Type is
         Value : Holder_Type;
      begin
         Share.Read (Value);
         return Value;
      end Get;

   begin
      Share.Set (Initial_Value);    -- SET INITIAL VALUE
   end Shared;

   package Events is

      type Event_Type is record
         Trace  : String (1 .. 4) := "....";
         Length : Natural         := 0;
      end record;

      procedure Update (Var : in out Event_Type; Val : Character);
      procedure Set (Var : out Event_Type; Val : Event_Type);
   end Events;

   package Counter is
      procedure Update (Var : in out Integer; Val : Integer);
      procedure Set (Var : out Integer; Val : Integer);
   end Counter;

   package body Counter is
      procedure Update (Var : in out Integer; Val : Integer) is
      begin
         Var := Var + Val;
      end Update;

      procedure Set (Var : out Integer; Val : Integer) is
      begin
         Var := Val;
      end Set;
   end Counter;

   package body Events is
      procedure Update (Var : in out Event_Type; Val : Character) is
      begin
         Var.Length             := Var.Length + 1;
         Var.Trace (Var.Length) := Val;
      end Update;

      procedure Set (Var : out Event_Type; Val : Event_Type) is
      begin
         Var := Val;
      end Set;

   end Events;

   use Events, Counter;

   package Trace is new Shared (Event_Type, Character, ("....", 0));
   package Terminate_Count is new Shared (Integer, Integer, 0);

   function Enter_Terminate return Boolean is
   begin
      Terminate_Count.Update (1);
      return True;
   end Enter_Terminate;

begin -- C94008C

   Test
     ("C94008C",
      "CHECK CORRECT OPERATION OF SELECT WITH " & "TERMINATE ALTERNATIVE");

   declare

      procedure Event (Var : Character) renames Trace.Update;

      task T1 is
         entry E1;
      end T1;

      task body T1 is

         task T2 is
            entry E2;
         end T2;

         task body T2 is

            task T3 is
               entry E3;
            end T3;

            task body T3 is
            begin
               select
                  accept E3;
               or when Enter_Terminate =>
                  terminate;
               end select;
               Event ('D');
            end T3;

         begin -- T2

            select
               accept E2;
            or when Enter_Terminate =>
               terminate;
            end select;

            delay 10.0;

            if Terminate_Count.Get /= 1 then
               delay 20.0;
            end if;

            if Terminate_Count.Get /= 1 then
               Failed ("30 SECOND DELAY NOT ENOUGH - 1 ");
            end if;

            Event ('C');
            T1.E1;
            T3.E3;
         end T2;

      begin -- T1;

         select
            accept E1;
         or when Enter_Terminate =>
            terminate;
         end select;

         Event ('B');
         Terminate_Count.Set (0);
         T2.E2;

         select
            accept E1;
         or when Enter_Terminate =>
            terminate;
         end select;

         select
            accept E1;
         or
            terminate;  -- ONLY THIS ONE EVER CHOSEN.
         end select;

         Failed ("TERMINATE NOT SELECTED IN T1");
      end T1;

   begin

      delay 10.0; -- WAIT FOR T1, T2, AND T3 TO GET TO SELECT STMTS.

      if Terminate_Count.Get /= 3 then
         delay 20.0;
      end if;

      if Terminate_Count.Get /= 3 then
         Failed ("30 SECOND DELAY NOT ENOUGH - 2");
      end if;

      Event ('A');
      T1.E1;

   exception
      when others =>
         Failed ("EXCEPTION IN MAIN BLOCK");
   end;

   if Trace.Get.Trace /= "ABCD" then
      Failed ("INCORRECT ORDER OF EVENTS: " & Trace.Get.Trace);
   end if;

   Result;
end C94008c;
