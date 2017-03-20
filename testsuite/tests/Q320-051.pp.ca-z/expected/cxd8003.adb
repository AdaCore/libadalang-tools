-- CXD8003.A
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
--      Check that the Ada.Real_Time package operations Split and
--      Time_Of operations work properly.
--      Check that the clock does not jump backwards.
--
-- TEST DESCRIPTION:
--      The checks of Split and Time_Of check a variety of samples to
--      be sure that the correct value is returned.
--      To check that there are no backward clock jumps, the clock
--      is sampled as frequently as possible and compared to the previous
--      sample.
-- APPLICABILITY CRITERIA:
--      This test is only applicable to implementations supporting the
--      Real-Time Annex.
--
--
-- CHANGE HISTORY:
--     02 OCT 95   SAIC    Initial version
--
--!

with Report;
with Impdef;
with System;
with Ada.Real_Time;
procedure Cxd8003 is
   Verbose : constant Boolean := False;
   type Int is range 0 .. System.Max_Int;
   package Rt renames Ada.Real_Time;
   use Rt;  -- for all the operators

   procedure Print_Time (Note : String; T : Rt.Time) is
      Sc : Rt.Seconds_Count;
      Ts : Rt.Time_Span;
   begin
      Rt.Split (T, Sc, Ts);
      Report.Comment
        (Note &
         Rt.Seconds_Count'Image (Sc) &
         Duration'Image (Rt.To_Duration (Ts)) &
         " [" &
         Integer'Image (Ts / Rt.Time_Span_Unit) &
         "]");
   end Print_Time;

   procedure Check_Split is
      T                                     : Rt.Time;
      One_Second : constant Rt.Time_Span := Rt.Milliseconds (1_000);
      Almost_5_Seconds, Almost_A_Second, Ts : Rt.Time_Span;
      Sc                                    : Rt.Seconds_Count;
      Dif                                   : Integer;
   begin
      Ts := Rt.Milliseconds (Report.Ident_Int (1_000));  -- 1 second
      T  := Rt.Time_Of (1, Ts);  -- 2 seconds
      Rt.Split (T, Sc, Ts);
      if Sc /= 2 then
         Report.Failed ("SC expected 2 got" & Rt.Seconds_Count'Image (Sc));
      end if;
      if Ts /= Rt.Time_Span_Zero then
         Report.Failed
           ("TS not 0 got (in units)" &
            Integer'Image (Ts / Rt.Time_Span_Unit));
      end if;

      -- compute a time_span that has one less time unit than is found
      -- in a second.
      T := Rt.Time_Of (1, Rt.Time_Span_Zero) - Rt.Time_Span_Unit;
      -- Almost_A_Second is just below 1 second
      Rt.Split (T, Sc, Almost_A_Second);
      if Almost_A_Second >= One_Second then
         Report.Failed ("Almost_A_Second >= One_Second");
      end if;
      if Verbose then
         Report.Comment
           ("time units in a second" &
            Integer'Image (Almost_A_Second / Rt.Time_Span_Unit + 1));
      end if;
      if Sc /= 0 then
         Report.Failed ("SC expected 0 got" & Rt.Seconds_Count'Image (Sc));
      end if;
      Almost_5_Seconds := Almost_A_Second * 5;  -- just under 5 seconds
      Rt.Split (Rt.Time_Of (0, Almost_5_Seconds), Sc, Ts);
      if Sc /= 4 then
         Report.Failed ("SC expected 4 got" & Rt.Seconds_Count'Image (Sc));
      end if;
      -- TS should be 5 time_span_unit below one second
      Dif := (Rt.Milliseconds (1_000) - Ts) / Rt.Time_Span_Unit;
      if Dif /= 5 then
         Report.Failed
           ("expected 5 time units difference, got" & Integer'Image (Dif));
      end if;
   end Check_Split;

   procedure Check_Clock is
      -- determine how long the test is to run
      Time_To_Run    : Rt.Time_Span := To_Time_Span (2.0);
      Time_To_Finish : Rt.Time      := Rt.Clock + Time_To_Run;
      Tick_Cnt       : Int          := 0;
      Previous       : Rt.Time      := Rt.Clock;
      This           : Rt.Time;
   begin
      loop
         This := Rt.Clock;
         if This /= Previous then
            if This < Previous then
               Report.Failed ("clock jumped backwards");
               Print_Time ("Previous time ", Previous);
               Print_Time ("This time ", This);
            else  -- normal tick
               Tick_Cnt := Tick_Cnt + 1;
            end if;
            Previous := This;
            exit when This > Time_To_Finish;
         end if;
      end loop;
      if Verbose then
         Report.Comment
           ("clock ticks detected in 2 seconds:" & Int'Image (Tick_Cnt));
      end if;
   end Check_Clock;

begin
   Report.Test
     ("CXD8003",
      "Check the Clock, Split and Time_Of routines " &
      " of the Ada.Real_Time package");

   Check_Split;
   Check_Clock;

   Report.Result;
end Cxd8003;
