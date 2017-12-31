-------------------------------------------------------------------------------
with Report;
with Impdef;
with System;
with Ada.Real_Time;
with Cxd8002_1;
procedure Cxd8002 is
   Verbose : constant Boolean := False;
   type Int is range 0 .. System.Max_Int;
   package Rt renames Ada.Real_Time;

   task type Delay_Test (Pri : System.Priority) is
      pragma Priority (Pri);
   end Delay_Test;

   procedure Do_Delay_Test (Verbose : Boolean; Pri : Integer) is
      Delay_Amount  : Rt.Time_Span := Rt.Time_Span_Unit;
      Start, Finish : Rt.Time;
      Iteration     : Int          := 1;
      use type Rt.Time_Span, Rt.Time;
      Max_Delay : Duration := Impdef.Switch_To_New_Task;
   begin    -- encapsulation
      -- we want our maximum delay to be at least a second
      if Max_Delay < 1.0 then
         Max_Delay := 1.0;
      end if;

      -- delay until test
      if Verbose then
         Report.Comment ("testing delay_until_statement");
      end if;

      loop
         Start := Rt.Clock;
         delay until Start + Delay_Amount;
         Finish := Rt.Clock;
         if Finish - Start < Delay_Amount then
            Report.Failed
              ("delay_until too short at iteration" & Int'Image (Iteration) &
               "  requested delay: " &
               Duration'Image (Rt.To_Duration (Delay_Amount)) &
               "  task priority:" & Integer'Image (Pri) & "  actual delay: " &
               Duration'Image (Rt.To_Duration (Finish - Start)));
         elsif Verbose then
            Report.Comment
              ("delay_until iteration" & Int'Image (Iteration) &
               "  requested delay: " &
               Duration'Image (Rt.To_Duration (Delay_Amount)) &
               "  task priority:" & Integer'Image (Pri) & "  actual delay: " &
               Duration'Image (Rt.To_Duration (Finish - Start)));
         end if;
         exit when Rt.To_Duration (Delay_Amount) > Max_Delay;
         Iteration    := Iteration + 1;
         Delay_Amount := Delay_Amount * 2;
      end loop;

      -- delay relative test
      if Verbose then
         Report.Comment ("testing delay_relative_statement");
      end if;

      Delay_Amount := Rt.To_Time_Span (Duration'Small);
      Iteration    := 1;
      loop
         Start := Rt.Clock;
         delay Rt.To_Duration (Delay_Amount);
         Finish := Rt.Clock;
         if Rt.To_Duration (Finish - Start) < Rt.To_Duration (Delay_Amount)
         then
            -- We must check this in type Duration, as the conversion to
            -- Duration can round down if Duration'Small > Time_Unit and
            -- Duration'Small isn't a multiple of Time_Unit. In that case,
            -- we could delay less than Delay_Amount.
            Report.Failed
              ("delay too short at iteration" & Int'Image (Iteration) &
               "  requested delay: " &
               Duration'Image (Rt.To_Duration (Delay_Amount)) &
               "  task priority:" & Integer'Image (Pri) & "  actual delay: " &
               Duration'Image (Rt.To_Duration (Finish - Start)));
         elsif Verbose then
            Report.Comment
              ("delay_relative iteration" & Int'Image (Iteration) &
               "  requested delay: " &
               Duration'Image (Rt.To_Duration (Delay_Amount)) &
               "  task priority:" & Integer'Image (Pri) & "  actual delay: " &
               Duration'Image (Rt.To_Duration (Finish - Start)));
         end if;
         exit when Rt.To_Duration (Delay_Amount) > Max_Delay;
         Iteration    := Iteration + 1;
         Delay_Amount := Delay_Amount * 2;
      end loop;
   end Do_Delay_Test;

   task body Delay_Test is
   begin
      -- Normally only the environment task runs the delay test in a verbose
      -- mode. This is to prevent the output from multiple tasks getting all
      -- jumbled.
      Do_Delay_Test (False, Pri);
   end Delay_Test;
begin

   Report.Test
     ("CXD8002", "Check the use of Real_Time.Clock" & " in delay statements");

   declare
      -- lots of tests going on in parallel and preempting tests
      Tm3 : Delay_Test (System.Default_Priority - 3);
      Tm2 : Delay_Test (System.Default_Priority - 2);
      Tm1 : Delay_Test (System.Default_Priority - 1);
      Tp1 : Delay_Test (System.Default_Priority + 1);
      Tp2 : Delay_Test (System.Default_Priority + 2);
      Tp3 : Delay_Test (System.Default_Priority + 3);
   begin
      -- this is the only one that is potentially verbose
      Do_Delay_Test (Verbose, System.Default_Priority);
   end;     -- encapsulation

   Cxd8002_1.Stop;
   Report.Result;
end Cxd8002;
