----------------------------------------------------------------------------

with Tctouch;
package body C3a2001_4 is

   function Construct
     (Voltage : C3a2001_2.Voltages; Amperage : C3a2001_2.Amps)
      return Special_Breaker
   is
      It : Special_Breaker;
      procedure Set_Root (It : in out C3a2001_2.Basic_Breaker) is
      begin
         It := C3a2001_2.Construct (Voltage, Amperage);
      end Set_Root;
   begin
      Tctouch.Touch ('i'); --------------------------------------------- i
      Set_Root (C3a2001_2.Basic_Breaker (It));
      Set_Root (It.Backup);
      return It;
   end Construct;

   function Status_Of (It : C3a2001_1.Breaker) return C3a2001_1.Status renames
     C3a2001_1.Status_Of;

   procedure Flip (The_Breaker : in out Special_Breaker) is
   begin
      Tctouch.Touch ('j'); --------------------------------------------- j
      case Status_Of (C3a2001_1.Breaker (The_Breaker)) is
         when C3a2001_1.Power_Off | C3a2001_1.Power_On =>
            C3a2001_2.Flip (C3a2001_2.Basic_Breaker (The_Breaker));
         when others =>
            C3a2001_2.Flip (The_Breaker.Backup);
      end case;
   end Flip;

   procedure Trip (The_Breaker : in out Special_Breaker) is
   begin
      Tctouch.Touch ('k'); --------------------------------------------- k
      case Status_Of (C3a2001_1.Breaker (The_Breaker)) is
         when C3a2001_1.Power_Off =>
            null;
         when C3a2001_1.Power_On =>
            C3a2001_2.Reset (The_Breaker.Backup);
            C3a2001_2.Trip (C3a2001_2.Basic_Breaker (The_Breaker));
         when others =>
            C3a2001_2.Trip (The_Breaker.Backup);
      end case;
   end Trip;

   procedure Reset (The_Breaker : in out Special_Breaker) is
   begin
      Tctouch.Touch ('l'); --------------------------------------------- l
      case Status_Of (C3a2001_1.Breaker (The_Breaker)) is
         when C3a2001_1.Tripped =>
            C3a2001_2.Reset (C3a2001_2.Basic_Breaker (The_Breaker));
         when C3a2001_1.Failed =>
            C3a2001_2.Reset (The_Breaker.Backup);
         when C3a2001_1.Power_On | C3a2001_1.Power_Off =>
            null;
      end case;
   end Reset;

   procedure Fail (The_Breaker : in out Special_Breaker) is
   begin
      Tctouch.Touch ('m'); --------------------------------------------- m
      case Status_Of (C3a2001_1.Breaker (The_Breaker)) is
         when C3a2001_1.Failed =>
            C3a2001_2.Fail (The_Breaker.Backup);
         when others =>
            C3a2001_2.Fail (C3a2001_2.Basic_Breaker (The_Breaker));
            C3a2001_2.Reset (The_Breaker.Backup);
      end case;
   end Fail;

   function Status_Of (The_Breaker : Special_Breaker) return C3a2001_1.Status
   is
   begin
      Tctouch.Touch ('n'); --------------------------------------------- n
      case Status_Of (C3a2001_1.Breaker (The_Breaker)) is
         when C3a2001_1.Power_On =>
            return C3a2001_1.Power_On;
         when C3a2001_1.Power_Off =>
            return C3a2001_1.Power_Off;
         when others =>
            return C3a2001_2.Status_Of (The_Breaker.Backup);
      end case;
   end Status_Of;

   function On_Backup (The_Breaker : Special_Breaker) return Boolean is
      use C3a2001_2;
      use type C3a2001_1.Status;
   begin
      return Status_Of (Basic_Breaker (The_Breaker)) = C3a2001_1.Tripped or
        Status_Of (Basic_Breaker (The_Breaker)) = C3a2001_1.Failed;
   end On_Backup;

end C3a2001_4;
