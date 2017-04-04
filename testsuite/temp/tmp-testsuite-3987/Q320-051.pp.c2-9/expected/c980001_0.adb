-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
with Tctouch;
package body C980001_0 is

   Tc_Master_Value : Integer := 0;

   function Tc_Unique return Integer is  -- make all values unique.
   begin
      Tc_Master_Value := Tc_Master_Value + 1;
      return Tc_Master_Value;
   end Tc_Unique;

   protected body Sticker is

      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;

      procedure Unlock is
      begin
         Locked := False;
      end Unlock;

      function Is_Locked return Boolean is
      begin
         return Locked;
      end Is_Locked;

   end Sticker;

   procedure Initialize (Av : in out Sticks_In_Initialize) is
   begin
      Tctouch.Touch
        ('I');  -------------------------------------------------- I
      Hold_Up.Unlock;               -- cause the select to abort
      Initialize_Called := True;
      Av.Item           := Tc_Unique;
      Tctouch.Touch
        ('i');  -------------------------------------------------- i
      Progress.Unlock;              -- allows Wait_Your_Turn to continue
   end Initialize;

   procedure Adjust (Av : in out Sticks_In_Adjust) is
   begin
      Tctouch.Touch
        ('A');  -------------------------------------------------- A
      Hold_Up.Unlock;               -- cause the select to abort
      Adjust_Called := True;
      Av.Item       := Tc_Unique;
      Tctouch.Touch
        ('a');  -------------------------------------------------- a
      Progress.Unlock;
   end Adjust;

   procedure Finalize (Av : in out Sticks_In_Finalize) is
   begin
      Tctouch.Touch
        ('F');  -------------------------------------------------- F
      Hold_Up.Unlock;               -- cause the select to abort
      Finalize_Called := True;
      Av.Item         := Tc_Unique;
      Tctouch.Touch
        ('f');  -------------------------------------------------- f
      Progress.Unlock;
   end Finalize;

   procedure Fail_And_Clear (Message : String) is
   begin
      Report.Failed (Message);
      Hold_Up.Unlock;
      Progress.Unlock;
   end Fail_And_Clear;

end C980001_0;
