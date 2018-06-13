with Tctouch;
package body F392c00_1 is

   function Create return Toggle is
   begin
      Tctouch.Touch ('1');  ------------------------------------------------ 1
      return Toggle'(On => True);
   end Create;

   function Create return Dimmer is
   begin
      Tctouch.Touch ('2');  ------------------------------------------------ 2
      return Dimmer'(On => True, Intensity => 75);
   end Create;

   function Create return Auto_Dimmer is
   begin
      Tctouch.Touch ('3');  ------------------------------------------------ 3
      return Auto_Dimmer'
          (On => True, Intensity => 25,
           Cutout_Threshold | Cutin_Threshold => 50, Auto_Engaged => True);
   end Create;

   procedure Flip (It : in out Toggle) is
   begin
      Tctouch.Touch ('A');  ------------------------------------------------ A
      It.On := not It.On;
   end Flip;

   function On (It : Toggle'Class) return Boolean is
   begin
      Tctouch.Touch ('B');  ------------------------------------------------ B
      return It.On;
   end On;

   function Off (It : Toggle'Class) return Boolean is
   begin
      Tctouch.Touch ('C');  ------------------------------------------------ C
      return not It.On;
   end Off;

   procedure Brighten (It : in out Dimmer; By : in Luminance := 10) is
   begin
      Tctouch.Touch ('D');  ------------------------------------------------ D
      if (It.Intensity + By) <= Luminance'Last then
         It.Intensity := It.Intensity + By;
      else
         It.Intensity := Luminance'Last;
      end if;
   end Brighten;

   procedure Dim (It : in out Dimmer; By : in Luminance := 10) is
   begin
      Tctouch.Touch ('E');  ------------------------------------------------ E
      if (It.Intensity - By) >= Luminance'First then
         It.Intensity := It.Intensity - By;
      else
         It.Intensity := Luminance'First;
      end if;
   end Dim;

   function Intensity (It : Dimmer) return Luminance is
   begin
      Tctouch.Touch ('F');  ------------------------------------------------ F
      if On (It) then
         return It.Intensity;
      else
         return Luminance'First;
      end if;
   end Intensity;

   procedure Flip (It : in out Dimmer) is
   begin
      Tctouch.Touch ('G');  ------------------------------------------------ G
      if On (It) and (It.Intensity < 50) then
         It.Intensity := Luminance'Last - It.Intensity;
      else
         Flip (Toggle (It));
      end if;
   end Flip;

   procedure Set_Auto (It : in out Auto_Dimmer) is
   begin
      Tctouch.Touch ('H');  ------------------------------------------------ H
      It.Auto_Engaged := True;
   end Set_Auto;

   procedure Clear_Auto (It : in out Auto_Dimmer) is
   begin
      Tctouch.Touch ('I');  ------------------------------------------------ I
      It.Auto_Engaged := False;
   end Clear_Auto;

   function Auto (It : Auto_Dimmer) return Boolean is
   begin
      Tctouch.Touch ('J');  ------------------------------------------------ J
      return It.Auto_Engaged;
   end Auto;

   procedure Flip (It : in out Auto_Dimmer) is
   begin
      Tctouch.Touch ('K');  ------------------------------------------------ K
      if It.Auto_Engaged then
         if Off (It) then
            Flip (Dimmer (It));
         else
            It.Auto_Engaged := False;
         end if;
      else
         Flip (Dimmer (It));
      end if;
   end Flip;

   procedure Set_Cutin (It : in out Auto_Dimmer; Lumens : in Luminance) is
   begin
      Tctouch.Touch ('L');  ------------------------------------------------ L
      It.Cutin_Threshold := Lumens;
   end Set_Cutin;

   procedure Set_Cutout (It : in out Auto_Dimmer; Lumens : in Luminance) is
   begin
      Tctouch.Touch ('M');  ------------------------------------------------ M
      It.Cutout_Threshold := Lumens;
   end Set_Cutout;

   function Cutout_Threshold (It : Auto_Dimmer) return Luminance is
   begin
      Tctouch.Touch ('N');  ------------------------------------------------ N
      return It.Cutout_Threshold;
   end Cutout_Threshold;

   function Cutin_Threshold (It : Auto_Dimmer) return Luminance is
   begin
      Tctouch.Touch ('O');  ------------------------------------------------ O
      return It.Cutin_Threshold;
   end Cutin_Threshold;

   function Tc_Cw_Ti (Key : Character) return Toggle'Class is
   begin
      Tctouch.Touch ('W');  ------------------------------------------------ W
      case Key is
         when 'T' | 't' =>
            return Toggle'(On => True);
         when 'D' | 'd' =>
            return Dimmer'(On => True, Intensity => 75);
         when 'A' | 'a' =>
            return Auto_Dimmer'
                (On => True, Intensity => 25,
                 Cutout_Threshold | Cutin_Threshold => 50,
                 Auto_Engaged => True);
         when others =>
            null;
      end case;
   end Tc_Cw_Ti;

   function Tc_Non_Disp (It : Toggle) return Boolean is
   begin
      Tctouch.Touch ('X');  ------------------------------------------------ X
      return It.On;
   end Tc_Non_Disp;

   function Tc_Non_Disp (It : Dimmer) return Boolean is
   begin
      Tctouch.Touch ('Y');  ------------------------------------------------ Y
      return It.On;
   end Tc_Non_Disp;

   function Tc_Non_Disp (It : Auto_Dimmer) return Boolean is
   begin
      Tctouch.Touch ('Z');  ------------------------------------------------ Z
      return It.On;
   end Tc_Non_Disp;

end F392c00_1;