with Report;
with Tctouch;
package body C761005_Support is
   type Pick_Rotation is mod 52;
   type Pick_String is array (Pick_Rotation) of Character;

   From : constant Pick_String :=
     "ABCDEFGHIJKLMNOPQRSTUVWXYZ" & "abcdefghijklmnopqrstuvwxyz";
   Recent_Pick : Pick_Rotation := Pick_Rotation'Last;

   function Pick_Char return Character is
   begin
      Recent_Pick := Recent_Pick + 1;
      return From (Recent_Pick);
   end Pick_Char;

   function Invert (S : String) return String is
      T  : String (1 .. S'Length);
      Ti : Positive := 1;
   begin
      for Si in reverse S'Range loop
         T (Ti) := S (Si);
         Ti     := Ti + 1;
      end loop;
      return T;
   end Invert;

   procedure Validate (Initcount : Natural; Testnumber : Natural) is
      Number : constant String := Natural'Image (Testnumber);
   begin
      if Inits_Called /= Initcount then
         Report.Failed ("Wrong number of inits, Subtest " & Number);
      else
         Tctouch.Validate
           (Invert (Inits_Order (1 .. Inits_Called)), "Subtest " & Number,
            True);
      end if;
      Inits_Called := 0;
   end Validate;

end C761005_Support;
