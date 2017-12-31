-- Semaphores;

--=========================================================--

package body C940a03_1 is
   -- Semaphores is

   protected body Login_Semaphore_Type is

      entry Request_Login (Resource_Key : in out Login_Record_Type)
        when Logins_Avail > 0 is
      begin
         Next_Key := Next_Key + 1;        -- login process returns a key
         Resource_Key.Key := Next_Key;     -- to the requesting user
         Logins_Avail     := Logins_Avail - 1;
      end Request_Login;

      procedure Release_Login is
      begin
         Logins_Avail := Logins_Avail + 1;
      end Release_Login;

      function Available return Integer is
      begin
         return Logins_Avail;
      end Available;

   end Login_Semaphore_Type;

   function Tc_Key_Val (Login_Rec : Login_Record_Type) return Integer is
   begin
      return Integer (Login_Rec.Key);
   end Tc_Key_Val;

end C940a03_1;
