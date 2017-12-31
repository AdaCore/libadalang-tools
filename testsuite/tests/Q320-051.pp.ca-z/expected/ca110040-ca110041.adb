--=================================================================--

   -- Child Package body Computer_System.Manager
   package body Ca110040.Ca110041 is

   function Account_Limit_Reached return Boolean is
   begin
      if Total_Authorized_Accounts = Maximum_System_Accounts then
         return (True);
      else
         return (False);
      end if;
   end Account_Limit_Reached;
   ---------------------------------------------------------------
   function Valid_Account (Acct : User_Account) return Boolean is
      Result : Boolean := False;
   begin
      if (Acct.User_Id /= System_Account.User_Id) and
        (Acct.User_Id /= Auditor_Account.User_Id) then
         Result := True;
      end if;
      return (Result);
   end Valid_Account;
   ---------------------------------------------------------------
   procedure Initialize_User_Account (Acct : out User_Account) is
   begin
      if Account_Limit_Reached then
         raise Account_Limit_Exceeded;
      else
         Acct.User_Id   := Next_Available_Id;
         Acct.Privilege := User;
      end if;
      if not Valid_Account (Acct) then
         raise Unauthorized_Account;
      end if;
   end Initialize_User_Account;

end Ca110040.Ca110041;          -- Child Package body Computer_System.Manager
