--=================================================================--

package body Ca110040 is                     -- Package body Computer_System.

   function Next_Available_Id return Id_Type is
   begin
      Total_Accounts := Total_Accounts + 1;
      return (Id_Type (Total_Accounts));
   end Next_Available_Id;

end Ca110040;                                -- Package body Computer_System.
