-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
package body C760010_0.Check_Formal_Tagged is

   procedure Initialize (Ed : in out Embedded_Derived) is
   begin
      Ed.Tc_Meaningless_Value := Unique;
      case Action is
         when Init_Raise_User_Defined =>
            raise User_Defined_Exception;
         when Init_Raise_Standard =>
            raise Tasking_Error;
         when others =>
            null;
      end case;
   end Initialize;

   procedure Adjust (Ed : in out Embedded_Derived) is
   begin
      Ed.Tc_Meaningless_Value := Unique;
      case Action is
         when Adj_Raise_User_Defined =>
            raise User_Defined_Exception;
         when Adj_Raise_Standard =>
            raise Tasking_Error;
         when others =>
            null;
      end case;
   end Adjust;

   procedure Finalize (Ed : in out Embedded_Derived) is
   begin
      Ed.Tc_Meaningless_Value := Unique;
      case Action is
         when Fin_Raise_User_Defined =>
            raise User_Defined_Exception;
         when Fin_Raise_Standard =>
            raise Tasking_Error;
         when others =>
            null;
      end case;
   end Finalize;

end C760010_0.Check_Formal_Tagged;