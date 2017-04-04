package body Cc3019a_Queues is

   procedure Add (To_Q : in out Queue_Type; Value : Element_Type) is
   begin
      To_Q.Size                 := To_Q.Size + 1;
      To_Q.Contents (To_Q.Size) := Value;
   end Add;

--   GENERIC
--        WITH PROCEDURE APPLY (VAL : ELEMENT_TYPE);
   procedure Iterator (To_Q : Queue_Type) is
   begin
      for I in To_Q.Contents'First .. To_Q.Size loop
         Apply (To_Q.Contents (I));
      end loop;
   end Iterator;

end Cc3019a_Queues;
