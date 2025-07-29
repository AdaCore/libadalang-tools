
package body TACHES is

   task body Semaphore_1 is
      Data : INTEGER ;
   begin
      loop
         select
            accept Ecrire (Donnee : in    INTEGER) do
               Data := Donnee ;
            end Ecrire ;

         or
            accept Lire (Donnee :    out INTEGER) do
               Donnee := Data ;
            end Lire ;
         else
            null;
         then abort
            null;
         end select ;
      end loop ;
   end Semaphore_1 ;

end TACHES;
