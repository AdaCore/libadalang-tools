with System;      use System;
with Report;      use Report;
with C94005b_Pkg; use C94005b_Pkg;
with F;
procedure C94005b is

begin
   Test
     ("C94005B",
      "CHECK THAT IF A TASK TYPE IS DECLARED IN A " &
      "LIBRARY PACKAGE, ANY BLOCKS, SUBPROGRAMS, OR " &
      "TASKS THAT DECLARE OBJECTS OF THAT TYPE DO " &
      "WAIT FOR TERMINATION OF SUCH OBJECTS");

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (A)

      T : Tt;

   begin -- (A)

      T.E (Ident_Int (1));

   end; -- (A)

   if Global /= 1 then
      Failed ("DEPENDENT TASK NOT TERMINATED BEFORE " & "BLOCK EXIT - (A)");
   end if;

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (B)

      I : Integer;

   begin -- (B)

      I := F;

      if Global /= 2 then
         Failed
           ("DEPENDENT TASK NOT TERMINATED BEFORE " & "FUNCTION EXIT - (B)");
      end if;

   end; -- (B)

   --------------------------------------------------

   Global := Ident_Int (0);

   declare -- (C)

      task Tsk is
         entry Ent;
      end Tsk;

      task body Tsk is
         T : Tt;
      begin
         T.E (Ident_Int (3));
      end Tsk;

   begin -- (C)

      while not Tsk'Terminated loop
         delay 0.1;
      end loop;

      if Global /= 3 then
         Failed ("DEPENDENT TASK NOT TERMINATED BEFORE " & "TASK EXIT - (C)");
      end if;

   end; -- (C)

   --------------------------------------------------

   Result;
end C94005b;
