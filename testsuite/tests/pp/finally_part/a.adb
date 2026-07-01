procedure A is
   procedure Finally_Only is
   begin
   Cleanup_Only;
   finally
   Release;
   end Finally_Only;

   procedure Exception_And_Finally is
   begin
   Work;
   exception
   when others => Log;
   finally
   Release_1;
   Release_2;
   end Exception_And_Finally;
begin
   declare
   X : Integer;
   begin
   Use_It (X);
   finally
   Free (X);
   end;
end A;
