------------------------------------------------------------------- CXH3001

with Report;
with Cxh3001_0;
procedure Cxh3001 is
begin
   Report.Test
     ("CXH3001",
      "Check pragma Reviewable as a configuration pragma");

   Block :
   declare
      A_Truth : Boolean;
      Message : String := Report.Ident_Str ("Bad value encountered");
   begin
      begin
         A_Truth :=
           Report.Ident_Bool (True) or A_Truth;  -- (8) not initialized
         if not A_Truth then
            Report.Comment ("True or Uninit = False");
            A_Truth := Report.Ident_Bool (True);
         else
            A_Truth := Report.Ident_Bool (True);
            -- We do this separately on each branch in order to insure that
            -- a clever optimizer can find out little about this value.
            -- Ident_Bool is supposed to be opaque to any optimizer.
         end if;
      exception
         when Constraint_Error | Program_Error =>
            -- Possible results of accessing an uninitialized object.
            A_Truth := Report.Ident_Bool (True);
      end;

      Cxh3001_0.Pt.Set (A_Truth);

      Cxh3001_0.Global_Variable := A_Truth;

      Cxh3001_0.Tt.Release;  -- (9) rendezvous with TT

      while Cxh3001_0.Tt'Callable loop
         delay 1.0; -- wait for TT to become non-callable
      end loop;

      if not Cxh3001_0.Pt.Enquire or
        not Cxh3001_0.Global_Variable or
        Cxh3001_0.Tt'Callable
      then
         Report.Failed (Message);
      end if;

   end Block;

   Report.Result;
end Cxh3001;
