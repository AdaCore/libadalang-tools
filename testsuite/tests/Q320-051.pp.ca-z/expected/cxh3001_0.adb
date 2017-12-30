-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
package body Cxh3001_0 is

   procedure P (R : Root) is
      Warnable : Positive := 0; -- (7)                      -- OPTIONAL WARNING
      -- this would raise Constraint_Error if P were ever called, however this
      -- test never calls P.
   begin
      case R.Disc is
         when Item =>
            Report.Comment ("Got Item");
         when Stuff =>
            Report.Comment ("Got Stuff");
         when Things =>
            Report.Comment ("Got Things");
      end case;
      if Report.Ident_Int (Warnable) = 0 then
         Global_Variable := not Global_Variable; -- (8) known to be initialized
      end if;
   end P;

   function F return A_Proc is
   begin
      return P'Access;
   end F;

   protected body Pt is

      entry Set (Switch : Boolean) when True is
      begin
         Toggle := Switch;
      end Set;

      function Enquire return Boolean is
      begin
         return Toggle;
      end Enquire;

   end Pt;

   task body Tt is
   begin
      loop
         accept Release;
         exit when Global_Variable;
      end loop;
   end Tt;

   -- (9) TT activation
end Cxh3001_0;
