pragma Ada_2012;
package body Pack is

   ----------
   -- Proc --
   ----------

   procedure Proc (I : Integer; B : Boolean) is separate;

   -----------
   -- Inner --
   -----------

   package body Inner is

      --------
      -- P1 --
      --------

      procedure P1 (I : Integer) is separate;

      --------
      -- F1 --
      --------

      function F1 (I : Integer) return Boolean is separate;

   end Inner;

end Pack;
