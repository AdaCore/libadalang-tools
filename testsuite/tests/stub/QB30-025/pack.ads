--  pack.ads

package Pack is
   procedure Proc (I : Integer; B : Boolean);

   package Inner is
      procedure P1 (I : Integer);
      function F1 (I : Integer) return Boolean;
   end Inner;

end Pack;
