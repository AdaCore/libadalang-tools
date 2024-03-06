with P;
package Q is
    type T2 is new P.T with null record;

   function X2 (Self : T2) return Integer is (10 - Self.X);
end Q;
