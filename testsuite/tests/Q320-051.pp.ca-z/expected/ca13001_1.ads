-- No bodies required for CA13001_0.

     --==================================================================--

-- Public parent.

package Ca13001_1 is

   type Transportation is (Bicycle, Clunker, New_Car);
   type Key_Type is private;
   Walking : Boolean := False;

   -- Other type definitions and procedure declarations in real application.

private
   type Key_Type is
     range Transportation'Pos (Bicycle) .. Transportation'Pos (New_Car);

end Ca13001_1;
