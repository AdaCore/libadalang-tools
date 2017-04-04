-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Tctouch;
package body C760012_0 is

   package body Firsts is

      procedure Initialize (T : in out Constrained_First) is
      begin
         Tctouch.Touch
           ('C');   ----------------------------------------------- C
      end Initialize;

      procedure Finalize (T : in out Constrained_First) is
      begin
         Tctouch.Touch
           ('S');   ----------------------------------------------- S
      end Finalize;

      procedure Initialize (T : in out Simple_First) is
      begin
         T.My_Init_Seq_Number := 0;
         Tctouch.Touch
           ('A');   ----------------------------------------------- A
      end Initialize;

      procedure Finalize (T : in out Simple_First) is
      begin
         Tctouch.Touch
           ('T');   ----------------------------------------------- T
      end Finalize;

   end Firsts;

   procedure Initialize (T : in out Constrained_Second) is
   begin
      Tctouch.Touch
        ('D');   ------------------------------------------------- D
   end Initialize;

   procedure Finalize (T : in out Constrained_Second) is
   begin
      Tctouch.Touch
        ('R');   ------------------------------------------------- R
   end Finalize;

   procedure Initialize (T : in out Simple_Second) is
   begin
      T.My_Init_Seq_Number := 0;
      Tctouch.Touch
        ('A');   ------------------------------------------------- A
   end Initialize;

   procedure Finalize (T : in out Simple_Second) is
   begin
      Tctouch.Touch
        ('T');   ------------------------------------------------- T
   end Finalize;

   procedure Initialize (L : in out List_Item) is
   begin
      Tctouch.Touch
        ('F');   ------------------------------------------------- F
   end Initialize;

   procedure Finalize (L : in out List_Item) is
   begin
      Tctouch.Touch
        ('Q');   ------------------------------------------------- Q
   end Finalize;

end C760012_0;
