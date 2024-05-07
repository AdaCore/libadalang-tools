package Pkg is

   type A_String is new String (1 .. 10);

   type My_Stream is tagged limited null record;

   procedure Get
     (S : access My_Stream'Class; Res : out A_String);

end Pkg;
