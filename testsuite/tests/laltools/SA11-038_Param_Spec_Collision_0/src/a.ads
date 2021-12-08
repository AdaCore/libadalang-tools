package A is
   type Bar is (Bar1, Bar2, Bar3);
   procedure Foo (My_Bar : Bar);
   procedure Foo_Bar (My_Bar, My_Other_Bar : Bar; Yet_Another_Bar : Bar);
end A;
