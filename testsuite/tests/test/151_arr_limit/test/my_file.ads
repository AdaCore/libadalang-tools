package My_File is

   type Weekday is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);

   type My_Arr is array (Positive range <>, Weekday range <>) of Positive;

   procedure Foo (X : My_Arr) with Import;

end My_File;
