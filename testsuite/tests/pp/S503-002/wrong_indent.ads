--  GNATpp alingnment issues with the following lines

package wrong_indent is

   type T_Bar is (A,
   -- Member A.

B,
   -- Member B.

C
   -- Member C.
   );
   -- Some enumeration.

   procedure Foo
     (S         : in Natural;
      Long_Name : in Natural);

   procedure Bar
     (S : in Natural;
      -- This argument has a comment
      Long_Name : in Natural
      -- This one too
      );
      -- This procedure is commented

end wrong_indent;
