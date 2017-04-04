     --==================================================================--

-- Library-level instantiation. Actual parameter is record extension.

with C340a02_0;  -- Extended book abstraction.
with F340a000;   -- Singly-linked list abstraction.
package C340a02_2 is new F340a000
  (Parent_Type => C340a02_0.Detailed_Book_Type);
