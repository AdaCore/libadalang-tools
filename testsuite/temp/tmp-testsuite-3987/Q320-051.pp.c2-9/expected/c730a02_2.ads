-- No body for C730A02_1.

     --==================================================================--

-- Library-level instantiation. Actual parameter is private extension.

with C730a02_0;  -- Extended book abstraction.
with F730a000;   -- Singly-linked list abstraction.
package C730a02_2 is new F730a000
  (Parent_Type => C730a02_0.Detailed_Book_Type);
