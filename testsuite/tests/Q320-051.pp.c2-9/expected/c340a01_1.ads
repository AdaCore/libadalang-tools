     --==================================================================--

-- Library-level instantiation. Actual parameter is tagged record.

with F340a001;  -- Book definitions.
with F340a000;  -- Singly-linked list abstraction.
package C340a01_1 is new F340a000 (Parent_Type => F340a001.Book_Type);
