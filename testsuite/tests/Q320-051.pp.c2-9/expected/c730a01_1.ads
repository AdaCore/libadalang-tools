     --==================================================================--

     --==================================================================--

-- Library-level instantiation. Actual parameter is tagged record.

with F730a001;  -- Book definitions.
with F730a000;  -- Singly-linked list abstraction.
package C730a01_1 is new F730a000 (Parent_Type => F730a001.Book_Type);
