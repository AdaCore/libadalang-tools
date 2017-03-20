     --==================================================================--

-- The instantiation of C3A2A02_0 should NOT result in any exceptions.

with F3a2a00;
with C3a2a02_0;
pragma Elaborate (C3a2a02_0);
package C3a2a02_3 is new C3a2a02_0 (F3a2a00.Tagged_Type);
