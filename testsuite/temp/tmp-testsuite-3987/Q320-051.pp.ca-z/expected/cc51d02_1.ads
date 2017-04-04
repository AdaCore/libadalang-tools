     --==================================================================--

--                           --
-- Formal package used here. --
--                           --

with Fc51d00;    -- Generic list abstraction.
with Cc51d02_0;  -- Tagged type declarations.
generic          -- This procedure simulates a generic operation for types
   -- in the class rooted at Blind_ID_Type.
   type Elem_Type (<>) is new Cc51d02_0.Blind_Id_Type with private;
   with package List_Mgr is new Fc51d00 (Elem_Type);
procedure Cc51d02_1 (L : in out List_Mgr.List_Type; E : in Elem_Type);
