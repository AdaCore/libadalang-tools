-- No body for CC51003_0;

     --==================================================================--

with Cc51003_0;  -- Matrix types.
generic          -- Generic double-matrix "clear" operation.
   type Dbl_Square is new Cc51003_0.Double_Square;            -- Indefinite
procedure Cc51003_1 (Dbl : in out Dbl_Square);                -- formal.
