-- No body for CC51004_0;

     --==================================================================--

with Cc51004_0;  -- Matrix types.
generic          -- Generic matrix "clear" operation.
   type Squares is new Cc51004_0.Sq_Type with private;       -- Indefinite
procedure Cc51004_1 (Sq : in out Squares);                   -- formal.
