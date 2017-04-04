     --==================================================================--

with Cc51002_0;  -- Root message type and operations.
generic          -- Message class function.
   type Msg_Block is new Cc51002_0.Msg_Type with private;
function Cc51002_1 (M : in Msg_Block) return Boolean;
