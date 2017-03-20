-----------------------------------------------------------------------------

package Cxe5002_State is
--
-- This package contains state information that is communicated between the
-- main test procedure and the body of System.RPC.  The purpose of the state
-- information is to inform System.RPC as to what type of call is expected
-- and to let the main test procedure know if the expected call occurred.
   Apc_Expected         : Boolean := False;
   Remote_Call_Occurred : Boolean := False;

-- Let System.RPC know whether the call is intended for partition A
-- or partition B.
   Calls_To_Partition_B : Boolean := False;
end Cxe5002_State;
