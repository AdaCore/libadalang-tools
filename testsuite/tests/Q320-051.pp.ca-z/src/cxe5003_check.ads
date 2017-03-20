
-----------------------------------------------------------------------------

package CXE5003_Check is
--
-- This package contains state information that is communicated between the
-- library unit elaboration and the body of System.RPC.  The purpose of the
-- state information is to inform System.RPC as to what elaborations have
-- occurred.
  Part_A1   : Boolean := False;
  Part_A2   : Boolean := False;
  Normal    : Boolean := False;
  RPC       : Boolean := False;
  Main      : Boolean := False;
  Establish : Boolean := False;
end CXE5003_Check;
