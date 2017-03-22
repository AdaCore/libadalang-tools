
-----------------------------------------------------------------------------

generic
  Low : Integer;
  High : Integer;
package CXE4003_Generic_RCI is
  pragma Remote_Call_Interface;
  pragma Elaborate_Body (CXE4003_Generic_RCI);

  subtype Serial_Numbers is Integer range Low .. High;
  procedure Take_Call (X : Serial_Numbers);
  procedure Done;
end CXE4003_Generic_RCI;
