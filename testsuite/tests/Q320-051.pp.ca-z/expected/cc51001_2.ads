-- No body for CC51001_1.

     --==================================================================--

with Cc51001_0;       -- Root type for message class.
generic               -- I/O operations for message class.
   type Message_Type (<>) is new Cc51001_0.Msg_Type with private;
package Cc51001_2 is

   -- This subprogram contains an artificial result for testing purposes:
   -- the function returns the text of the message to the caller as a string.

   function Print_Message (M : in Message_Type) return String;

   -- ... Other operations.

end Cc51001_2;
