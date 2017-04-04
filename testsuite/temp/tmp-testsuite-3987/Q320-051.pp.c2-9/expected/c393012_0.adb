with Tctouch;
package body C393012_0 is
   function Display (T : Ticket) return String is
   begin
      Tctouch.Touch
        ('T');  --------------------------------------------------- T
      return "Fl:" &
        Natural'Image (T.Flight) &
        Service (Ticket'Class (T)) &
        " Seat:" &
        Row_Number'Image (T.Row) &
        T.Seat;
   end Display;
end C393012_0;
