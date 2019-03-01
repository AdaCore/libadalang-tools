with Langkit_Support.Slocs; use Langkit_Support;
with Pp.Scanner;
package Pp.Error_Slocs is

   Error_Sloc : Slocs.Source_Location := Slocs.No_Source_Location;
   --  Global variable (!) that stores the current source location being
   --  processed by gnatpp. This is used in case gnatpp crashes; we print an
   --  error message containing the source location if possible, to give the
   --  user a hint as to which construct in their code caused the crash.
   --  This is set in the various passes. It is not guaranteed to be accurate.

   function To_Langkit
     (Sloc : Scanner.Source_Location) return Slocs.Source_Location is
     ((Slocs.Line_Number (Sloc.Line), Slocs.Column_Number (Sloc.Col)));
--  Convert Scanner.Source_Location to Langkit's version of
--  Source_Location.

end Pp.Error_Slocs;
