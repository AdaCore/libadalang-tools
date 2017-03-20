with Tctouch;
package body C393012_1 is
   function Display (T : Economy) return String is
   begin
      Tctouch.Touch
        ('E');  --------------------------------------------------- E
      return C393012_0.Display (C393012_0.Ticket (T));
   end Display;                -- conversion to abstract type

   function Service (T : Economy) return String is
   begin
      Tctouch.Touch
        ('e');  --------------------------------------------------- e
      return " K";
   end Service;

   function Display (T : First) return String is
   begin
      Tctouch.Touch
        ('F');  --------------------------------------------------- F
      return C393012_0.Display (C393012_0.Ticket (T));
   end Display;                -- conversion to abstract type

   function Service (T : First) return String is
   begin
      Tctouch.Touch
        ('f');  --------------------------------------------------- f
      return " F" & Meal_Designator'Image (T.Meal);
   end Service;

   procedure Set_Meal (T : in out First; To_Meal : Meal_Designator) is
   begin
      T.Meal := To_Meal;
   end Set_Meal;

end C393012_1;
