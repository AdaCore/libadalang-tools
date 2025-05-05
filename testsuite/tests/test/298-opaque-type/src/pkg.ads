with Ada.Real_Time;

package Pkg is

   type Opaque is new Ada.Real_Time.Time_Span;
   --  Ada.Real_Time.Time_Span is defined as a private type

   function Foo (X : Opaque) return Natural is (42);
end Pkg;
