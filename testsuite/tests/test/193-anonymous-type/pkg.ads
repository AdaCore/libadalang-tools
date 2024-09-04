with Ada.Streams;

package Pkg is

   type Parent_Tagged_Record_Type is tagged record
      A : Integer;
      B : Boolean;
   end record;

   type Child_Tagged_Record_Type is new Parent_Tagged_Record_Type with record
      C : Character;
   end record;

   type Child_Tagged_Record_Type_Access is access all
     Child_Tagged_Record_Type;

   procedure Child_Tagged_Record_Type_Access_Output
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Item   : Child_Tagged_Record_Type_Access);

end Pkg;
