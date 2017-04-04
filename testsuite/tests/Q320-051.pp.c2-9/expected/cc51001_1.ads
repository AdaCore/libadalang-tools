-- No body for CC51001_0.

     --==================================================================--

with Cc51001_0;       -- Root type for message class.
package Cc51001_1 is  -- Extensions to message class.

   subtype Source_Length is Natural range 0 .. 10;

   type From_Msg_Type
     (Slen : Source_Length)
   is            -- Direct derivative
   new Cc51001_0.Msg_Type with
   record                    -- of root type
      From : String (1 .. Slen);                           -- (indefinite).
   end record;

   subtype Dest_Length is Natural range 0 .. 10;

   type To_From_Msg_Type
     (Dlen : Dest_Length)
   is           -- Indirect
   new From_Msg_Type (Slen => 10) with
   record            -- derivative of
      To : String (1 .. Dlen);                             -- root type
   end record;                                             -- (indefinite).

end Cc51001_1;
