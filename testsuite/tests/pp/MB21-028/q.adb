package body Q is

   procedure P is
      pragma Warnings (Off);
      --  Explain why we turned warnings off.
   begin
      null;
   end P;

   procedure P is
      pragma Warnings (Off);
      --  Explain why we turned warnings off.

   begin
      null;
   end P;

   procedure P is
      pragma Warnings (Off);

      --  Start of processing for P.
   begin
      null;
   end P;

   procedure P is
      pragma Warnings (Off);

      --  Start of processing for P.

   begin
      null;
   end P;

end Q;
