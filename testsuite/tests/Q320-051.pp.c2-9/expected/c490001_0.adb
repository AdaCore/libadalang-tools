     --==================================================================--

with Tctouch;
package body C490001_0 is

   procedure Float_Subtest (A, B : in My_Flt; Msg : in String) is
   begin
      Tctouch.Assert (A = B, Msg);
   end Float_Subtest;

   procedure Float_Subtest (A, B, C : in My_Flt; Msg : in String) is
   begin
      Tctouch.Assert (A = B or A = C, Msg);
   end Float_Subtest;

end C490001_0;
