package P is
   procedure Q (X : Boolean; Y : String);
   procedure R (X : Boolean);
   function S (X : Boolean) return String;
end P;

procedure P.Main is
begin
   P.Q (True, "Hello");
end P.Main;
