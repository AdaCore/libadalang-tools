with Test_Package;

procedure Main is
   procedure Dummy (I : Integer) is null;

begin
   Dummy (Test_Package.C1.Q.B.F.I);
   Dummy (Test_Package.C1.Q.B.F.I);
   Dummy (Test_Package.C2.Q.B.F.I);
end Main;
