package body Pkg is
task body T is
begin
accept E do
Handle;
finally
Cleanup;
end E;
end T;
end Pkg;
