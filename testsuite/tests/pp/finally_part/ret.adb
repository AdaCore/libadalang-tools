function F return Integer is
begin
return R : Integer := 0 do
Compute (R);
finally
Cleanup;
end return;
end F;
