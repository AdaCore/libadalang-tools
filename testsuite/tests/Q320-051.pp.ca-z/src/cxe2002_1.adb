
with Report; use Report;
procedure CXE2002_1(X: Integer) is
    -- Note: The separate specification is necessary, as Report is not an
    -- RCI unit.
begin
    if X /= 1 then
        Failed("Bad value passed to CXE2002_1:"
               & Integer'Image(X));
    end if;
end CXE2002_1;
