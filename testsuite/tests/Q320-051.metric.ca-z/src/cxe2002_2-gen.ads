
generic
    type Int is new Integer;
procedure CXE2002_2.Gen(Y: out Int);
    pragma Remote_Call_Interface(CXE2002_2.Gen);
