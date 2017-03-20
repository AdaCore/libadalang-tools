
with CXE2002_2.Gen;
procedure CXE2002_2.Inst is new CXE2002_2.Gen(Int => Integer);
    pragma Remote_Call_Interface(CXE2002_2.Inst);
