procedure Sample_Formatting is

begin

  My_Block:
    declare
      My_Var : Integer := 0;
    begin
      My_Var := 1;
    end My_Block;

  My_For_Loop:
    for I in 1..100 loop
      if I = 100 then
        null;
      end if;
    end loop My_For_Loop;

  My_While_Loop:
    while True loop
        null;
    end loop My_While_Loop;

  My_Plain_Loop:
    loop
       null;
       exit;
    end loop My_Plain_Loop;

  My_Begin_Block:
    begin
      null; null;
    end My_Begin_Block;

end Sample_Formatting;
