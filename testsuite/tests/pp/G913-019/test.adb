procedure TEST is
  procedure PROCEDURE_NAME (PARAM1 : in INTEGER;
                            PARAM2 : in INTEGER;
                            PARAM3 : in INTEGER);
  procedure PROCEDURE_NAME (PARAM1 : in INTEGER;
                            PARAM2 : in INTEGER;
                            PARAM3 : in INTEGER) is
  begin
    null;
  end;
  type T is
     record
       FIELD1 : INTEGER;
       FIELD2 : INTEGER;
       FIELD3 : INTEGER;
     end record;
  Variable : T := (FIELD1 => 1,
                   FIELD2 => 2,
                   FIELD3 => 3);
begin
  PROCEDURE_NAME (PARAM1 => 1,
                  PARAM2 => 2,
                  PARAM3 => 3);
  VARIABLE := (FIELD1 => 1,
               FIELD2 => 2,
               FIELD3 => 3);
end TEST;
