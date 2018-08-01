package Records is

  type Uncompact_Type is
    record
      Field_1 : Boolean;
      Field_2 : Float;
    end record;

  for Uncompact_Type use
    record
      Field_1 at 123 range 456 .. 789;
    end record;

  type Status_Type is  -- IRS Table F-44
    -- another comment, aligned with 'record' here
    record
      -- yet another comment, aligned with record fields here
      Nav_Status  : Nav_Status_Type;
      -- still another comment, aligned with record fields here
      Test_Status : Test_Status_Type;
    end record;

end;
