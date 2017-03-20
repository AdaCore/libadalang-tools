
------------------------------------------------------------------- CXH1001

with Impdef.Annex_H;
with Report;
with CXH1001_0.CXH1001_1;
procedure CXH1001 is

  package Imp_H renames Impdef.Annex_H;
  use type CXH1001_0.Num;

  My_Object : Imp_H.Scalar_To_Normalize;  -- not initialized

  Value     : CXH1001_0.Num := CXH1001_0.STN_2_Num ( My_Object );
                               -- My_Object is not initialized

  Parameter_Value : Imp_H.Scalar_To_Normalize
                  := Imp_H.Scalar_To_Normalize'Last;

  type Structure is record  -- not initialized
    Std_Int : Integer;
    Scalar  : Imp_H.Scalar_To_Normalize;
    Num     : CXH1001_0.Num;
  end record;

  S : Structure;  -- not initialized

  procedure Bad_Code( Unassigned : out Imp_H.Scalar_To_Normalize ) is
    -- returns uninitialized OUT parameter
  begin

    if Report.Ident_Int( 0 ) = 1 then
      Report.Failed( "Nothing is something" );
      Unassigned := Imp_H.Scalar_To_Normalize'First;
    end if;

  end Bad_Code; 

  procedure Check( V : CXH1001_0.Num; Message : String ) is
  begin


    if Imp_H.Default_For_Scalar_To_Normalize_Is_In_Range then
      if V /= Imp_H.Scalar_To_Normalize'Pos(
                                  Imp_H.Default_For_Scalar_To_Normalize) then
        Report.Failed(Message & ": Implicit initial value for object "
                       & "is not the predicted value"); 
      end if;
    elsif V'Valid and then V in
      0 .. Imp_H.Scalar_To_Normalize'Pos(Imp_H.Scalar_To_Normalize'Last) then
      Report.Failed(Message & ": Implicit initial value for object "
                     & "is a value of the type"); 
    end if;

  end Check;

begin  -- Main test procedure.

  Report.Test ("CXH1001", "Check that the configuration pragma " &
                          "Normalize_Scalars causes uninitialized scalar " &
                          "objects to be set to a predictable value. " &
                          "Check that multiple compilation units are " &
                          "affected.  Check for uninitialized scalar " &
                          "objects that are subcomponents of composite " &
                          "objects, unassigned out parameters, have been " &
                          "allocated without an initial value, and are " &
                          "stand alone." );
   
  CXH1001_0.Package_Check;

  if My_Object'Valid then
    Value := CXH1001_0.STN_2_Num ( My_Object ); -- My_Object not initialized
  end if;
  -- otherwise, we just leave Value uninitialized

  Check( Value, "main procedure variable" );

  Bad_Code( Parameter_Value );

  if Parameter_Value'Valid then
    Check( CXH1001_0.STN_2_Num ( Parameter_Value ), "Out parameter return" );
  end if;

  if S.Scalar'Valid then
    Check( CXH1001_0.STN_2_Num ( S.Scalar ), "Record component" );
  end if;

  CXH1001_0.CXH1001_1.Thingy.Check_Embedded_Values;

  Report.Result;

end CXH1001;
