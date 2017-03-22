
----------------------------------------------------------------- CXH1001_1

with Unchecked_Conversion;
package CXH1001_0.CXH1001_1 is

  -- kill as many birds as possible with a single stone:
  --   embed a protected object in the body of a child package,
  -- checks the multiple compilation unit case,
  -- and part of the subcomponent case.

  protected Thingy is
    procedure Check_Embedded_Values;
  private
    Hidden_Object : Imp_H.Scalar_To_Normalize;  -- not initialized
    Hidden_Number : Imp_H.Small_Number;         -- not initialized
  end Thingy;

end CXH1001_0.CXH1001_1;
