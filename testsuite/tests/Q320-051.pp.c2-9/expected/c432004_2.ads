--==================================================================--

with C432004_0;
with C432004_1;
package C432004_2 is

   -- All types herein are record extensions, since aggregates cannot be given
   -- for private extensions

   type Sampletype_D is new C432004_1.Sampletype_B with record
      Sample_On_Loan : Boolean := False;
   end record;

   type Sampletype_E is new C432004_1.Sampletype_C with null record;

   type Sampletype_I is new C432004_1.Sampletype_G with record
      Sample_On_Loan : Boolean := True;
   end record;

   type Sampletype_J is new C432004_1.Sampletype_H with record
      Sample_On_Loan : Boolean := True;
   end record;

end C432004_2;
