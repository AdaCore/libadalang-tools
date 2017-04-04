--==================================================================--

generic

   type Formal_Scalar is range <>;

   Fso : Formal_Scalar;

package C540001_2 is

   type Enum is (Alpha, Beta, Theta);

   procedure Assign_Enum (Et : out Enum);

end C540001_2;
