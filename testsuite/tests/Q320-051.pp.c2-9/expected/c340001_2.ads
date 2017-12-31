package C340001_2 is

   type Media is (Paper, Electronic);

   type Transaction (Medium : Media) is tagged record
      Id : Natural range 1_000 .. 9_999;
   end record;

   function "=" (L, R : in Transaction) return Boolean;

   type Authorization (Kind : Media) is new Transaction (Medium => Kind) with
   record
      case Kind is
         when Paper =>
            Signature_On_File : Boolean;
         when Electronic =>
            Paper_Backup : Boolean; -- to retain opposing value
      end case;
   end record;

end C340001_2;
