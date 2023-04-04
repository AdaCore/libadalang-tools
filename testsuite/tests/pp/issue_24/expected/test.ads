with Ada_Package;

package Test is

   type Rec_1 is
     record
        Int_Data : Integer;
     end record;
   for Rec_1_Type use
      record
         Int_Data at 0 range 0 .. 31;
         pragma Warnings (Off);
      end record;

   type Rec_2 (Val : Integer := 0) is
     record
        Val : String (1 .. 10);
     end record;

   type Rec_3 (Val : Boolean := False) is
     record
        case Val is
           when False =>
              null;
           when True =>
              Data : Boolean;
        end case;
     end record;

   type REC_4 (NB : LARGE_CHARACTER_NB_TYPE := LARGE_CHARACTER_NB_TYPE'First)
   is
     record
        DATA : STRING (POSITIVE (CHARACTER_NB_TYPE'First) .. NB);
     end record;

   type REC_5 (FIELD : PROTOCOL_FIELD_ENUMERATED_TYPE := DATA_LENGTH) is
     record
        case FIELD is
           when DATA_LENGTH =>
              null;
           when PROTOCOL_TYPE =>
              VALUE : Integer;
        end case;
     end record;

end Test;
