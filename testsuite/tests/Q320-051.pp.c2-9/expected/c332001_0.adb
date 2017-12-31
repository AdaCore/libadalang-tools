-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

with Report;
package body C332001_0 is

   procedure Assert (Truth : Boolean; Message : String) is
   begin
      if not Truth then
         Report.Failed ("Assertion " & Message & " not true");
      end if;
   end Assert;

   procedure Check_Values is
   begin

      Assert
        (Nothing_New_Integer * Named_Integer = Named_Modular,
         "Nothing_New_Integer * Named_Integer = Named_Modular"); -- 1*2 = 2
      Assert
        (Nothing_New_Real * Named_Float = Named_Fixed,
         "Nothing_New_Real * Named_Float = Named_Fixed");-- 1.0*2.0 = 2.0

      Assert
        (Fn_Integer = Int2mod + Flt2int,
         "Fn_Integer = Int2Mod + Flt2Int");              -- 4 = 2+2
      Assert
        (Fn_Modular = Flt2int * 2,
         "Fn_Modular = Flt2Int * 2");                    -- 4 = 2*2
      Assert
        (Fn_Float = Mod2flt**Fix2mod,
         "Fn_Float   = Mod2Flt ** Fix2Mod");             -- 4.0 = 2.0**2
      Assert
        (Fn_Fixed = (-Mod2flt),
         "Fn_Fixed   = (- Mod2Flt)");                    -- -2.0 = (-2.0)

      Assert
        (Itf = Modular_Type'First,
         "ITF = Modular_Type'First");                    -- 0 = 0
      Assert
        (Mtl < Integer_Type'Last,
         "MTL < Integer_Type'Last");                     -- 255 < 1023
      Assert
        (Mtm < Integer_Type'Last,
         "MTM < Integer_Type'Last");                     -- 256 < 1023
      Assert
        (Enp > Mtp, "ENP > MTP");                                   -- 3 > 1
      Assert
        ((Fts < Mtl) or (Fts >= Mtl),  -- given FTS is impdef...
         "(FTS < MTL) or (FTS >= MTL)");                 -- True
      Assert
        (Fts > Its,
         "FTS > ITS");                                   -- impdef > 3

      Assert
        (Mafirst = Int_Array_Object'First,
         "MAFirst = Int_Array_Object'First");            -- 0 = 0
      Assert
        (Ialast > Mafirst,
         "IALast  > MAFirst");                           -- 1023 > 0
      Assert
        (Mal < Ial,
         "MAL     < IAL");                               -- 255 < 1024

      Assert
        (Mod2flt = Flt2fix,
         "Mod2Flt = Flt2Fix");                           -- 2.0 = 2.0

   end Check_Values;

end C332001_0;
