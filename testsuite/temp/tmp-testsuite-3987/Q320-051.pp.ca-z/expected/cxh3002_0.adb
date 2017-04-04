-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- CXH3002_0

package body Cxh3002_0 is

   Global_Variable : Character := 'A';

   procedure Proc (R : Root) is
   begin
   -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---=====
      pragma Inspection_Point (Global_Variable);                     -- BDPQT
   -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---=====
      case R.Disc is
         when Item =>
            Global_Variable := 'I';
         when Stuff =>
            Global_Variable := 'S';
         when Things =>
            Global_Variable := 'T';
      end case;
   end Proc;

   function Func return A_Proc is
   begin
      -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---====
      pragma Inspection_Point;                                        -- APQT
      -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---====
      return Proc'Access;
   end Func;

   protected body Pt is
      entry Prot_Entry (Switch : Boolean) when True is
      begin
         Toggle := Switch;
      -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---====
         pragma Inspection_Point;                                    -- APVT
      -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---====
      end Prot_Entry;
   end Pt;

   task body Tt is
      List_Copy : A_List;
   begin
      -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---====
      pragma Inspection_Point;                                        -- APUT
      -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---====
      loop
      -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---====
         pragma Inspection_Point;                                      -- APUT
      -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---====
         select
            accept Task_Entry (Items : in A_List) do
               List_Copy := Items;
   -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---=====
               pragma Inspection_Point
                 (List_Copy);                     -- BFPUT
   -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---=====
            end Task_Entry;
      -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---====
            pragma Inspection_Point;                                    -- APUT
      -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---====
         or
            terminate;
         end select;
      end loop;
   end Tt;

   -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---=====
   pragma Inspection_Point;                                           -- ARTO
   -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- --- -- ---=====

end Cxh3002_0;
