pragma Warnings (Off);
with ASIS_UL.Debug; use ASIS_UL.Debug;
with Text_IO;
pragma Warnings (On);

package LAL_UL is

   type Opt_Ada_Version_Type is
     (Ada_83, Ada_95, Ada_2005, Ada_2012, No_Ada_Version);
   pragma Ordered (Opt_Ada_Version_Type);
   subtype Ada_Version_Type is Opt_Ada_Version_Type range
     Ada_83 .. Opt_Ada_Version_Type'Pred (No_Ada_Version);
   Ada_Version : Ada_Version_Type := Ada_Version_Type'Last;

end LAL_UL;
