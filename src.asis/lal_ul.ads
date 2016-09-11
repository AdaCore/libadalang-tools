--  ????????????????
--  Pretty-print everything.

--  Diffs with old command-line parsing. All switches have a long form. Don't
--  disallow conflicting enums -- take the last. Package for common switches,
--  then for Formatting switches. Support File_Name_Args. Efficiency notes:
--  no free. Binary search. Error for switch not allowed in
--  project. Document errors. Maybe we need a global Cmd. Otherwise, we'll
--  have to pass it all
--    around, and calls to Arg will be verbose. Can we do both?

--  ???Are we allowed to change ASIS_UL? (YES) Or do customers rely on it? (NO)

--  For each common switch:
--   Change gnatmetric to use Arg
--  For each gnatmetric switch:
--   Most of above, plus consider making it common.

pragma Warnings (Off);
with ASIS_UL.Debug; use ASIS_UL.Debug;
with Text_IO; -- use Text_IO; -- ????
pragma Warnings (On);

package LAL_UL is

   type Opt_Ada_Version_Type is
     (Ada_83, Ada_95, Ada_2005, Ada_2012, No_Ada_Version);
   pragma Ordered (Opt_Ada_Version_Type);
   subtype Ada_Version_Type is Opt_Ada_Version_Type range
     Ada_83 .. Opt_Ada_Version_Type'Pred (No_Ada_Version);
   Ada_Version : Ada_Version_Type := Ada_Version_Type'Last;

end LAL_UL;
