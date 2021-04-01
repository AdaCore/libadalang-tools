------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
-- Libadalang Tools  is free software; you can redistribute it and/or modi- --
-- fy  it  under  terms of the  GNU General Public License  as published by --
-- the Free Software Foundation;  either version 3, or (at your option) any --
-- later version. This software  is distributed in the hope that it will be --
-- useful but  WITHOUT  ANY  WARRANTY; without even the implied warranty of --
-- MERCHANTABILITY  or  FITNESS  FOR A PARTICULAR PURPOSE.                  --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Langkit_Support.Slocs; use Langkit_Support;
with Libadalang.Analysis; use Libadalang.Analysis;
with METRICS.Command_Lines; use METRICS.Command_Lines;
package METRICS.Line_Counting is

   subtype Metric_Nat is Natural range 0 .. 1_000_000_000;

   procedure Inc (X : in out Metric_Nat; By : Metric_Nat := 1);
   procedure Dec (X : in out Metric_Nat; By : Metric_Nat := 1);

   --  Support for counting the number of lines of certain types
   --  (code lines, comment lines, etc).

   subtype Cumulative_Metrics is Lines_Metrics with
     Predicate => Cumulative_Metrics in
       Lines_Code | Lines_Comment | Lines_Eol_Comment | Lines_Blank;

   type Cumulative_Counts_Array (<>) is private;
   function Last (Counts : Cumulative_Counts_Array) return Slocs.Line_Number;

   function Get_Cumulative_Counts
     (Unit : Analysis_Unit) return Cumulative_Counts_Array;
   --  Precompute the data used by Line_Range_Count

   function Line_Range_Count
     (Counts : Cumulative_Counts_Array;
      First_Line, Last_Line : Slocs.Line_Number;
      Metric : Cumulative_Metrics) return Metric_Nat;
   --  Return the value of Metric for the range of lines given

private

   subtype Cumulative_Metrics_Index is
     Lines_Metrics range Lines_Code .. Lines_Blank;
   type Cumulative_Counts is array (Cumulative_Metrics_Index) of Metric_Nat;
   --  Mapping from Cumulative_Metrics to Metric_Nat. We'd like to use
   --  Cumulative_Metrics as the index, but Ada doesn't allow that,
   --  because it has a predicate. Unfortunately, Lines_Ratio is in
   --  the middle of those, and needs to be, so the metrics get
   --  printed in the right order. So we need to skip that one.

   type Line_Num is new Positive;
   --  We'd like to use Slocs.Line_Number, but that is a modular type
   type Cumulative_Counts_Array is
     array (Line_Num range <>) of Cumulative_Counts;

   --  For each line, and for each of the Cumulative_Metrics, we store
   --  the number of lines of that kind that precede the line. Thus,
   --  if C is the result of a call to Get_Cumulative_Counts, then:
   --
   --  C (N) (Lines_Code) = number of code lines preceding line N. A
   --  code line is one that contains at least one non-comment token.
   --
   --  C (N) (Lines_Comment) = number of whole-line comments preceding
   --  line N.
   --
   --  C (N) (Lines_Eol_Comment) = number of code lines that contain a
   --  comment, preceding line N.
   --
   --  C (N) (Lines_Blank) = number of blank lines preceding line N.
   --
   --  C (1) = (others => 0) always, because there are no lines
   --  preceding line 1. We pretend there is an extra line after the
   --  last line of the file, so C (Cumulative'Last) has the data for
   --  the whole file.
   --
   --  So for example, if we have these four source lines, with one
   --  blank line:

   --     --  A whole-line comment.
   --     package P is
   --
   --     end P; -- An end-of-line comment.

   --  C will have 'Last = 5, and will look like this:
   --
   --     (1 => (Lines_Code => 0,
   --            Lines_Comment => 0,
   --            Lines_Eol_Comment => 0,
   --            Lines_Blank => 0),
   --     (2 => (Lines_Code => 0,
   --            Lines_Comment => 1,
   --            Lines_Eol_Comment => 0,
   --            Lines_Blank => 0),
   --     (3 => (Lines_Code => 1,
   --            Lines_Comment => 1,
   --            Lines_Eol_Comment => 0,
   --            Lines_Blank => 0),
   --     (4 => (Lines_Code => 1,
   --            Lines_Comment => 1,
   --            Lines_Eol_Comment => 0,
   --            Lines_Blank => 1));
   --     (5 => (Lines_Code => 2,
   --            Lines_Comment => 1,
   --            Lines_Eol_Comment => 1,
   --            Lines_Blank => 1));
   --
   --  Note that line 4 counts as both Lines_Code and
   --  Lines_Eol_Comment, and these numbers are reflected in
   --  C (5).
   --
   --  The purpose of this is to allow us to easily compute the number
   --  of code lines (etc), between one line and another.
   --  For example, the number of code lines between line 100 and line
   --  200 (inclusive) is:
   --
   --    C (201) (Lines_Code) - C (100) (Lines_Code)
   --
   --  Note 201. That is, the number of code lines before line 201,
   --  minus the number before line 100.

end METRICS.Line_Counting;
