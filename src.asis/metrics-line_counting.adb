--  with Interfaces; use Interfaces;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang; use Libadalang;
with Libadalang.AST; use Libadalang.AST;
with Libadalang.Lexer; use Libadalang.Lexer;

with LAL_UL.Formatted_Output; use LAL_UL.Formatted_Output;

with ASIS_UL.Vectors;
package body METRICS.Line_Counting is

   procedure Inc (X : in out Metric_Nat; By : Metric_Nat := 1) is
   begin
      X := X + By;
   end Inc;

   procedure Dec (X : in out Metric_Nat; By : Metric_Nat := 1) is
   begin
      X := X - By;
   end Dec;

   function Line (TD : Token_Data_Type) return Line_Num is
   --  Return the line number on which the token appears
     (Line_Num (TD.Sloc_Range.Start_Line));

   procedure Validate (Prev, This : Cumulative_Counts);
   --  Check some required properties of subsequent elements, and
   --  raise an exception if we find something wrong.

   package Cumulative_Counts_Vectors is new ASIS_UL.Vectors
     (Line_Num, Cumulative_Counts, Cumulative_Counts_Array);

   function Cumulative (Unit : Analysis_Unit) return Cumulative_Counts_Array is
      Result : Cumulative_Counts_Vectors.Vector;
      use Cumulative_Counts_Vectors;
      Prev_Line : Line_Num'Base := 0;
      Cur : Token_Type := First_Token (Unit);
   begin
      Append (Result, Cumulative_Counts'(others => 0));

      --  Go through the tokens one by one. If the current token is on
      --  a different line from the previous one, we know we're
      --  starting a new line. If the new line is more than 1 higher,
      --  then there must be some blank lines in between, so we append
      --  entries for the blank lines. Then we append a new entry for
      --  the line after this one, and increment counts appropriately.
      --
      --  If the current token is on the same line as the previous
      --  one, we need only take care of end-of-line comments.

      loop
         declare
            TD : constant Token_Data_Type := Data (Cur);
            Cur_Line : constant Line_Num := Line (TD);
         begin
            exit when TD.Kind = Ada_Termination;

            if False then
               Put ("\1\2\n", Token_Kind_Name (TD.Kind),
                    (if TD.Text = null
                       then ""
                       else " " & Image (TD.Text.all, With_Quotes => True)));
            end if;

            if Cur_Line = Prev_Line then
               declare
                  Last : Cumulative_Counts renames Last_Ptr (Result).all;
                  Prev : Cumulative_Counts renames
                    Result (Last_Index (Result) - 1);
               begin
                  pragma Assert (Last (Lines_Code) = Prev (Lines_Code) + 1);
                  --  We must have seen a previous non-comment token on
                  --  this line.
               end;

               if TD.Kind = Ada_Comment then
                  Inc (Last_Ptr (Result) (Lines_Eol_Comment));
               end if;

            else
               --  First token on this line

               --  Deal with blank lines, if any

               while Last_Index (Result) < Cur_Line loop
                  Append (Result, Last_Element (Result));
                  Inc (Last_Ptr (Result) (Lines_Blank));
               end loop;

               --  Append an entry for the next line, and increment as
               --  appropriate.

               Append (Result, Last_Element (Result));

               if TD.Kind = Ada_Comment then
                  Inc (Last_Ptr (Result) (Lines_Comment));
               else
                  Inc (Last_Ptr (Result) (Lines_Code));
               end if;
            end if;

            pragma Assert (Last_Index (Result) = Cur_Line + 1);
            --  When processing the tokens on line N, we're operating
            --  on the N+1 element, because the counts are for lines
            --  PRECEDING a given line.

            Prev_Line := Cur_Line;
         end;

         Validate (Result (Last_Index (Result) - 1), Last_Element (Result));
         Cur := Next (Cur);
      end loop;

      return R : constant Cumulative_Counts_Array :=
        Elems (Result) (1 .. Last_Index (Result))
      do
         for Index in 2 .. R'Last loop
            declare
               Prev : Cumulative_Counts renames R (Index - 1);
               This : Cumulative_Counts renames R (Index);
            begin
               Validate (Prev, This);
            end;
         end loop;
      end return;
   end Cumulative;

   function Line_Range_Count
     (Counts : Cumulative_Counts_Array;
      First_Line, Last_Line : Unsigned_32;
      Metric : Cumulative_Metrics) return Metric_Nat
   is
     (Counts (Line_Num (Last_Line) + 1) (Metric) -
      Counts (Line_Num (First_Line)) (Metric));

   procedure Validate (Prev, This : Cumulative_Counts) is
   begin
      --  The entry for each metric is either the same as the
      --  preceding one, or one more.

      for Metric in Cumulative_Metrics_Index loop
         pragma Assert
           (This (Metric) in Prev (Metric) | Prev (Metric) + 1);
      end loop;
   end Validate;

end METRICS.Line_Counting;
