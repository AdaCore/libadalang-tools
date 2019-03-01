with Libadalang;        use Libadalang;
with Libadalang.Common; use Libadalang.Common;

with Utils.Vectors;
package body METRICS.Line_Counting is

   subtype Token_Data_Type is Libadalang.Common.Token_Data_Type;

   procedure Inc (X : in out Metric_Nat; By : Metric_Nat := 1) is
   begin
      --  Use saturating arithmetic in case of really huge values.

      if X > Metric_Nat'Last - By then -- X + By > Metric_Nat'Last
         X := Metric_Nat'Last;
      else
         X := X + By;
      end if;
   end Inc;

   procedure Dec (X : in out Metric_Nat; By : Metric_Nat := 1) is
   begin
      X := X - By;
   end Dec;

   function Last (Counts : Cumulative_Counts_Array) return Slocs.Line_Number is
     (Slocs.Line_Number (Counts'Last - 1));
   --  We want to return the index of the last real line, not that of the
   --  sentinel at the end.

   function Line (TD : Token_Data_Type) return Line_Num is
     --  Return the line number on which the token appears

     (Line_Num (Sloc_Range (TD).Start_Line));

   procedure Validate (Prev, This : Cumulative_Counts);
   --  Check some required properties of subsequent elements, and
   --  raise an exception if we find something wrong.

   package Cumulative_Counts_Vectors is new Utils.Vectors (Line_Num,
      Cumulative_Counts, Cumulative_Counts_Array);

   function Get_Cumulative_Counts
     (Unit : Analysis_Unit) return Cumulative_Counts_Array
   is
      Result : Cumulative_Counts_Vectors.Vector;
      use Cumulative_Counts_Vectors;
      Prev_Line : Line_Num'Base   := 0;
      Cur       : Token_Reference := First_Token (Unit);
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
            TD       : constant Token_Data_Type := Data (Cur);
            K        : constant Token_Kind      := Kind (TD);
            Cur_Line : constant Line_Num        := Line (TD);
         begin
            if K = Ada_Whitespace then
               goto Skip_Whitespace;
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

               exit when K = Ada_Termination;

               if K = Ada_Comment then
                  Inc (Last_Ptr (Result) (Lines_Eol_Comment));
               end if;

            else
               pragma Assert (Cur_Line > Prev_Line);
               --  First token on this line

               --  Deal with blank lines, if any

               while Last_Index (Result) < Cur_Line loop
                  Append (Result, Last_Element (Result));
                  Inc (Last_Ptr (Result) (Lines_Blank));
               end loop;

               exit when K = Ada_Termination;

               --  Append an entry for the next line, and increment as
               --  appropriate.

               Append (Result, Last_Element (Result));

               if K = Ada_Comment then
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
         <<Skip_Whitespace>>
         Cur := Next (Cur);
      end loop;

      return
        R : constant Cumulative_Counts_Array :=
          Elems (Result) (1 .. Last_Index (Result)) do
         for Index in 2 .. R'Last loop
            declare
               Prev : Cumulative_Counts renames R (Index - 1);
               This : Cumulative_Counts renames R (Index);
            begin
               Validate (Prev, This);
            end;
         end loop;
      end return;
   end Get_Cumulative_Counts;

   function Line_Range_Count
     (Counts                : Cumulative_Counts_Array;
      First_Line, Last_Line : Slocs.Line_Number; Metric : Cumulative_Metrics)
      return Metric_Nat is
     (Counts (Line_Num (Last_Line) + 1) (Metric) -
      Counts (Line_Num (First_Line)) (Metric));

   procedure Validate (Prev, This : Cumulative_Counts) is
   begin
      --  The entry for each metric is either the same as the
      --  preceding one, or one more.

      for Metric in Cumulative_Metrics_Index loop
         pragma Assert (This (Metric) in Prev (Metric) | Prev (Metric) + 1);
      end loop;
   end Validate;

end METRICS.Line_Counting;
