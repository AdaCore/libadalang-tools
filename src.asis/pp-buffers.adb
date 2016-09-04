------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                      G N A T 2 X M L . B U F F E R S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 2013-2016, AdaCore                    --
--                                                                          --
-- Gnat2xml is free software; you can redistribute it and/or modify it      --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software Foundation;  either version 2,  or  (at your option)  any later --
-- version. Gnat2xml is distributed  in the hope  that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MER-      --
-- CHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General  --
-- Public License for more details. You should have received a copy of the  --
-- GNU General Public License distributed with GNAT; see file COPYING. If   --
-- not, write to the Free Software Foundation, 59 Temple Place Suite 330,   --
-- Boston, MA 02111-1307, USA.                                              --
-- The gnat2xml tool was derived from the Avatox sources.                   --
------------------------------------------------------------------------------

pragma Ada_2012;

with GNAT.Decode_String;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Byte_Order_Mark;

package body Pp.Buffers is

   procedure Append_New_Marker (Buf : in out Buffer; Name : W_Char);
   --  Append a new Marker onto To_Markers, making it point to 'point'

   procedure Maybe_Move_Marker (Buf : in out Buffer);
   pragma Inline (Maybe_Move_Marker);
   --  If the first mark in From_Markers is at 'point', move it to the
   --  To_Markers array, adjusting its Position and Flag, to preserve
   --  the invariant.

   procedure Move_Marker (Buf : in out Buffer);
   --  Helper for Maybe_Move_Marker, out of line so we can inline
   --  Maybe_Move_Marker.

   procedure Maybe_Adjust_Marker (Buf : in out Buffer);
   --  See comment in Insert

   procedure Insert_Ada (Buf : in out Buffer; C : W_Char);
   --  Same as Insert_Any, except we don't need to call Maybe_Adjust_Marker

   function Valid_From (Buf : Buffer) return W_Str;
   --  Return the valid portion of Buf.From

   procedure Initialize_Buffer (Buf : in out Buffer);

   procedure Append_New_Marker (Buf : in out Buffer; Name : W_Char) is
   begin
      Append
        (Buf.Markers,
         Marker_Rec'
           (Position => Last_Index (Buf.To) + 1,
            Flag     => Buf.To_Flag,
            Name     => Name));
      Append (Buf.To_Markers, Last_Index (Buf.Markers));
   end Append_New_Marker;

   function At_End (Buf : Buffer) return Boolean is
   begin
      return Result : constant Boolean := Buf.Cur_Char = W_NUL do
         pragma Assert (Result = (Buf.From_First > Last_Index (Buf.From)));
      end return;
   end At_End;

   function At_Beginning (Buf : Buffer) return Boolean is
   begin
      return Is_Empty (Buf.To);
   end At_Beginning;

   function At_Point (Buf : Buffer; Mark : Marker) return Boolean is
      Rec : constant Marker_Rec := Buf.Markers (Mark);

   begin
      return Rec.Position = Last_Index (Buf.To) + 1
        and then Rec.Flag = Buf.To_Flag;
   end At_Point;

   function Char_At (Buf : Buffer; Mark : Marker) return W_Char is
      Rec    : constant Marker_Rec := Buf.Markers (Mark);
      Result : W_Char;

   begin
      if Rec.Flag = Buf.To_Flag then
         if Rec.Position = Last_Index (Buf.To) + 1 then
            Result := Buf.From (Buf.From_First);

         else
            Result := Buf.To (Rec.Position);
         end if;
      else
         Result := Buf.From (Rec.Position);
      end if;
      pragma Assert (Result = Char_At (Buf, Position (Buf, Mark)));
      return Result;
   end Char_At;

   function Char_At (Buf : Buffer; Position : Positive) return W_Char is
   begin
      if Position <= Last_Index (Buf.To) then
         return Buf.To (Position);
      else
         return Buf.From (Position - Last_Index (Buf.To) + Buf.From_First - 1);
      end if;
   end Char_At;

   procedure Clear (Buf : in out Buffer) is
   begin
      pragma Debug (Validate (Buf, "Clear"));

      Clear (Buf.From);
      Clear (Buf.To);
      Clear (Buf.Markers);
      Clear (Buf.From_Markers);
      Clear (Buf.To_Markers);
      Initialize_Buffer (Buf);
   end Clear;

   function Cur (Buf : Buffer) return W_Char is
   begin
      return Result : constant W_Char := Buf.Cur_Char do
         pragma Assert
           (if Buf.From_First > Last_Index (Buf.From) then Result = W_NUL
            else Result = Buf.From (Buf.From_First));
      end return;
   end Cur;

   function Cur_Column (Buf : Buffer) return Positive is
      pragma Assert (False); -- ???Not currently used.
   --  All adjustments of Cur_Column are commented out, except when inserting
   --  Ada source code.
   begin
      return Buf.Cur_Column;
   end Cur_Column;

   procedure Delete_Char (Buf : in out Buffer) is
      From_First : constant Positive := Buf.From_First + 1;
   begin
      Buf.From_First := From_First;
--         if C = NL then
--            Buf.Cur_Column := 1;
--         else
--            Buf.Cur_Column := Buf.Cur_Column + 1;
--         end if;
      Buf.Cur_Char :=
        (if From_First > Last_Index (Buf.From) then W_NUL
         else Buf.From (From_First));
      Maybe_Move_Marker (Buf);
   end Delete_Char;

   procedure Dump_Buf (Buf : Buffer) is
      use Dbg_Out;
   begin
      Put
        ("To[\1..\2] = <<\3>>end To[\1..\2]\n",
         "1",
         Image (Last_Index (Buf.To)),
         To_UTF8 (To_Array (Buf.To)));
      Put
        ("From[\1..\2] = <<\3>>end From[\1..\2]\n",
         Image (Buf.From_First),
         Image (Last_Index (Buf.From)),
         To_UTF8 (Valid_From (Buf)));

      if At_Beginning (Buf) then
         Put ("At_Beginning\n");
      end if;
      if At_End (Buf) then
         Put ("At_End\n");
      end if;
   end Dump_Buf;

   procedure Dump_Buffer (Buf : Buffer) is
      use Dbg_Out;
   begin
      if False then
         Put ("Buf = <<\1>>end Buf\n", To_String (Buf));
         Put ("Dbg = <<\1>>end Dbg\n", To_Debug_String (Buf));
      end if;

      Dump_Buf (Buf);

      Put ("To_Markers:\n");
      Indent;

      for X in 1 .. Last_Index (Buf.To_Markers) loop
         Dump_Marker (Buf, Buf.To_Markers (X));
      end loop;
      Outdent;
      Put ("end To_Markers\n");

      Put
        ("From_Markers[\1..\2]:\n",
         Image (Integer (Buf.From_Markers_First)),
         Image (Integer (Last_Index (Buf.From_Markers))));
      Indent;

      for X in Buf.From_Markers_First .. Last_Index (Buf.From_Markers) loop
         Dump_Marker (Buf, Buf.From_Markers (X));
      end loop;
      Outdent;
      Put ("end From_Markers\n");
   end Dump_Buffer;

   procedure Dump_Marker (Buf : Buffer; Mark : Marker) is
      use Dbg_Out;
      Rec : Marker_Rec renames Buf.Markers (Mark);
      Pos : constant Positive := Position (Buf, Mark);

   begin
      if Rec.Flag = Buf.To_Flag then
         Put ("to   ");

      else
         Put ("from ");
      end if;
      Put
        ("\1 --> \2 '\3'  \4\n",
         Image (Rec.Position),
         Image (Pos),
         To_UTF8 ((1 => Rec.Name)),
         Image (Modular (Mark)));

      Put ("<<");

      for P in Pos - 30 .. Pos + 30 loop
         if P = Pos then
            Put_Char ('[');
         end if;
         if P in 1 .. Last_Position (Buf) then
            if Char_At (Buf, P) = NL then
               Put_Char ('$');
            else
               Put ("\1", To_UTF8 ((1 => Char_At (Buf, P))));
            end if;
         end if;
         if P = Pos then
            Put_Char (']');
         end if;
      end loop;
      Put (">>\n");
   end Dump_Marker;

   function Mark_LT (Buf : Buffer; M, N : Marker) return Boolean is
   begin
      return Position (Buf, M) < Position (Buf, N);
   end Mark_LT;

   function Mark_LE (Buf : Buffer; M, N : Marker) return Boolean is
   begin
      return Position (Buf, M) = Position (Buf, N);
   end Mark_LE;

   procedure Move_Marker (Buf : in out Buffer) is
      Mark : constant Marker := Buf.From_Markers (Buf.From_Markers_First);
      Rec : Marker_Rec renames Buf.Markers (Mark);
   begin
      pragma Assert (Rec.Position >= Buf.From_First);
      if Rec.Position = Buf.From_First then
         Append (Buf.To_Markers, Mark);
         Rec :=
           (Flag     => not Rec.Flag,
            Position => Last_Index (Buf.To) + 1,
            Name     => Rec.Name);
         pragma Assert (Rec.Flag = Buf.To_Flag);
         Buf.From_Markers_First := Buf.From_Markers_First + 1;
         pragma Assert (At_Point (Buf, Mark));
      end if;
   end Move_Marker;

   procedure Maybe_Move_Marker (Buf : in out Buffer) is

      procedure Assert;
      --  Perform some consistency checks

      procedure Assert is
      begin
         if Buf.From_Markers_First <= Last_Index (Buf.From_Markers) then
            declare
               Mark : constant Marker :=
                 Buf.From_Markers (Buf.From_Markers_First);
               Rec : Marker_Rec renames Buf.Markers (Mark);

            begin
               pragma Assert
                 (Rec.Position in
                    Buf.From_First + 1 .. Last_Index (Buf.From) + 1);
            end;

         else
            pragma Assert
              (Buf.From_Markers_First = Last_Index (Buf.From_Markers) + 1);
         end if;
      end Assert;

   --  Start of processing for Maybe_Move_Marker

   begin
      --  We can't have two different Markers pointing to the same character
      --  (see Mark), so we don't need a while loop here.

      if Buf.From_Markers_First <= Last_Index (Buf.From_Markers) then
         Move_Marker (Buf);
      end if;

      pragma Debug (Assert);
   end Maybe_Move_Marker;

   procedure Move_Forward (Buf : in out Buffer) is
      pragma Assert (not At_End (Buf));
      From_First : constant Positive := Buf.From_First + 1;
   begin
      Buf.From_First := From_First;
      Append (Buf.To, Buf.Cur_Char);
--         if C = NL then
--            Buf.Cur_Column := 1;
--         else
--            Buf.Cur_Column := Buf.Cur_Column + 1;
--         end if;
      Buf.Cur_Char :=
        (if From_First > Last_Index (Buf.From) then W_NUL
         else Buf.From (From_First));
      Maybe_Move_Marker (Buf);
   end Move_Forward;

   function Move_Forward (Buf : in out Buffer) return W_Char is
      Result : W_Char;
      pragma Assert (not At_End (Buf));
      From_First : constant Positive := Buf.From_First + 1;
   begin
      Buf.From_First := From_First;
      Append (Buf.To, Buf.Cur_Char);
--         if C = NL then
--            Buf.Cur_Column := 1;
--         else
--            Buf.Cur_Column := Buf.Cur_Column + 1;
--         end if;
      Result :=
        (if From_First > Last_Index (Buf.From) then W_NUL
         else Buf.From (From_First));
      Buf.Cur_Char := Result;
      Maybe_Move_Marker (Buf);
      return Result;
   end Move_Forward;

   procedure Initialize_Buffer (Buf : in out Buffer) is
   begin
      Buf.From_First         := 1;
      Buf.From_Markers_First := 1;
      Buf.Cur_Char           := W_NUL;
      Buf.Cur_Column         := 1;
   end Initialize_Buffer;

   procedure Initialize (Buf : in out Buffer) renames Initialize_Buffer;

   procedure Maybe_Adjust_Marker (Buf : in out Buffer) is
   begin
      if not Is_Empty (Buf.To_Markers)
        and then Buf.From_First <= Last_Index (Buf.From)
      then
         declare
            Mark : constant Marker := Last_Element (Buf.To_Markers);

         begin
            if At_Point (Buf, Mark) then
               declare
                  Rec : Marker_Rec renames Buf.Markers (Mark);

               begin
                  Rec.Position := Rec.Position + 1;
               end;
            end if;
         end;
      end if;
   end Maybe_Adjust_Marker;

   procedure Insert (Buf : in out Buffer; C : W_Char) is
      pragma Assert (C /= W_NUL);
      pragma Assert (not Is_Line_Terminator (C));
      pragma Assert (C /= W_HT); -- ???For now
   begin
      --  The whole point of this package is that we don't need to adjust the
      --  Markers here! Markers in To_Markers are to the left of the newly
      --  inserted character, so their Position is correct. Valid Markers in
      --  From_Markers (in From_Markers(From_Markers_First..From_Markers'Last))
      --  have Positions relative to From, so also don't need to be adjusted.
      --  A Marker's Position only needs to be adjusted when 'point' is moved
      --  forward past the Marker.
      --
      --  There is one minor exception: A marker that is at 'point' needs to be
      --  adjusted, but only if 'point' is not at the end:

      Maybe_Adjust_Marker (Buf);

--      Buf.Cur_Column := Buf.Cur_Column + 1;
      Append (Buf.To, C);
   end Insert;

   procedure Insert (Buf : in out Buffer; S : W_Str) is
   begin
      for C of S loop
         Insert (Buf, C);
      end loop;
   end Insert;

   procedure Insert_Any (Buf : in out Buffer; C : W_Char) is
      pragma Assert (C /= W_NUL);
      pragma Assert (C /= W_HT); -- ???For now
   begin
      Maybe_Adjust_Marker (Buf);

      pragma Assert -- no trailing blanks allowed
        (if
           Is_Line_Terminator (C)
         then
           (Is_Empty (Buf.To) or else Last_Element (Buf.To) /= ' '));

--      if Is_Line_Terminator (C) then
--         Buf.Cur_Column := 1;
--      else
--         Buf.Cur_Column := Buf.Cur_Column + 1;
--      end if;
      Append (Buf.To, C);
   end Insert_Any;

   procedure Insert_Any (Buf : in out Buffer; S : W_Str) is
   begin
      for C of S loop
         Insert_Any (Buf, C);
      end loop;
   end Insert_Any;

   procedure Insert_NL (Buf : in out Buffer) is
   begin
      Maybe_Adjust_Marker (Buf);

--      Buf.Cur_Column := 1;
      pragma Assert -- no trailing blanks allowed
      (Is_Empty (Buf.To) or else Last_Element (Buf.To) /= ' ');

      Append (Buf.To, NL);
   end Insert_NL;

   procedure Append (Buf : in out Buffer; C : W_Char) is
      pragma Assert (False); -- not currently used
      pragma Assert (At_End (Buf));
      pragma Assert (not Is_Line_Terminator (C));
      pragma Assert (C /= W_HT); -- ???For now
   begin
      Maybe_Adjust_Marker (Buf);

--      Buf.Cur_Column := Buf.Cur_Column + 1;
      Append (Buf.To, C);
      pragma Assert (At_End (Buf));
   end Append;

   procedure Append (Buf : in out Buffer; S : W_Str) is
      pragma Assert (False); -- not currently used
   begin
      for C of S loop
         Append (Buf, C);
      end loop;
   end Append;

   procedure Append_Any (Buf : in out Buffer; C : W_Char) is
      pragma Assert (At_End (Buf));
      pragma Assert (C /= W_HT); -- ???For now
   begin
      Maybe_Adjust_Marker (Buf);

      pragma Assert -- no trailing blanks allowed
        (if
           Is_Line_Terminator (C)
         then
           (Is_Empty (Buf.To) or else Last_Element (Buf.To) /= ' '));

--      if Is_Line_Terminator (C) then
--         Buf.Cur_Column := 1;
--      else
--         Buf.Cur_Column := Buf.Cur_Column + 1;
--      end if;
      Append (Buf.To, C);
      pragma Assert (At_End (Buf));
   end Append_Any;

   procedure Append_Any (Buf : in out Buffer; S : W_Str) is
      pragma Assert (False); -- not currently used
   begin
      for C of S loop
         Append_Any (Buf, C);
      end loop;
   end Append_Any;

   procedure Append_NL (Buf : in out Buffer) is
      pragma Assert (False); -- not currently used
      pragma Assert (At_End (Buf));
   begin
      Maybe_Adjust_Marker (Buf);

--      Buf.Cur_Column := 1;
      pragma Assert -- no trailing blanks allowed
      (Is_Empty (Buf.To) or else Last_Element (Buf.To) /= ' ');

      Append (Buf.To, NL);
      pragma Assert (At_End (Buf));
   end Append_NL;

   procedure Insert_Keeping_Mark
     (Buf  : in out Buffer;
      Mark : Marker;
      C    : W_Char)
   is
   begin
      pragma Assert (False); -- ???Not used

      Append (Buf.To, C);
--      if C = NL then
--         Buf.Cur_Column := 1;
--      else
--         Buf.Cur_Column := Buf.Cur_Column + 1;
--      end if;

      pragma Assert (Char_At (Buf, Mark) = C);
   end Insert_Keeping_Mark;

   function Is_Empty (Buf : Buffer) return Boolean is
   begin
      return
        Result : constant Boolean :=
          Is_Empty (Buf.From) and then Is_Empty (Buf.To)
      do
         pragma Assert (Result = Is_Empty (Buf.Markers));
         pragma Assert (Result = Is_Empty (Buf.From_Markers));
         pragma Assert (Result = Is_Empty (Buf.To_Markers));
      end return;
   end Is_Empty;

   function Last_Position (Buf : Buffer) return Natural is
   begin
      return Last_Index (Buf.To) +
        (Last_Index (Buf.From) - Buf.From_First + 1);
   end Last_Position;

   function Lookahead (Buf : Buffer; Offset : Positive := 1) return W_Char is
   begin
      if Buf.From_First + Offset <= Last_Index (Buf.From) then
         return Buf.From (Buf.From_First + Offset);

      else
         return W_NUL;
      end if;
   end Lookahead;

   function Lookback (Buf : Buffer; Offset : Positive := 1) return W_Char is
   begin
      if Last_Index (Buf.To) - Offset + 1 >= 1 then
         return Buf.To (Last_Index (Buf.To) - Offset + 1);

      else
         return W_NUL;
      end if;
   end Lookback;

   function Mark (Buf : in out Buffer; Name : W_Char) return Marker is
   begin
      if Is_Empty (Buf.To_Markers) then
         Append_New_Marker (Buf, Name); -- Create a new one

      --  Avoid creating a new Marker if we already have one at 'point'

      elsif not At_Point (Buf, Last_Element (Buf.To_Markers)) then
         Append_New_Marker (Buf, Name); -- Create a new one
      end if;

      return Last_Element (Buf.To_Markers);
   end Mark;

   function Mark_Previous (Buf : in out Buffer; Name : W_Char) return Marker is
      L       : constant Marker_Index'Base := Last_Index (Buf.To_Markers);
      M, Save : Marker;
      At_P    : Boolean;

   begin
      if L >= 1
        and then Position (Buf, Buf.To_Markers (L)) = Last_Index (Buf.To)
      then
         return Buf.To_Markers (L);
      end if;
      if L >= 2
        and then Position (Buf, Buf.To_Markers (L - 1)) = Last_Index (Buf.To)
      then
         return Buf.To_Markers (L - 1);
      end if;

      At_P := L > 0 and then At_Point (Buf, Buf.To_Markers (L));
      if At_P then
         Save := Last_Element (Buf.To_Markers);
         Delete_Last (Buf.To_Markers);
      end if;

      Append
        (Buf.Markers,
         Marker_Rec'
           (Position => Last_Index (Buf.To),
            Flag     => Buf.To_Flag,
            Name     => Name));
      M := Last_Index (Buf.Markers);
      Append (Buf.To_Markers, M);
      if At_P then
         Append (Buf.To_Markers, Save);
      end if;

      return M;
   end Mark_Previous;

   function Name (Buf : Buffer; Mark : Marker) return W_Char is
   begin
      return Buf.Markers (Mark).Name;
   end Name;

   function Point (Buf : Buffer) return Positive is
   begin
      return Last_Index (Buf.To) + 1;
   end Point;

   function Position (Buf : Buffer; Mark : Marker) return Positive is
      Rec : constant Marker_Rec := Buf.Markers (Mark);

   begin
      if Rec.Flag = Buf.To_Flag then
         return Rec.Position;

      else
         return Last_Index (Buf.To) - (Buf.From_First - 1) + Rec.Position;
      end if;
   end Position;

   procedure Insert_Ada (Buf : in out Buffer; C : W_Char) is
      pragma Assert (C /= W_HT); -- ???For now
   begin
      pragma Assert (At_End (Buf));
      pragma Assert
        (Is_Empty (Buf.To_Markers) and then Is_Empty (Buf.From_Markers));

      if Is_Line_Terminator (C) then
         Buf.Cur_Column := 1;

         --  Delete trailing blanks at end of line. There are no markers to
         --  worry about here.

         while not Is_Empty (Buf.To) and then Is_Space (Last_Element (Buf.To))
         loop
            Delete_Last (Buf.To);
         end loop;
      else
         Buf.Cur_Column := Buf.Cur_Column + 1;
      end if;
      Append (Buf.To, C);
   end Insert_Ada;

   procedure Insert_Ada_Source
     (Buf         : in out Buffer;
      Input       : String;
      Wide_Character_Encoding_Method : System.WCh_Con.WC_Encoding_Method;
      Expand_Tabs : Boolean := False)
   is
      pragma Assert (Expand_Tabs); -- ???For now

      package Decoder is new GNAT.Decode_String
        (Encoding_Method => Wide_Character_Encoding_Method);
      package Brackets_Decoder is new GNAT.Decode_String
        (Encoding_Method => System.WCh_Con.WCEM_Brackets);
      Ptr     : Natural  := Input'First;
      C       : W_Char;
      Tab_Len : constant := 8;

      function At_Brackets_Start return Boolean with
         Pre => Input (Ptr) = '[';
         --  True if we're pointing to the start of a valid brackets sequence
         --  to be interpreted as a wide character.

      function At_Brackets_Start
        return Boolean is
        (Ptr + 2 <= Input'Last
         and then Input (Ptr + 1) = '"'
         and then Input (Ptr + 2) in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F');

      type State_Enum is (In_Comment, In_String_Literal, Other);
      State : State_Enum := Other;
   --  We need to keep track of whether we're inside a comment, because
   --  brackets encoding is disabled in that case. We need to keep track of
   --  whether we're inside a string literal in order to keep track of whether
   --  we're inside a comment ('--' doesn't start a comment inside a string
   --  literal).

   begin
      while Ptr <= Input'Last loop
         --  Set C to the current wide character

         if Input (Ptr) = '[' then
            if State /= In_Comment and then At_Brackets_Start then
               Brackets_Decoder.Decode_Wide_Character (Input, Ptr, C);
            else
               C   := '[';
               Ptr := Ptr + 1;
            end if;
         else
            Decoder.Decode_Wide_Character (Input, Ptr, C);
         end if;

         --  Keep track of whether we're in a comment

         case State is
            when In_Comment =>
               if Is_Line_Terminator (C) then
                  if False and then C = W_VT then
                     --  Ignore VT characters in comments. This differs from
                     --  the behavior of the old gnatpp, which has an option
                     --  for that. ???Disable this, at least for now.
                     --  There's really no such thing as a VT in a comment,
                     --  because VT is a line terminator.
                     Ptr := Ptr + 1;
                     goto Continue;
                  end if;
                  State := Other;
               end if;
            when In_String_Literal =>
               if C in '"' | '%' then
                  State := Other;
               end if;
            when Other =>
               if C = '-'
                 and then Ptr <= Input'Last
                 and then Input (Ptr) = '-'
               then
                  State := In_Comment;
               elsif C in '"' | '%' then
                  State := In_String_Literal;
               end if;
         end case;

         --  Insert C into the buffer, expanding tabs

         if C = W_HT and then Expand_Tabs then
            loop
               Insert_Ada (Buf, ' ');
               exit when Buf.Cur_Column mod Tab_Len = 1;
            end loop;
         else
            Insert_Ada (Buf, C);
         end if;
         <<Continue>>
      end loop;

      --  Make sure last line is terminated by NL
      if C /= NL then
         Insert_Ada (Buf, NL);
      end if;
   end Insert_Ada_Source;

   procedure Read_Ada_File
     (Buf         : in out Buffer;
      File_Name   : String;
      Wide_Character_Encoding_Method : System.WCh_Con.WC_Encoding_Method :=
        System.WCh_Con.WCEM_Brackets;
      BOM_Seen    : out Boolean;
      Expand_Tabs : Boolean := False)
   is
      --  We read the file into a String, and convert to wide characters
      --  according to the encoding method.
      --
      --  No matter what the encoding method is, we recognize brackets
      --  encoding, but not within comments.
      --
      --  These behaviors are intended to match what the compiler does.

      Input : String_Access := Read_File (File_Name);
      First : Natural       := 1;

      use GNAT.Byte_Order_Mark;
      BOM     : BOM_Kind;
      BOM_Len : Natural;

   begin
      Clear (Buf);

      --  Check for BOM at start of file. The only supported BOM is
      --  UTF8_All. If present, when we're called from gnatpp, the
      --  Wide_Character_Encoding_Method should already be set to
      --  WCEM_UTF8, but when we're called from xml2gnat, we need to set it.

      Read_BOM (Input.all, BOM_Len, BOM);
      if BOM = UTF8_All then
         First := BOM_Len + 1; -- skip it
         BOM_Seen := True;
      else
         pragma Assert (BOM = Unknown); -- no BOM found
         BOM_Seen := False;
      end if;

      Insert_Ada_Source (Buf, Input (First .. Input'Last),
                         Wide_Character_Encoding_Method, Expand_Tabs);

      Free (Input);
      Reset (Buf);
   end Read_Ada_File;

   procedure Replace_Cur (Buf : in out Buffer; C : W_Char) is
   begin
      pragma Assert (not At_End (Buf));
      Buf.From (Buf.From_First) := C;
      Buf.Cur_Char              := C;
      pragma Assert -- no trailing blanks
        (if
           C = NL
         then
           (Is_Empty (Buf.To) or else Last_Element (Buf.To) /= ' '));

      --  No need to adjust Buf.Cur_Column
   end Replace_Cur;

   procedure Replace_Previous (Buf : in out Buffer; C : W_Char) is
   begin
      Buf.To (Last_Index (Buf.To)) := C;
      pragma Assert -- no trailing blanks
        (if
           C = NL
         then
           (Last_Index (Buf.To) = 1
            or else Buf.To (Last_Index (Buf.To) - 1) /= ' '));
--      if C = NL then
--         Buf.Cur_Column := 1;
--      end if;
   end Replace_Previous;

   procedure Reset (Buf : in out Buffer) is
   begin
      pragma Debug (Validate (Buf, "Reset"));

      pragma Assert (Buf.From_First = Last_Index (Buf.From) + 1);
      if Buf.From_Markers_First = Last_Index (Buf.From_Markers) then
         Move_Forward (Buf);
      end if;
      pragma Assert
        (Buf.From_Markers_First = Last_Index (Buf.From_Markers) + 1);

      Move (Target => Buf.From, Source => Buf.To);
      pragma Assert (Is_Empty (Buf.To));

      Move (Target => Buf.From_Markers, Source => Buf.To_Markers);
      pragma Assert (Is_Empty (Buf.To_Markers));

      Buf.From_First         := 1;
      Buf.From_Markers_First := 1;
      Buf.To_Flag            := not Buf.To_Flag;
      Buf.Cur_Char           :=
        (if Buf.From_First > Last_Index (Buf.From) then W_NUL
         else Buf.From (Buf.From_First));
      Buf.Cur_Column := 1;

      --  If there is a Marker pointing to the first character, we need to move
      --  it to To to preserve the invariant.

      Maybe_Move_Marker (Buf);
   end Reset;

   function Slice (Buf : Buffer; First, Last : Marker) return W_Str is
      pragma Assert (Position (Buf, First) <= Position (Buf, Last));
      F                          : constant Marker_Rec := Buf.Markers (First);
      L                          : constant Marker_Rec := Buf.Markers (Last);
      To_F, To_L, From_F, From_L : Natural;
   begin
      if F.Flag = Buf.To_Flag then
         To_F   := F.Position;
         From_F := 1;
      else
         To_F   := Last_Index (Buf.To) + 1;
         From_F := F.Position;
      end if;
      if L.Flag = Buf.To_Flag then
         To_L   := L.Position - 1;
         From_L := 0;
      else
         To_L   := Last_Index (Buf.To);
         From_L := L.Position - 1;
      end if;

      declare
         Result : constant W_Str :=
           Slice (Buf.To, To_F, To_L) & Slice (Buf.From, From_F, From_L);
         pragma Assert (Result'First = 1); -- Ensured by Slice
      begin
         if False then -- Too slow, but we keep it for documentation
            pragma Assert
              (Result =
               To_W_Str (Buf)
                 (Position (Buf, First) .. Position (Buf, Last) - 1));
         end if;
         return Result;
      end;
   end Slice;

   function Slice
     (Buf   : Buffer;
      First : Positive;
      Last  : Natural;
      Lines : Boolean := False)
      return  W_Str
   is
      F : Positive := First;
      L : Natural  := Last;
   begin
      if Lines then
         while F > 1 and then Char_At (Buf, F) /= NL loop
            F := F - 1;
         end loop;
         while L < Last_Position (Buf) and then Char_At (Buf, L) /= NL loop
            L := L + 1;
         end loop;
      end if;

      return Result : W_Str (1 .. L - F + 1) do
         for J in Result'Range loop
            Result (J) := Char_At (Buf, F + J - 1);
         end loop;
         if False then -- Slow
            pragma Assert (Result = To_W_Str (Buf) (F .. L));
         end if;
      end return;
   end Slice;

   function String_To_Buffer (S : W_Str) return Buffer is
   begin
      --  This is called only for debugging. "pragma Assert (Assert_Enabled);"
      --  doesn't work, because if Assert_Enabled is False, the pragma is
      --  disabled.

      if not Assert_Enabled then
         raise Program_Error;
      end if;

      return Buf : Buffer do
         Insert (Buf, S);
         Reset (Buf);
      end return;
   end String_To_Buffer;

   procedure Move (Target, Source : in out Buffer) is
      Source_Str : constant W_Str :=
        (if Assert_Enabled then To_W_Str (Source) else "");
      --  For Assert below
   begin
      Clear (Target);

      Move (Target => Target.From, Source => Source.From);
      Move (Target => Target.To, Source => Source.To);
      Move (Target => Target.Markers, Source => Source.Markers);
      Move (Target => Target.From_Markers, Source => Source.From_Markers);
      Move (Target => Target.To_Markers, Source => Source.To_Markers);

      Target.From_First := Source.From_First;
      Target.From_Markers_First := Source.From_Markers_First;
      Target.Cur_Char := Source.Cur_Char;
      Target.Cur_Column := Source.Cur_Column;

      pragma Assert (To_W_Str (Target) = Source_Str);
   end Move;

   function To_Debug_String (Buf : Buffer) return String is
      S     : constant W_Str        := To_W_Str (Buf);
      Marks : constant Marker_Array :=
        To_Array (Buf.To_Markers) &
        Slice
          (Buf.From_Markers,
           Buf.From_Markers_First,
           Last_Index (Buf.From_Markers));
      pragma Assert (Marks'First = 1);
      M_Index : Marker_Index := 1;
      Result  : Char_Vector;

   --  Start of processing for To_Debug_String

   begin
      for X in S'Range loop
         if M_Index <= Marks'Last then
            pragma Assert (X <= Position (Buf, Marks (M_Index)));
            if X = Position (Buf, Marks (M_Index)) then
               Append (Result, Name (Buf, Marks (M_Index)));
               M_Index := M_Index + 1;
            end if;

            while M_Index <= Marks'Last
              and then X = Position (Buf, Marks (M_Index))
            loop
               Append (Result, "[duplicate]");
               Append (Result, Name (Buf, Marks (M_Index)));
               M_Index := M_Index + 1;
            end loop;
         end if;
         Append (Result, S (X));
      end loop;

      --  Check for one last marker after the last character

      if M_Index = Marks'Last then
         Append (Result, Name (Buf, Marks (M_Index)));
         M_Index := M_Index + 1;
      end if;
      pragma Assert (M_Index = Marks'Last + 1);

      return To_UTF8 (To_Array (Result));
   end To_Debug_String;

   function To_W_Str (Buf : Buffer) return W_Str is
   begin
      return Result : constant W_Str := To_Array (Buf.To) & Valid_From (Buf) do
         pragma Assert (Result'First = 1);
      end return;
   end To_W_Str;

   function To_String (Buf : Buffer) return String is
   begin
      return To_UTF8 (To_W_Str (Buf));
   end To_String;

   function To_Vector (Buf : Buffer) return Char_Vector is
   begin
      pragma Assert (Is_Empty (Buf.To) and then Buf.From_First = 1);
      return Buf.From;
   end To_Vector;

   function Elements
     (Buf  : Buffer)
      return ASIS_UL.Char_Vectors.Char_Vectors.Big_Ptr
   is
   begin
      pragma Assert (Is_Empty (Buf.To) and then Buf.From_First = 1);
      return Elems (Buf.From);
   end Elements;

   function Valid_From (Buf : Buffer) return W_Str is
   begin
      return Slice (Buf.From, Buf.From_First, Last_Index (Buf.From));
   end Valid_From;

   Validations : Natural := 0;

   procedure Validate (Buf : Buffer; Message : String) is

      procedure Fail (Msg : String);

      procedure Fail (Msg : String) is
      begin
         Dbg_Out.Output_Enabled := True;
         Dbg_Out.Put ("Validate: \1: failure: \2\n", Message, Msg);
         Dump_Buffer (Buf);
         raise Program_Error;
      end Fail;

      function Marker_Less (X, Y : Marker) return Boolean;

      function Marker_Less (X, Y : Marker) return Boolean is
      begin
         return Position (Buf, X) < Position (Buf, Y);
      end Marker_Less;

      package Sorting is new Marker_Vectors.Generic_Sorting
        ("<" => Marker_Less);

   --  Start of processing for Validate

   begin
      if False then
         Dbg_Out.Output_Enabled := True;
         Dbg_Out.Put ("\nValidate: \1\n", Message);
         Dump_Buffer (Buf);
      end if;

      if not Sorting.Is_Sorted (Buf.To_Markers) then
         Fail ("To_Markers not sorted");
      end if;
      if not Sorting.Is_Sorted (Buf.From_Markers) then
         Fail ("From_Markers not sorted");
      end if;

      for M in 1 .. Last_Index (Buf.To_Markers) loop
         declare
            Mark : constant Marker     := Buf.To_Markers (M);
            Rec  : constant Marker_Rec := Buf.Markers (Mark);

         begin
            if Rec.Flag /= Buf.To_Flag then
               Fail ("Bad To_Marker flag");
            end if;
            pragma Warnings (Off);
            --  "lower bound check only fails if it is invalid"
            if Rec.Position not in 1 .. Last_Index (Buf.To) + 1 then
               pragma Warnings (On);
               Fail ("Bad To_Marker position");
            end if;
         end;
      end loop;

      for M in Buf.From_Markers_First .. Last_Index (Buf.From_Markers) loop
         declare
            Mark : constant Marker     := Buf.From_Markers (M);
            Rec  : constant Marker_Rec := Buf.Markers (Mark);

         begin
            if Rec.Flag /= not Buf.To_Flag then
               Fail ("Bad From_Marker flag");
            end if;
            if Rec.Position not in
                Buf.From_First + 1 .. Last_Index (Buf.From) + 1
            then
               Fail ("Bad From_Marker position");
            end if;
         end;
      end loop;

      if Last_Index (Buf.To_Markers) + Last_Index (Buf.From_Markers)
        < 100_000 -- arbitrary limit, to avoid stack overflow
      then
         declare
            Marks : constant Marker_Array :=
              To_Array (Buf.To_Markers) &
              Slice
              (Buf.From_Markers,
               Buf.From_Markers_First,
               Last_Index (Buf.From_Markers));
            pragma Assert (Marks'First = 1);
         begin
            for J in 2 .. Marks'Last loop
               if Position (Buf, Marks (J - 1))
                 = Position (Buf, Marks (J))
               then
                  Fail ("Duplicate marks");
               end if;
            end loop;
         end;
      end if;

      Validations := Validations + 1;
   end Validate;

end Pp.Buffers;
