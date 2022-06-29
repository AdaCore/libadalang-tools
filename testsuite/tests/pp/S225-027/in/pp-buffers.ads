------------------------------------------------------------------------------
--                                                                          --
--                            GNAT2XML COMPONENTS                           --
--                                                                          --
--                      G N A T 2 X M L . B U F F E R S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                      Copyright (C) 2013-2017, AdaCore                    --
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

with System.WCh_Con;
with Utils.Vectors;
with Utils.Char_Vectors; use Utils.Char_Vectors;
use Utils.Char_Vectors.WChar_Vectors;
--  use all type Utils.Char_Vectors.WChar_Vector;

private with Ada.Finalization;

package Pp.Buffers is

   --  This package supports a character Buffer type, and a Marker type that
   --  can be used to point at particular characters. The main feature of
   --  this abstraction is that Markers are automatically kept up to date
   --  as modifications are made. For example, suppose the buffer Buf contains:
   --
   --     "Hello, world."
   --      123456789 123
   --           ^   ^
   --  and we have two Markers, one pointing at Buf(6) = ',' and the other
   --  pointing at Buf(10) = 'r'. Suppose the insertion point is at Buf(8) =
   --  'w', meaning insertions will occur before that character. If we insert
   --  "***", we will then have:
   --
   --     "Hello, ***world."
   --      123456789 123456
   --           ^      ^
   --  Note that the second Mark has "moved" so that it still points at the
   --  'r'.
   --
   --  The basic idea is to move through the buffer (see procedure
   --  Move_Forward), inserting text at various places (see procedure Insert).
   --  When we get to the end, we can call Reset to move back to the beginning.

   type Buffer is private;
   --  Initially empty. A buffer has a current insertion point, called 'point',
   --  which is initially at position 1.

   type Marker is new Positive;
   --  A Marker is a pointer to a particular character in the buffer. Note that
   --  a Marker is valid only for a particular Buffer; it cannot be used to
   --  refer to a different Buffer, nor can it be used after Clear.
   --
   --  Type Marker should really be private, but Ada has an annoying
   --  restriction that would disallow package Marker_Vectors below, so we
   --  expose the fact that it's an integer. You shouldn't be doing arithmetic
   --  and the like on it outside this package. "=" comparisons are OK.

   type Marker_Index is new Positive;

   type Marker_Array is array (Marker_Index range <>) of Marker;
   package Marker_Vectors is new Utils.Vectors (Marker_Index, Marker,
      Marker_Array);
   subtype Marker_Vector is Marker_Vectors.Vector;

   type Marker_Vector_Ptr is access all Marker_Vector;
   use Marker_Vectors;
   --  use all type Marker_Vector;

   function At_End (Buf : Buffer) return Boolean;
   pragma Inline (At_End);
   --  True if 'point' is past the last character

   function At_Beginning (Buf : Buffer) return Boolean;
   pragma Inline (At_Beginning);
   --  True if 'point' points to the first character (i.e. position 1)

   function Cur (Buf : Buffer) return W_Char with
     Post => Cur'Result /= W_HT; -- ???For now
   pragma Inline (Cur);
   --  Return the character at 'point'. NUL if At_End.

   function Cur_Column (Buf : Buffer) return Positive;
   pragma Inline (Cur_Column);
   --  Column number in which the Cur character appears

   function Lookahead (Buf : Buffer; Offset : Positive := 1) return W_Char;
   --  Return the character at 'point' + Offset. NUL if out of range.

   function Lookback (Buf : Buffer; Offset : Positive := 1) return W_Char;
   --  Return the character at 'point' - Offset. NUL if out of range.

   function To_String (Buf : Buffer) return String with
     Post => To_String'Result'First = 1;
   function To_W_Str (Buf : Buffer) return W_Str with
     Post => To_W_Str'Result'First = 1;
   --  Returns the current logical string of the buffer

   function To_Vector (Buf : Buffer) return WChar_Vector with
     Pre => At_Beginning (Buf);
   --  'point' must be at the beginning of the buffer (e.g. after Reset).
   --  Returns the content of the buffer.

   function Elements
     (Buf : Buffer) return Utils.Char_Vectors.WChar_Vectors.Big_Ptr with
     Pre => At_Beginning (Buf);
   --  'point' must be at the beginning of the buffer (e.g. after Reset).
   --  Returns the content of the buffer.

   function Char_At (Buf : Buffer; Mark : Marker) return W_Char;
   function Char_At (Buf : Buffer; Position : Positive) return W_Char;
   pragma Inline (Char_At);
   --  Return the character at the given Mark/Position

   function Slice (Buf : Buffer; First, Last : Marker) return W_Str with
     Post => Slice'Result'First = 1;
   --  Return the string from First up to just before Last

   function Slice
     (Buf : Buffer; First : Positive; Last : Natural; Lines : Boolean := False)
      return W_Str with
     Post => Slice'Result'First = 1;
   --  Return the string from First up to and including Last.
   --  If Lines is True, we expand the slice to include whole lines.

   procedure Insert (Buf : in out Buffer; C : W_Char);
   procedure Insert (Buf : in out Buffer; S : W_Str);
   procedure Insert_Any (Buf : in out Buffer; C : W_Char);
   procedure Insert_Any (Buf : in out Buffer; S : W_Str);
   --  Insert C/S at 'point', leaving 'point' after the insertion. It is an
   --  error for NL to follow ' '. Insert disallows NLs; Insert_Any allows
   --  them.

   procedure Insert_NL (Buf : in out Buffer);
   --  Same as Insert_Any (Buf, NL)

   procedure Append (Buf : in out Buffer; C : W_Char);
   procedure Append (Buf : in out Buffer; S : W_Str);
   procedure Append_Any (Buf : in out Buffer; C : W_Char);
   procedure Append_Any (Buf : in out Buffer; S : W_Str);
   procedure Append_NL (Buf : in out Buffer);
   --  Above are the same as Insert*, except that they may be slightly more
   --  efficient, but they only work when inserting at the end of the buffer.
   --  ???Actually, we should probably get rid of these; they probably don't
   --  do any good.

   procedure Insert_Keeping_Mark
     (Buf : in out Buffer; Mark : Marker; C : W_Char) with
     Pre => At_Point (Buf, Mark),
     Unreferenced; -- currently not used
   --  Mark must be at 'point'. This does the same as Insert (Buf, C),
   --  except that Mark is not adjusted to point to the character after
   --  'point'; it ends up pointing at the newly-inserted C.

   procedure Replace_Cur (Buf : in out Buffer; C : W_Char);
   --  Replace character at 'point' with C

   procedure Replace_Previous (Buf : in out Buffer; C : W_Char);
   --  Replace character just before 'point' with C

   function String_To_Buffer (S : W_Str) return Buffer;
   --  Return a buffer containing S, with 'point' set to the beginning

   procedure Move_Forward (Buf : in out Buffer);
   function Move_Forward (Buf : in out Buffer) return W_Char;
   --  Move 'point' forward one character position. 'point' must not be at the
   --  end. The function version returns the new current character.

   procedure Delete_Char (Buf : in out Buffer) with
     Pre => not At_End (Buf) and then False;
   --  Delete the character at 'point', leaving 'point' at the following one.
   --  ???This causes "duplicate marker" errors; currently not used.

   procedure Clear (Buf : in out Buffer);
   --  Set the buffer to its initial empty state. All existing Markers become
   --  invalid.

   function Is_Empty (Buf : Buffer) return Boolean;

   procedure Reset (Buf : in out Buffer) with
     Pre  => At_End (Buf),
     Post => At_Beginning (Buf);
   --  'point' must be at the end of the buffer. Move 'point' back to the
   --  beginning. The buffer contents and markers are not changed.

   function Mark (Buf : in out Buffer; Name : W_Char) return Marker with
     Unreferenced;
   --  Return a Marker that points to the current 'point'. Name is for
   --  debugging; it is printed by debugging printouts, and may be used to keep
   --  track of different kinds of Markers. Note that if you call Mark twice at
   --  the same position, only the first Name will be used.

   function Mark_Previous
     (Buf : in out Buffer; Name : W_Char) return Marker with
     Unreferenced;
   --  Similar to Mark, but the Marker points to the character just before the
   --  current 'point'.

   function At_Point (Buf : Buffer; Mark : Marker) return Boolean;
   function At_Point (Buf : Buffer; Position : Positive) return Boolean;
   pragma Inline (At_Point);
   --  True if Mark/Position = the current 'point'

   function Point (Buf : Buffer) return Positive;
   --  Returns the position of 'point' in the logical string

   function Position (Buf : Buffer; Mark : Marker) return Positive;
   --  Returns the position of the mark in the logical string

   function Last_Position (Buf : Buffer) return Natural;
   --  Returns the last position in the buffer

   function Mark_LT (Buf : Buffer; M, N : Marker) return Boolean;
   --  less than

   function Mark_LE (Buf : Buffer; M, N : Marker) return Boolean;
   --  less than or equal

   procedure Insert_Ada_Source
     (Buf                     : in out Buffer; Input : String;
      Wide_Character_Encoding :        System.WCh_Con.WC_Encoding_Method;
      Expand_Tabs             :        Boolean := False);
   procedure Read_Ada_File
     (Buf                     : in out Buffer; File_Name : String;
      Wide_Character_Encoding :        System.WCh_Con.WC_Encoding_Method :=
        System.WCh_Con.WCEM_Brackets;
      BOM_Seen : out Boolean; Expand_Tabs : Boolean := False);
   --  Read an Ada source file into Buf. BOM_Seen is set to True if a UTF8_All
   --  BOM was seen; False otherwise.

   procedure Move (Target, Source : in out Buffer);

   function To_Debug_String (Buf : Buffer) return String with
     Post => To_Debug_String'Result'First = 1;
   --  For debugging. Returns the current logical string of the buffer, with
   --  the Name of each Marker interspersed.

   procedure Dump_Buf (Buf : Buffer); -- less verbose
   procedure Dump_Buffer (Buf : Buffer); -- more verbose
   procedure Dump_Marker (Buf : Buffer; Mark : Marker);
   --  For debugging

   function Marker_Name (Buf : Buffer; Mark : Marker) return W_Char;

   procedure Validate (Buf : Buffer; Message : String);

private

   --  The concept of markers that automatically track buffer changes comes
   --  from Emacs. The implementation here is not based on Emacs.

   --  Markers are logically updated on every insertion. The "obvious"
   --  implementation, which we don't use because it's grossly inefficient,
   --  is as follows: Store the characters in a Vector, and store the markers
   --  in another vector, each marker having the index of the corresponding
   --  character. When inserting a character, we would have to shove all the
   --  characters to the right of 'point' one position to the right. We would
   --  also have to shove all the markers to the right of 'point' one position
   --  to the right, and increment their index.
   --
   --  So we don't do that.
   --
   --  The actual implementation is set up so we don't have to loop through
   --  all the characters and Markers to the right of the 'point' every time we
   --  do an insertion. Instead, we update each Marker just once on each pass
   --  through the buffer. These updates are done when moving forward, not
   --  when inserting text (except in one minor case: insertion just before
   --  a marker).

   type Marker_Rec is record
      Position : Positive;
      --  Position in either From or To

      Flag : Boolean;
      --  Determines whether Position points into From or To. In particular,
      --  if Flag = Buf.To_Flag, Position points into To, and if Flag = not
      --  Buf.To_Flag, Position points into From. This trick allows us to
      --  switch all the Markers from To to From by flipping the To_Flag
      --  (see Reset).

      Name : W_Char;
   end record;

   type Marker_Rec_Array is array (Marker range <>) of Marker_Rec;
   package Marker_Rec_Vectors is new Utils.Vectors (Marker, Marker_Rec,
      Marker_Rec_Array);
   subtype Marker_Rec_Vector is Marker_Rec_Vectors.Vector;
   use Marker_Rec_Vectors;
   --  use all type Marker_Rec_Vector;

   type Buffer is new Ada.Finalization.Controlled with record
      To, From : WChar_Vector;
      --  The current characters of the buffer are:
      --
      --    To & From(From_First..From'Last)
      --
      --  This is what To_String returns, and what we call the "logical
      --  string". As we move the 'point' forward, we copy characters from From
      --  to To, and adjust From_First. Inserted characters are simply appended
      --  to To.

      From_First : Positive := 1;
      --  First in-use character in From. Characters before that have already
      --  been copied to To.

      Markers : Marker_Rec_Vector;
      --  Positions of all the Markers. A Marker is represented as an index
      --  into this array, offset by Unique_Id. The order in which Markers
      --  are stored in this array is not significant.

      To_Markers, From_Markers : Marker_Vector;
      --  To_Markers point into To, From_Markers into From. To_Markers includes
      --  all markers up to and including 'point', so the last To_Marker can be
      --  At_Point, in which case its Position is one past the end of To. That
      --  is, a marker pointing to the first character of From is the last
      --  element of To_Markers, rather than the first element of From_Markers
      --  as you might expect. This is necessary because we can only append to
      --  To_Markers, not prepend to From_Markers. Both arrays are stored in
      --  increasing order of Position.

      From_Markers_First : Marker_Index := 1;
      --  First in-use Marker in From_Markers. Markers before that have already
      --  been copied to To. Thus, all valid Markers are:
      --     To_Markers & From_Markers(From_Markers_First..From_Markers'Last).

      To_Flag : Boolean := False; -- Initial value doesn't matter

      Cur_Char : W_Char := W_NUL;
      --  This is the result of the Cur function. It is equal to the first
      --  character in the valid portion of From:
      --     Buf.From (Buf.From_First)
      --  unless we're at the end, in which case it is NUL.

      Cur_Column : Positive := 1;
   end record;

   overriding procedure Initialize (Buf : in out Buffer);

end Pp.Buffers;
