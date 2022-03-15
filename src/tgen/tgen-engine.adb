------------------------------------------------------------------------------
--                                                                          --
--                                  TGen                                    --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
--                                                                          --
-- TGen  is  free software; you can redistribute it and/or modify it  under --
-- under  terms of  the  GNU General  Public License  as  published by  the --
-- Free  Software  Foundation;  either version 3, or  (at your option)  any --
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

with Ada.Text_IO;

package body TGen.Engine is

   MAX_TESTS : constant Positive := 10;

   -----------------
   -- Test_Runner --
   -----------------

   procedure Test_Runner
     (Value_Stream : access Flushable_Stream'Class;
      Wrapped_Test : access procedure)
   is
   begin
      for I in 1 .. MAX_TESTS loop
         begin
            Wrapped_Test.all;
         exception
            when Invalid_Generation_Error =>
               Ada.Text_IO.Put_Line
                 ("Generated values do not respect the precondition. "
                  & "Regenerating...");
         end;

         Value_Stream.Flush;
      end loop;
   end Test_Runner;

end TGen.Engine;
