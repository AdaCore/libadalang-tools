------------------------------------------------------------------------------
--                                                                          --
--                             Libadalang Tools                             --
--                                                                          --
--                    Copyright (C) 2021-2023, AdaCore                      --
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

with Utils.Err_Out;
with GNATCOLL.Terminal;

package body Pp is

   Info_Message_Style : GNATCOLL.Traces.Message_Style renames
     GNATCOLL.Traces.Use_Default_Style;

   Debug_Message_Style : constant GNATCOLL.Traces.Message_Style :=
     GNATCOLL.Traces.Message_Style'
       (Fg    => GNATCOLL.Terminal.Blue,
        Bg    => GNATCOLL.Terminal.Unchanged,
        Style => GNATCOLL.Terminal.Unchanged);

   Error_Message_Style : constant GNATCOLL.Traces.Message_Style :=
     GNATCOLL.Traces.Message_Style'
       (Fg    => GNATCOLL.Terminal.Red,
        Bg    => GNATCOLL.Terminal.Unchanged,
        Style => GNATCOLL.Terminal.Unchanged);

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Message  : String;
      Level    : Log_Level := Info) is
   begin
      case Level is
         when Info =>
            Info_Logger.Trace
              (Message => Message,
               Style   => Info_Message_Style);
         when Debug =>
            Debug_Logger.Trace
              (Message => Message,
               Style   => Debug_Message_Style);
         when Error =>
            if Utils.Err_Out.Output_Enabled then
               Utils.Err_Out.Put ("\1\n", Message);
            end if;
            Error_Logger.Trace
              (Message => Message,
               Style   => Error_Message_Style);
      end case;
   end Trace;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (E        : Ada.Exceptions.Exception_Occurrence;
      Message  : String := "Unexpected exception: ";
      Level    : Log_Level := Error) is
   begin
      case Level is
         when Info =>
            Info_Logger.Trace
              (E       => E,
               Msg     => Message,
               Style   => Info_Message_Style);
         when Debug =>
            Debug_Logger.Trace
              (E       => E,
               Msg     => Message,
               Style   => Debug_Message_Style);
         when Error =>
            if Utils.Err_Out.Output_Enabled then
               Utils.Err_Out.Put ("\1\n", Message);
            end if;
            Error_Logger.Trace
              (E       => E,
               Msg     => Message,
               Style   => Error_Message_Style);
      end case;
   end Trace;

end Pp;
