""" GNAT Studio gnatpp development plugin

This is a GNAT Studio plugin made for gnatpp developers.

You can load in in GNAT Studio by calling

   gnatstudio --load=python:<path to gs_pp_mode.py>  <your arguments>

"""

import os
import os_utils
from gs_utils import interactive

ADA_FILE_CONTENTS = """with Ada.Text_IO; use Ada.Text_IO;

procedure Hello is
begin
   Put_Line ("Hello World");
end Hello;
"""

TEMPLATE_FILE_CONTENTS = """
{
    "ADA_SUBP_BODY": "?~~ ~!?~~$~#+1 is$!!end$!"
}
"""

GNATPP_MODE = None


class plugin(object):
    def __init__(self):
        # Constructor

        # Setup some variables
        self.base_dir = os.path.join(
            GPS.Project.root().object_dirs()[0], "gnatpp_plugin"
        )
        self.tmp_dir = os.path.join(self.base_dir, "tmp")
        self.ada_file = os.path.join(self.base_dir, "hello.adb")
        self.template_file = os.path.join(self.base_dir, "templates.json")

        # Open the files for the user to edit
        self.open_files()

        # Add a hook to get called after the buffers are edited
        GPS.Hook("buffer_edited").add(self.on_buffer_edited)

        # Run right now
        self.run()

    def on_buffer_edited(self, hook, file):
        # Call when a buffer has been edited

        # We don't care about other buffers
        if not file.name() in (self.ada_file, self.template_file):
            return

        # We're editing the files: run right now
        self.run()

    def open_files(self):
        # Fill up and open the files that are of interest

        # If the files don't exist, create them

        for d in (self.base_dir, self.tmp_dir):
            if not os.path.exists(d):
                os.mkdir(d)

        for filename, contents in (
            (self.ada_file, ADA_FILE_CONTENTS),
            (self.template_file, TEMPLATE_FILE_CONTENTS),
        ):
            if not os.path.exists(filename):
                with open(filename, "w") as f:
                    f.write(contents)

        # Open source editors for both files
        for filename in (self.ada_file, self.template_file):
            GPS.EditorBuffer.get(GPS.File(filename))

        # Split them side by side
        GPS.execute_action("Split horizontally")

    def on_exit(self, command, status, output):
        # Called when gnatpp exits: print output in the console

        # Write something if the return code is nonzero
        if status != 0:
            GPS.Console().write("gnatpp returned {}\n".format(status))

        # Write a blank line then the output
        GPS.Console().write("\n\n{}\n".format(output))

    def run(self):
        # find gnatpp - first, if we're working on the gnatpp project,
        # use it!
        GPS.Console().clear()
        gnatpp = os.path.join(
            GPS.Project.root().exec_dir(),
            "gnatpp" + ("" if "linux" in sys.platform else ".exe"),
        )
        if not os.path.exists(gnatpp):
            gnatpp = os_utils.locate_exec_on_path("gnatpp")

        # Create the contents of the temp dir

        for f in (self.ada_file, self.template_file):
            base = os.path.basename(f)
            tmpfile = os.path.join(self.tmp_dir, base)
            b = GPS.EditorBuffer.get(GPS.File(f))
            with open(tmpfile, "w") as tmp:
                tmp.write(b.get_chars())

        # Run gnatpp

        tmp_templates = os.path.join(self.tmp_dir, os.path.basename(self.template_file))
        tmp_ada_file = os.path.join(self.tmp_dir, os.path.basename(self.ada_file))
        cmdline = [gnatpp, "--templates={}".format(tmp_templates), tmp_ada_file, "--pipe"]
        GPS.Process(
            cmdline,
            on_exit=self.on_exit,
        )

        # Print the command line on the console
        GPS.Console().write("{}\n".format(" ".join(cmdline)))


# This creates the action
@interactive("General", name="gnatpp mode")
def action():
    global GNATPP_MODE

    if GNATPP_MODE is not None:
        # if the action is already active, only re-set up the file editors
        GNATPP_MODE.open_files()
    else:
        # Initialise the action
        GNATPP_MODE = plugin()


# Associate the action to a toolbar button
GPS.Action("gnatpp mode").button(None)
