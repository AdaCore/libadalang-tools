from pathlib import Path
import os
import shutil
import tempfile

from drivers.base_driver import BaseDriver


class GNATtestDriver(BaseDriver):
    """
    GNATtest and TGen Test Driver.

    Interface:
        Same as `ShellScriptDriver`.

    This driver is specific to GNATtest and behaves like `ShellScriptDriver`,
    except that it copies the TGen runtime support project to a temporary
    directory and adds this copy to `GPR_PROJECT_PATH`.

    This approach prevents race conditions when GNATtest builds harnesses
    that depend on the TGen runtime support library. During the build process,
    `gprbuild` overwrites the static library artifact (`libtgen.a`) each time
    it is invoked. To avoid these conflicts, each test run operates on its own
    isolated copy of the `tgen_rts` project.
    """

    DEFAULT_TGEN_RTS_LOCATION: Path = Path("src/tgen/tgen_rts").absolute()

    def run(self) -> None:
        tgen_rts_project_path = os.environ.get(
            "TGEN_RTS_LOCATION", str(GNATtestDriver.DEFAULT_TGEN_RTS_LOCATION)
        )
        gpr_project_path = os.environ.get("GPR_PROJECT_PATH", "")

        temp_dir = Path(tempfile.mkdtemp()) / "tgen_rts"
        shutil.copytree(tgen_rts_project_path, temp_dir)

        process_result = self.shell(
            ["bash", "test.sh"],
            catch_error=False,
            env={**os.environ, "GPR_PROJECT_PATH": f"{temp_dir}:{gpr_project_path}"},
        )

        if process_result.status:
            self.output += ">>>program returned status code {}\n".format(
                process_result.status
            )
