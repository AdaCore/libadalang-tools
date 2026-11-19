from pathlib import Path
import subprocess

source = Path("routes-convert.adb")
subprocess.run(["gnatpp", "-j1", source], stdout=subprocess.DEVNULL, check=True)
print(source.read_text(), end="")
