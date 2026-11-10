from pathlib import Path
import subprocess


source = Path("test.adb").read_bytes()
target = Path("formatted.adb")

for name, options in (("default", []), ("brackets", ["-Wb"]), ("UTF-8", ["-W8"])):
    target.write_bytes(source.rstrip(b"\n"))
    command = ["gnatpp", "-j1", *options, str(target)]
    first = subprocess.run(command, capture_output=True, check=True)
    assert not first.stderr, first.stderr.decode()
    formatted = target.read_bytes()

    if name == "default":
        reference = formatted
        diagnostics = first.stdout
        print(diagnostics.decode(), end="")
        print(formatted.decode(), end="")
        assert formatted != source
    else:
        assert formatted == reference, name
        assert first.stdout == diagnostics, first.stdout.decode()

    second = subprocess.run(command, capture_output=True, check=True)
    assert not second.stderr, second.stderr.decode()
    assert second.stdout == diagnostics, second.stdout.decode()
    assert target.read_bytes() == formatted, name
    print(f"{name}: identical formatting, second pass unchanged")
