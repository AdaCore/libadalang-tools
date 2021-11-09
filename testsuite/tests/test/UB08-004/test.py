from drivers.utils import run

run('gnattest -P simple.gpr -q --harness-dir=h')
run('gnattest -P simple2.gpr -q --harness-dir=h2'
    ' --target=x86_64-linux --RTS=default')
f = open('h/gnattest_common.gpr')
for line in f:
    if 'for Target' in line:
        print(line.rstrip())
    if 'for Runtime' in line:
        print(line.rstrip())
f2 = open('h2/gnattest_common.gpr')
for line in f2:
    if 'for Target' in line:
        print(line.rstrip())
    if 'for Runtime' in line:
        print(line.rstrip())
