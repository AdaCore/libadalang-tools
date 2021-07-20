from drivers.utils import run, print_nonprintable

run('make')
f = open('foo')
lines = f.readlines()
if 's)' not in lines[0]:
    print('test duration missing')
