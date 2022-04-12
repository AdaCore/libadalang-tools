# gnatpp partil formatting experiments

Initial interesting cases to run on the debugger:
```
# This one works. Indentation seems to be relative to the declare own indentation. So we would need an offset here.
run -S ./src/partial_gnatpp.adb -SL 156 -SC 10

# raises PP.FORMATTING.TOKEN_MISMATCH
run -S ./src/partial_gnatpp.adb -SL 155 -SC 12

# Raises an exception in pp-actions.adb:4215
# this code assumes that ancentor nodes that in partial formatting might not exist
run -S ./src/partial_gnatpp.adb -SL 157 -SC 19
```
