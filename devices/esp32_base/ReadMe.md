Irrigation controller written in Micropython built on the Aiko framework.

- http://micropython.org
- https://github.com/geekscape/aiko_engine_mp

This controller acts on irrigation commands posted to the message broker.

The Aiko Readme details how to load this code onto a compatible microcontroller.


--------------------------------------------------------------

To flash:

```
nix-shell
export AMPY_PORT=<esp32 port, e.g. /dev/ttyUSB0>
./scripts/flash_micropython.sh
./scripts/mpf_script.sh ./scripts/aiko.mpf
```

If adding any files, be sure to add them to `./scripts/aiko.mpf` so they are included in the flashing.

Update single file via
```
mpfshell
> put path/to/updated_file.py
```

Inspect loaded files via
```
nix-shell
mpfshell -o <esp32 port without /dev/ path prefix e.g. ttyUSB0>
> get|put|cat|help|...
```

Soft reboot in `mpfshell` by running `repl` and pressing ctrl+d 

Exit `mpfshell` repl by pressing ctrl+]
