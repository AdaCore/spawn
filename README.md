Ada Process API _(spawn)_
=========================================

[![Build](https://github.com/AdaCore/spawn/workflows/Build/badge.svg)](https://github.com/AdaCore/spawn/actions)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/spawn.json)](https://alire.ada.dev/crates/spawn.html)

This library provides simple API to spawn processes and communicate with them.
We provide two implementations of the same API - the integrated into Glib event loop and
the independent.

## Install

### Using alire
Run `alr get --build spawn` or `alr get --build spawn_glib`.

Also you can build from the cloned repository directory with

    alr build

For `spawn_glib` do this in `packages/glib` subdirectory:

    cd packages/glib
    alr build

### Build from sources
Clone repository or unpack source archive, then run

    make all install PREFIX=/path/to/install

## Usage
 1. Add `with "spawn";` or `with "spawn_glib";` to your project file.
 2. Create a process object:
    ```ada
    P : Spawn.Processes.Process;
    ```
 3. Assign program name
    ```ada
    P.Set_Program ("/bin/bash");
    ```
 3. (Optional) Assign command line arguments, working directory, environments variables.
 4. Assign an event listener
    ```ada
    P.Set_Listener (L'Unchecked_Access);
    ```
 5. Start the process:
    ```ada
    P.Start;
    ```
 6. (Glib independent case) With no event loop integration you should drive it
    by you-self periodically calling
    ```ada
    Spawn.Processes.Monitor_Loop (1);
    ```
The listener will get these events:
 * Standard_Output_Available
 * Standard_Error_Available
 * Standard_Input_Available
 * Started
 * Finished
 * Error_Occurred
 * Exception_Occurred

See an example in the [spawn test](testsuite/spawn/spawn_test.adb)

### State diagram
Next diagram shows process states and emited events during its life:

![state diagram](docs/README.png "state diagram")

You should NOT finalize process object until it in `Not_Running` state.

## Maintainer

[AdaCore](https://github.com/AdaCore/)

## Contribute

Feel free to dive in!
[Open an issue](https://github.com/AdaCore/spawn/issues/new)
or submit PRs.

## License

[Apache-2.0](LICENSE) © [AdaCore](https://github.com/AdaCore/)

