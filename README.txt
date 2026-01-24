In this directory, run

  make

to build and

  ./crash.exe

to execute. You should see a segfault almost immediately.

There are at least 4 ways to make the crash go away:

1. Disable line [1].
2. Disable line [2].
3. Move line [3] one down, after init_window.
4. Disable line [4].
