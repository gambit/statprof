# statprof

Gambit library for statistical profiling

## Description

A statistical profiler is a program that will take snapshots of the state of another program at regular intervals to get information about where that program is spending its time.

This particular statistical profiler works by using Gambit's interrupt handler to identify where the code was interrupted.

It generates an HTML version of the source code with colors added to indicate how often the code was interrupted in particular places, giving an idea of where time is spent in the code.

It should not affect a program's performance by much.  It will work on multiple files.

When looking at the output, you can see a first column with the line number, then the second column is how many times the code was interrupted at that point out of how many interruptions.


## Usage

The statprof library exports the following three procedures:

(profile-start!)
  -- Begin the profiling

(profile-stop!)
  -- End the profiling

(write-profile-report &lt;profile-dir&gt;)
  -- Write the profiling results in HTML files in the directory &lt;profile-dir&gt; (relative to the current directory).  To view the results open the file &lt;profile-dir&gt;/index.html .

See the file demo.sld for a simple demo of how to use this library.  The demo can be run with the command

    gsi github.com/gambit/statprof/demo

## License

This code is released under the same license as Gambit itself.

## Authors

Written by Guillaume Germain, with some code from Marc Feeley.
Adapted to Gambit's module system by Marc Feeley.
