# statprof

Gambit library for statistical profiling

## Description

A statistical profiler is a program that will take snapshots of the state of another program at regular intervals to get information about where that program is spending its time.

This particular statistical profiler works by using Gambit's interrupt handler to identify where the code was interrupted.  It should not affect a program's performance by much.  It will work on multiple files.

In the default profile kind, an HTML version of the source code is generated with colors added to indicate how often the code was interrupted in particular places, giving an idea of where time is spent in the code.

When looking at the output, you can see in the first column how often the code was interrupted at that point and the second column is the line number.  Lines with high interrupt counts are redish in color.


## Usage

The statprof library exports the following three procedures:

(statprof-start! [&lt;kind&gt;])
  -- Begin the profiling.  The optional &lt;kind&gt; parameter is a list
     indicating the kind of profiles to generate.  The list can contain
     the symbols `profile` (the default HTML profiles) and `flamegraph`
     (which requires post-processing of the data with the  script
     `flamegraph.pl` see https://github.com/brendangregg/FlameGraph).

(statprof-stop!)
  -- End the profiling

(statprof-write! &lt;profile-dir&gt;)
  -- Write the profiling results to the directory &lt;profile-dir&gt; (relative to the current directory).  To view a profile generated with kind=`profile` open the file &lt;profile-dir&gt;/index.html .  The profile generated with kind=`flamegraph` is in &lt;profile-dir&gt;/flamegraph.folded .

See the file demo.sld for a simple demo of how to use this library.  The demo can be run with the command

    gsi github.com/gambit/statprof/demo

## License

This code is released under the same license as Gambit itself.

## Authors

Written by Guillaume Germain, with some code from Marc Feeley.
Adapted to Gambit's module system by Marc Feeley.
