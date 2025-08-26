# statprof

Gambit library for statistical profiling

## Description

A statistical profiler takes snapshots of the state of a program at regular intervals to get information about where that program is spending its time.

This particular statistical profiler works by using Gambit's heartbeat interrupt to identify where the code was interrupted.  It should not affect a program's performance by much.  It will work on multiple files.

In the default profile kind, an HTML version of the source code is generated with colors added to indicate how often the code was interrupted in particular places, giving an idea of where time is spent in the code.

When looking at the output, you can see in the first column how often the code was interrupted at that point and the second column is the line number.  Lines with high interrupt counts are redish in color.

The profiler can also generate flamegraphs which offer a different view of the program's activities (see https://github.com/brendangregg/FlameGraph).

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

(statprof-write! [&lt;profile-dir&gt;])
  -- Write the profiling results to the directory &lt;profile-dir&gt; (relative to the initial current directory).  The default &lt;profile-dir&gt; is &lt;command-name&gt;`.statprof` for scripts and compiled programs, otherwise `statprof.statprof`. To view a profile generated with kind=`profile` open the file &lt;profile-dir&gt;`/index.html` .  The profile generated with kind=`flamegraph` is in &lt;profile-dir&gt;`/flamegraph.folded` and it should be post-processed with `flamegraph.pl` &lt;profile-dir&gt;`/flamegraph.folded` to generate a SVG viewable with a web browser.

See the file demo.sld for a simple demo of how to use this library.  The demo can be run with the command

    gsi github.com/gambit/statprof/demo

In addition to the main `statprof` library are two utility libraries: `github.com/gambit/statprof/profile` and `github.com/gambit/statprof/flamegraph`. Linking with these libraries will automatically profile the program, either with a statistical profile or a flamegraph. For example the program `github.com/feeley/fib` executed by the interpreter can be profiled with these commands:

    gsi github.com/gambit/statprof/profile github.com/feeley/fib    # statistical profile
    gsi github.com/gambit/statprof/flamegraph github.com/feeley/fib # flamegraph
    ./flamegraph.pl statprof.statprof/flamegraph.folded > statprof.statprof/flamegraph.html
    open statprof.statprof/index.html statprof.statprof/flamegraph.html

This feature can also be used to profile the Gambit compiler by executing an `import` of the appropriate library from the command-line. For example the following will generate a flamegraph of the compiler compiling SRFI-1:

    gsc -c -e "(import github.com/gambit/statprof/flamegraph)" lib/srfi/1/1.scm

## License

This code is released under the same license as Gambit itself.

## Authors

Written by Guillaume Germain, with some code from Marc Feeley.
Adapted to Gambit's module system by Marc Feeley.
