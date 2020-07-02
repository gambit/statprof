# statprof

Gambit library for statistical profiling

## Description

A statistical profiler is a program that will take snapshots of the state of another program at regular intervals to get information about where that program is spending its time.

This particular statistical profiler works by using Gambit's interrupt handler to identify where the code was interrupted.

It generates an HTML version of the source code with colors added to indicate how often the code was interrupted in particular places, giving an idea of where time is spent in the code.

It should not affect a program's performance by much.  It will work on multiple files.

When looking at the output, you can see a first column with the line number, then the second column is how many times the code was interrupted at that point out of how many interruptions.


## Usage

You use it with the following three functions:

(profile-start!)
  -- begin the profiling

(profile-stop!)
  -- end the profiling

(write-profile-report <profile-dir>)
  -- output the data (annotated source) in HTML files in a
     <profile-dir> sub-directory created in the current directory.


See the file demo.sld for a simple demo of how to use this library.

## License

This code is released under the same license as Gambit itself.

## Authors

Written by Guillaume Germain, with some code from Marc Feeley.
Adapted to Gambit's module system by Marc Feeley.
