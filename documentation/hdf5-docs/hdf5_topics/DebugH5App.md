---
title: Debugging HDF5 Applications
redirect_from:
  - https://support.hdfgroup.org/HDF5/doc/H5.user/Debugging.html
---

# Debugging HDF5 Applications

## Introduction
The HDF5 library contains a number of debugging features to make programmers' lives easier including the ability to print detailed error messages, check invariant conditions, display timings and other statistics, and trace API function calls and return values.

### Error Messages
Error messages are normally displayed automatically on the standard error stream and include a stack trace of the library including file names, line numbers, and function names. The application has complete control over how error messages are displayed and can disable the display on a permanent or temporary basis. Refer to the documentation for the H5E error handling package.

### Invariant Conditions
Unless NDEBUG is defined during compiling, the library will include code to verify that invariant conditions have the expected values. When a problem is detected the library will display the file and line number within the library and the invariant condition that failed. A core dump may be generated for post mortem debugging. The code to perform these checks can be included on a per-package bases.

### Timings and Statistics
The library can be configured to accumulate certain statistics about things like cache performance, datatype conversion, data space conversion, and data filters. The code is included on a per-package basis and enabled at runtime by an environment variable.

### API Tracing
All API calls made by an application can be displayed and include formal argument names and actual values and the function return value. This code is also conditionally included at compile time and enabled at runtime.

The statistics and tracing can be displayed on any output stream (including streams opened by the shell) with output from different packages even going to different streams.

## Error Messages
By default any API function that fails will print an error stack to the standard error stream.

```
HDF5-DIAG: Error detected in thread 0.  Back trace follows.
  #000: H5F.c line 1245 in H5Fopen(): unable to open file
    major(04): File interface
    minor(10): Unable to open file
  #001: H5F.c line 846 in H5F_open(): file does not exist
    major(04): File interface
    minor(10): Unable to open file
```

The error handling package (H5E) is described elsewhere.

## Invariant Conditions
To include checks for invariant conditions the library should be configured with --disable-production, the default for versions before 1.2. The library designers have made every attempt to handle error conditions gracefully but an invariant condition assertion may fail in certain cases. The output from a failure usually looks something like this:

```
Assertion failed: H5.c:123: i<NELMTS(H5_debug_g)
IOT Trap, core dumped.
```

## Timings and Statistics
Code to accumulate statistics is included at compile time by using the --enable-debug configure switch. The switch can be followed by an equal sign and a comma-separated list of package names or else a default list is used.<br>

| Name | Default | Description |
| ---- | ------- | ----------- |
| a  | No  | Attributes |
| ac | Yes | Meta data cache |
| b  | Yes | B-Trees |
| d  | Yes | Datasets |
| e  | Yes | Error handling |
| f  | Yes | Files |
| g  | Yes | Groups |
| hg | Yes | Global heap |
| hl | No  | Local heaps |
| i  | Yes | Interface abstraction |
| mf | No  | File memory management |
| mm | Yes | Library memory management |
| o  | No  | Object headers and messages |
| p  | Yes | Property lists |
| s  | Yes | Data spaces |
| t  | Yes | Datatypes |
| v  | Yes | Vectors |
| z  | Yes | Raw data filters |

In addition to including the code at compile time the application must enable each package at runtime. This is done by listing the package names in the `HDF5_DEBUG` environment variable. That variable may also contain file descriptor numbers (the default is `2') which control the output for all following packages up to the next file number. The word `all` refers to all packages. Any word my be preceded by a minus sign to turn debugging off for the package.

### Sample debug specifications

|      |             |
| ---- | ----------- |
| all  | This causes debugging output from all packages to be sent to the standard error stream. |
| all -t -s | Debugging output for all packages except datatypes and data spaces will appear on the standard error stream. |
| -all ac 255 t,s | This disables all debugging even if the default was to debug something, then output from the meta data cache is send to the standard error stream and output from data types and spaces is sent to file descriptor 255 which should be redirected by the shell. |

The components of the `HDF5_DEBUG` value may be separated by any non-lowercase letter.

## API Tracing
The HDF5 library can trace API calls by printing the function name, the argument names and their values, and the return value. Some people like to see lots of output during program execution instead of using a good symbolic debugger, and this feature is intended for their consumption. For example, the output from h5ls foo after turning on tracing, includes:

```
H5Tcopy(type=184549388) = 184549419 (type);
H5Tcopy(type=184549392) = 184549424 (type);
H5Tlock(type=184549424) = SUCCEED;
H5Tcopy(type=184549393) = 184549425 (type);
H5Tlock(type=184549425) = SUCCEED;
H5Fopen(filename="foo", flags=0, access=H5P_DEFAULT) = FAIL;
HDF5-DIAG: Error detected in thread 0.  Back trace follows.
  #000: H5F.c line 1245 in H5Fopen(): unable to open file
    major(04): File interface
    minor(10): Unable to open file
  #001: H5F.c line 846 in H5F_open(): file does not exist
    major(04): File interface
    minor(10): Unable to open file
```


The code that performs the tracing must be included in the library by specifying the --enable-trace configuration switch (the default for versions before 1.2). Then the word trace must appear in the value of the `HDF5_DEBUG` variable. The output will appear on the last file descriptor before the word trace or two (standard error) by default.

To display the trace on the standard error stream:
```
$ env HDF5_DEBUG=trace a.out
```

To send the trace to a file:
```
$ env HDF5_DEBUG="55 trace" a.out 55>trace-output
```

### Performance
If the library was not configured for tracing then there is no unnecessary overhead since all tracing code is excluded. However, if tracing is enabled but not used there is a small penalty. First, code size is larger because of extra statically-declared character strings used to store argument types and names and extra auto variable pointer in each function. Also, execution is slower because each function sets and tests a local variable and each API function calls the H5_trace() function.

If tracing is enabled and turned on then the penalties from the previous paragraph apply plus the time required to format each line of tracing information. There is also an extra call to H5_trace() for each API function to print the return value.

### Safety
The tracing mechanism is invoked for each API function before arguments are checked for validity. If bad arguments are passed to an API function it could result in a segmentation fault. However, the tracing output is line-buffered so all previous output will appear.

### Completeness
There are two API functions that don't participate in tracing. They are H5Eprint() and H5Eprint_cb() because their participation would mess up output during automatic error reporting.

On the other hand, a number of API functions are called during library initialization and they print tracing information.

### Implementation
For those interested in the implementation here is a description. Each API function should have a call to one of the H5TRACE() macros immediately after the FUNC_ENTER() macro. The first argument is the return type encoded as a string. The second argument is the types of all the function arguments encoded as a string. The remaining arguments are the function arguments. This macro was designed to be as terse and unobtrousive as possible.

In order to keep the H5TRACE() calls synchronized with the source code we've written a perl script which gets called automatically just before Makefile dependencies are calculated for the file. However, this only works when one is using GNU make. To reinstrument the tracing explicitly, invoke the trace program from the hdf5 bin directory with the names of the source files that need to be updated. If any file needs to be modified then a backup is created by appending a tilde to the file name.

Explicit Instrumentation:

```
$ ../bin/trace *.c
H5E.c: in function `H5Ewalk_cb':
H5E.c:336: warning: trace info was not inserted
```

**Note:** The warning message is the result of a comment of the form `NO TRACE` somewhere in the function body. Tracing information will not be updated or inserted if such a comment exists.

Error messages have the same format as a compiler so that they can be parsed from program development environments like Emacs. Any function which generates an error will not be modified.
