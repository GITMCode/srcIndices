# A library for reading/retrieving common indices for ITM models

Currently implemented readers/converters:

| reader   |  indices read    |  indices derived |
| ------   |  :-----------    | :---             |
| init_f107(filename) | f107       | f107a            |
| init_ae(filename)  | ae, au, al | --  |
| init_hpi(filename) |  hpi  | hpin, hpis |
| init_hpi() |  --   | hpi, hpin, hpis *(needs AE already stored)* |
| init_imf(filename) | imfbx, imfby, imfbz, swvx, swvy, swvz, swn, swt | swvmag |

## Quickstart

> ***NOTE:*** If you found this library was cloned automatically by another model, it 
> should compile without any intervention. Just configure the host model with any 
> necessary flags & compile it.

An example program can be found in src/main/main.f90

To run it:

- Compile the library
- Create a run directory
- Run the executable

```bash
git clone git@github.com:GITMCode/srcIndices.git
cd srcIndices
./config.sh --compiler gfortran
make
make rundir
cd run
./io_test.exe
```

After making the run directory, subsequent runs can use the one-liner:

```bash
make clean && make && ./run/io_test.exe
```


---


## Interface

- ModIndices contains the front end
- Read a file with `call init_f107("data/f107.txt")`. Values will be stored unless there is an error.
- Tell `get_index` what time you want. To retrieve values either:
  - Retreve values with `get_index('f107', TIME, returned_value)`, where TIME is real (seconds since Jan 1 1965)
  - Tell the library the time with `set_time()` and then `get_index('f107', returned_value)`

`get_index()` can accept the string-name of an index (defined in [`ModIndices`](src/ModIndices.f90)) or the corresponding integer "index ID". Conversions between the two are handled automatically by `decode_index()`.

If there are any errors, `isOk=.false.`. Errors and warnings can be printed with `report_[warnings/errors]`.

## Time

This library can store & convert times to make subsequent calls easier. 

The interface `set_time()` dispatches the subroutines in `time_subroutines.inc`. Time can be set with:
- [`TimeType`](src/ModExtras.F90#L188)
- real: (sec since Jan 1, 1965)
- components: (year, month, day, hour, minute, second)
- Day-of-year conversion is handled automatically by `TimeType`
  - For DOY, provide (year, month=1, DOY, ...)
  - If month =/= 1 and day>31, there will be errors 

`TimeType` is borrowed from the SWMF and contains:

```fortran
  type TimeType
     integer           :: iYear
     integer           :: iMonth
     integer           :: iDay
     integer           :: iHour
     integer           :: iMinute
     integer           :: iSecond
     real(Real8_)      :: FracSecond
     real(Real8_)      :: Time         ! time in seconds since Jan 1 1965
     character(len=14) :: String       ! string with YYYY-MM-DD HH:MM:SS.mm
  end type TimeType
```

----

## Configuring & Compiling within other models

### How it works

The build system requires `build/Makefile.local` to exist before compiling. It contains two variables:

    DIRSFILE := /path/to/Makefile.dirs    # directory/path variables
    BUILDDIR  := /path/to/build/dir       # directory also searched for Makefile.conf

`Makefile.conf` holds compiler flags and suffix rules.

For standalone builds all three files live in this repo's `build/` directory. For coupled builds, `DIRSFILE` and `BUILDDIR` point into the host model's tree so that the host model's compiler settings are used.

### How to couple to a new host model

Three steps in your host model's configure script:

1. Write `build/Makefile.local` pointing to your build config:

```make
    BUILDDIR  := /path/to/your/build/dir       # must contain Makefile.conf
    DIRSFILE  := /path/to/your/Makefile.dirs   # path/directory variables
```

2. Touch `src/Makefile.DEPEND` so make can include it:

```bash
    touch /path/to/ext/srcIndices/src/Makefile.DEPEND
```

3. Build the library:

```bash
    cd /path/to/ext/srcIndices && make LIB
```
### PreProc flags for coupling

Several modules (`ModKind`, `ModIoUnit`, `ModErrors`, `ModTimeConvert`) may already be provided by a host model. To avoid conflicts, [`ModExtras.F90`](src/ModExtras.F90) wraps each module with pre-processor guards ensuring they're only compiled when needed.

| Flag | Module provided |
| ---- | :-------------- |
| `STANDALONE` | All modules below |
| `NEEDMODKIND` | `ModKind` |
| `NEEDMODIOUNIT` | `ModIoUnit` |
| `NEEDMODERRORS` | `ModErrors` |
| `NEEDMODTIMECONVERT` | `ModTimeConvert` |

Flags are passed through the `PreProc` variable in `Makefile.conf`. When building standalone, [`build/Makefile.conf.gfortran`](build/Makefile.conf.gfortran) sets `PreProc = -DSTANDALONE` and all modules are compiled. When coupled, the host model's compiler config is used instead. If no flags are set, nothing in `ModExtras` is compiled.


