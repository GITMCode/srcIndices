# A library for reading/retrieving common indices for ITM models

Currently implemented readers/converters:

| reader   |  indices read    |  indices derived |
| ------   |  :-----------    | :---             |
| init_f107(filename) | f107       | f107a            |
| init_ae(filename)  | ae, au, al | --  |
| init_hpi(filename) |  *not implemented yet*  | -- |
| init_hpi() |  --   | hpi, hpin, hpis *(needs AE already stored)* |
| init_imf(filename) | imfbx, imfby, imfbz, swvx, swvy, swvz, swn, swt | swvmag |

## Quickstart

An example program can be found in src/main/main.f90

To run it:

- Compile the library
- Create a run directory
- Run the executable

```
git clone git@github.com:GITMCode/srcIndices.git
cd srcIndices
make
make rundir
cd run
./io_test.exe
```

After making the run directory, subsequent runs can use the one-liner:

```
make cleanall && make && ./run/io_text.exe
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
- [`TimeType`](src/ModTime.f90)
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

## Building & Coupling

Several modules used by this library (`ModKind`, `ModIoUnit`, `ModErrors`, `ModTimeConvert`) may already be provided by a host model. To avoid conflicts, [`ModExtras.F90`](src/ModExtras.F90) wraps each module with pre-processor guards ensuring they're only compiled when needed.

| Flag | Module provided |
| ---- | :-------------- |
| `STANDALONE` | All modules below |
| `NEEDMODKIND` | `ModKind` |
| `NEEDMODIOUNIT` | `ModIoUnit` |
| `NEEDMODERRORS` | `ModErrors` |
| `NEEDMODTIMECONVERT` | `ModTimeConvert` |

Flags are passed through the `PreProc` variable in `Makefile.def`. When building standalone, [`build/Makefile.def`](build/Makefile.def) sets `PreProc = -DSTANDALONE` and all modules are compiled. When coupled, the host model's `Makefile.def` replaces this file. If no flags are set, nothing in `ModExtras` is compiled.

