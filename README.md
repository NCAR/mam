# mam
A modal aerosol model

## Development Notes ##

| Goal | Notes | Fortran limitations |
|------|-------|---------------------|
| Data hiding & one class per file | Data hiding is critical to writing modular code. One class per file is useful for keeping code organized and readable | No protected class members. No friend classes. Submodules might be useful in at least moving the function definitions to separate files. |
| Remove grid information from aerosol modules | Host module should be able to allocate resources for multiple aerosol states (e.g., per grid cell) and have these be contiguous in memory without an aerosol needing to be aware of the grid shape and size. | Less control of memory allocation and pointer assignment than in c/c++. Parameterized derived types could be useful for this, but a [bug in gfortran](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=82943) prevents these from having type-bound functions. |


