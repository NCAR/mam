! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module cam_history
  implicit none
  private
  public :: addfld, outfld, add_default
  integer, parameter, public :: fieldname_len = 24
  character(len=10), parameter, public :: horiz_only = "horiz_only"
  interface addfld
    module procedure :: addfld_1d
    module procedure :: addfld_nd
  end interface
contains
  subroutine addfld_1d(fname, vdim_name, avgflag, units, long_name,           &
       gridname, flag_xyfill, sampling_seq, standard_name, fill_value)
    use shr_kind_mod,                  only : r8 => shr_kind_r8
    character(len=*), intent(in)  :: fname
    character(len=*), intent(in)  :: vdim_name
    character(len=1), intent(in)  :: avgflag
    character(len=*), intent(in)  :: units
    character(len=*), intent(in)  :: long_name

    character(len=*), intent(in), optional :: gridname
    logical, intent(in), optional :: flag_xyfill
    character(len=*), intent(in), optional :: sampling_seq
    character(len=*), intent(in), optional :: standard_name
    real(r8),         intent(in), optional :: fill_value
  end subroutine addfld_1d
  subroutine addfld_nd(fname, dimnames, avgflag, units, long_name,            &
       gridname, flag_xyfill, sampling_seq, standard_name, fill_value)
    use shr_kind_mod,                  only : r8 => shr_kind_r8
    character(len=*), intent(in)  :: fname
    character(len=*), intent(in)  :: dimnames(:)
    character(len=1), intent(in)  :: avgflag
    character(len=*), intent(in)  :: units
    character(len=*), intent(in)  :: long_name
    character(len=*), intent(in), optional :: gridname
    logical, intent(in), optional :: flag_xyfill
    character(len=*), intent(in), optional :: sampling_seq
    character(len=*), intent(in), optional :: standard_name
    real(r8),         intent(in), optional :: fill_value
  end subroutine addfld_nd
  subroutine outfld(fname, field, idim, c, avg_subcol_field)
    use shr_kind_mod,                  only : r8 => shr_kind_r8
    character(len=*), intent(in)  :: fname
    integer, intent(in)           :: idim
    real(r8), intent(in)          :: field(idim,*)
    integer, intent(in)           :: c
    logical, optional, intent(in) :: avg_subcol_field
  end subroutine outfld
  subroutine add_default(name, tindex, flag)
    character(len=*), intent(in) :: name
    integer,          intent(in) :: tindex
    character(len=1), intent(in) :: flag
  end subroutine add_default
end module cam_history
