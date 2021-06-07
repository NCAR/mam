! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module spmd_utils
  use shr_kind_mod
  implicit none
  private
  public :: mpi_sum
  logical, parameter, public :: masterproc = .true.
  integer, parameter, public :: mpi_integer = shr_kind_in
  integer, parameter, public :: mpi_integer8 = shr_kind_i8
  integer, parameter, public :: mpi_max = 10
  integer,            public :: iam    = 0
  integer,            public :: mpicom = 0
  integer,            public :: npes   = 1
contains
  subroutine mpi_sum()

  end subroutine mpi_sum
end module spmd_utils
