! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module physics_types
  use shr_kind_mod,                    only : r8 => shr_kind_r8
  implicit none
  private
  type, public :: physics_state
    integer :: lchnk = 1
    integer :: ncol = 45
    real(r8), dimension(:,:,:), allocatable :: q
    real(r8), dimension(:,:),   allocatable :: pdeldry
    real(r8), dimension(:,:),   allocatable :: pmid
    real(r8), dimension(:,:),   allocatable :: t
  end type physics_state
end module physics_types
