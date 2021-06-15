! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Utility functions for MAM4 tests
module test_utils

  use shr_kind_mod,                    only : r8 => shr_kind_r8

  implicit none
  private

  public :: set_values

  interface set_values
    module procedure :: set_values_1d_real
    module procedure :: set_values_2d_real
    module procedure :: set_values_3d_real
  end interface

contains

  !> Sets random values given a mean and a fractional variability
  subroutine set_values_1d_real( array, mean, variability )
    implicit none
    real(r8), intent(out), dimension(:) :: array
    real(r8), intent(in )               :: mean
    real(r8), intent(in )               :: variability
    integer :: i_elem
    do i_elem = 1, size( array )
      call random_number( array( i_elem ) )
      array( i_elem ) = mean + ( 0.5_r8 - array( i_elem ) ) * mean            &
                        * variability
    end do
  end subroutine set_values_1d_real

  !> Sets random values given a mean and a fractional variability
  subroutine set_values_2d_real( array, mean, variability )
    implicit none
    real(r8), intent(out), dimension(:,:) :: array
    real(r8), intent(in )                 :: mean
    real(r8), intent(in )                 :: variability
    integer :: i_elem
    do i_elem = 1, size( array, 2 )
      call set_values_1d_real( array( :, i_elem ), mean, variability )
    end do
  end subroutine set_values_2d_real

  !> Sets random values given a mean and a fractional variability
  subroutine set_values_3d_real( array, mean, variability )
    implicit none
    real(r8), intent(out), dimension(:,:,:) :: array
    real(r8), intent(in )                   :: mean
    real(r8), intent(in )                   :: variability
    integer :: i_elem
    do i_elem = 1, size( array, 3 )
      call set_values_2d_real( array( :, :, i_elem ), mean, variability )
    end do
  end subroutine set_values_3d_real

end module test_utils
