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
    integer :: ncol
    real(r8), dimension(:,:,:), allocatable :: q
    real(r8), dimension(:,:),   allocatable :: pdeldry
    real(r8), dimension(:,:),   allocatable :: pmid
    real(r8), dimension(:,:),   allocatable :: t
  end type physics_state

  interface physics_state
    module procedure :: constructor
  end interface

contains

  function constructor( number_of_columns, number_of_layers,                  &
      number_of_species ) result( new_obj )
    type(physics_state) :: new_obj
    integer, intent(in) :: number_of_columns
    integer, intent(in) :: number_of_layers
    integer, intent(in) :: number_of_species
    integer :: i_col, i_layer, i_species
    allocate( new_obj%q(       number_of_columns, number_of_layers,           &
                               number_of_species ) )
    allocate( new_obj%pdeldry( number_of_columns, number_of_layers ) )
    allocate( new_obj%pmid(    number_of_columns, number_of_layers ) )
    allocate( new_obj%t(       number_of_columns, number_of_layers ) )
    do i_col = 1, number_of_columns
      do i_layer = 1, number_of_layers
        do i_species = 1, number_of_species
          call set_random_value( new_obj%q( i_col, i_layer, i_species),       &
                                 1.0e-6_r8, 0.05_r8 )
        end do
        call set_random_value( new_obj%pdeldry( i_col, i_layer ),             &
                                25.0_r8, 0.05_r8 )
        call set_random_value( new_obj%pmid(    i_col, i_layer ),             &
                               9.5e5_r8, 0.05_r8 )
        call set_random_value( new_obj%t(       i_col, i_layer ),             &
                               264.2_r8, 0.05_r8 )
      end do
    end do
    new_obj%ncol = number_of_columns
  end function constructor

  !! Set a random value given a mean and a fractional variability
  subroutine set_random_value( random_value, mean, variability )
    real(r8), intent(out) :: random_value
    real(r8), intent(in)  :: mean
    real(r8), intent(in)  :: variability
    call random_number( random_value )
    random_value = mean + ( 0.5_r8 - random_value ) * mean * variability
  end subroutine set_random_value

end module physics_types
