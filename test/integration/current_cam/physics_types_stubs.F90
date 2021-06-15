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
    real(r8), dimension(:,:,:), allocatable :: q       ! constituent mixing ratio [mol mol-1] (wet or dry)
    real(r8), dimension(:,:),   allocatable :: pdeldry ! layer thickness [Pa]
    real(r8), dimension(:,:),   allocatable :: pmid    ! midpoint pressure [Pa]
    real(r8), dimension(:,:),   allocatable :: t       ! temperature [K]
  end type physics_state

  interface physics_state
    module procedure :: constructor
  end interface

contains

  function constructor( number_of_columns, number_of_layers,                  &
      number_of_species ) result( new_obj )
    use test_utils,                    only : set_values
    implicit none
    type(physics_state) :: new_obj
    integer, intent(in) :: number_of_columns
    integer, intent(in) :: number_of_layers
    integer, intent(in) :: number_of_species
    allocate( new_obj%q(       number_of_columns, number_of_layers,           &
                               number_of_species ) )
    allocate( new_obj%pdeldry( number_of_columns, number_of_layers ) )
    allocate( new_obj%pmid(    number_of_columns, number_of_layers ) )
    allocate( new_obj%t(       number_of_columns, number_of_layers ) )
    call set_values( new_obj%q,       1.0e-6_r8, 0.05_r8 )
    call set_values( new_obj%pdeldry,   25.0_r8, 0.05_r8 )
    call set_values( new_obj%pmid,     9.5e5_r8, 0.05_r8 )
    call set_values( new_obj%t,        264.2_r8, 0.05_r8 )
    new_obj%ncol = number_of_columns
  end function constructor

end module physics_types
