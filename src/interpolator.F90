! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_interpolator module

!> The interpolator_t type and related functions
module mam_interpolator

  use ai_constants,                    only : kDouble

  implicit none
  private

  public :: interpolator_t

  type :: map_element_t
    integer            :: from_index_
    integer            :: to_index_
    real(kind=kDouble) :: weight_
  end type map_element_t

  !> Interpolator between wavelength grids
  type :: interpolator_t
    private
    type(map_element_t), allocatable :: map_(:)
  contains
    procedure :: interpolate
  end type interpolator_t

  interface interpolator_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructs an interpolator_t for a given pair of wavelength grids
  function constructor( from_grid, to_grid ) result( new_obj )

    use ai_wavelength_grid,            only : wavelength_grid_t

    type(interpolator_t) :: new_obj
    class(wavelength_grid_t), intent(in) :: from_grid
    class(wavelength_grid_t), intent(in) :: to_grid

    ! look up interpolation scheme used in MAM
    allocate( new_obj%map_( 0 ) )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Interpolate between grids
  subroutine interpolate( this, from, to )

    class(interpolator_t), intent(in)    :: this
    real(kind=kDouble),    intent(in)    :: from(:)
    real(kind=kDouble),    intent(inout) :: to(:)

    integer :: i_map

    do i_map = 1, size( this%map_ )
      to( this%map_( i_map )%to_index_ ) =                                    &
          from( this%map_( i_map )%from_index_ ) *                            &
          this%map_( i_map )%weight_
    end do

  end subroutine interpolate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_interpolator
