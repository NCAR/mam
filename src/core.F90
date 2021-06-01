! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_core module

!> The core_t type and related functions
module mam_core

  use ai_aerosol,                      only : aerosol_t
  use mam_mode,                        only : mode_t

  implicit none
  private

  public :: core_t

  !> The Modal Aerosol Model core
  type, extends(aerosol_t) :: core_t
    private
    type(mode_t), allocatable :: modes_(:)
  contains
    procedure :: optics_accessor
    procedure :: get_optics
  end type core_t

  interface core_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of the MAM core
  function constructor( number_of_columns, number_of_layers )                 &
      result( new_obj )

    use ai_constants,                  only : r8 => kDouble
    use mam_species,                   only : species_t

    type(core_t), pointer :: new_obj
    integer, intent(in) :: number_of_columns
    integer, intent(in) :: number_of_layers

    type(species_t) :: species(3)

    allocate( new_obj )

    ! Get current MAM 4 configuration

    species( 1 ) = species_t( "sulfate" )
    species( 2 ) = species_t( "nitrate" )
    species( 3 ) = species_t( "ammonia" )

    allocate( new_obj%modes_( 4 ) )

    new_obj%modes_( 1 ) = mode_t( 8e-8_r8, 2.1_r8, species, number_of_columns,&
                                  number_of_layers )
    new_obj%modes_( 2 ) = mode_t( 9e-8_r8, 2.0_r8, species, number_of_columns,&
                                  number_of_layers )
    new_obj%modes_( 3 ) = mode_t( 1e-7_r8, 1.9_r8, species, number_of_columns,&
                                  number_of_layers )
    new_obj%modes_( 4 ) = mode_t( 2e-7_r8, 1.8_r8, species, number_of_columns,&
                                  number_of_layers )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Provides an accessor for the specified optics
  function optics_accessor( this, optics )

    use ai_accessor,                   only : accessor_t
    use ai_optics,                     only : optics_t
    use mam_optics_accessor,           only : optics_accessor_t

    class(accessor_t), pointer    :: optics_accessor
    class(core_t),     intent(in) :: this
    class(optics_t),   intent(in) :: optics

    optics_accessor => optics_accessor_t( optics )

  end function optics_accessor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Get a set of optical properties on a specified grid
  subroutine get_optics( this, optics_accessor, environmental_state, optics )

    use ai_accessor,                   only : accessor_t
    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t
    use ai_util,                       only : die_msg
    use mam_optics_accessor,           only : optics_accessor_t

    class(core_t),                intent(in)    :: this
    class(accessor_t),            intent(in)    :: optics_accessor
    class(environmental_state_t), intent(in)    :: environmental_state
    class(optics_t),              intent(inout) :: optics

    select type( optics_accessor )
    class is( optics_accessor_t )
      call optics_accessor%calculate( this%modes_, environmental_state,       &
                                      optics )
    class default
      call die_msg( 874529081, "Invalid accessor type for optics" )
    end select

  end subroutine get_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_core
