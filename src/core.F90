! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_core module

!> The core_t type and related functions
module mam_core

  use ai_aerosol,                      only : aerosol_t
  use ai_aerosol_state,                only : aerosol_state_t
  use mam_mode,                        only : mode_t, mode_state_t

  implicit none
  private

  public :: core_t, state_t

  !> The Modal Aerosol Model core
  type, extends(aerosol_t) :: core_t
    private
    type(mode_t), allocatable :: modes_(:)
  contains
    procedure :: get_new_state
    procedure :: optics_accessor
    procedure :: get_optics
  end type core_t

  interface core_t
    module procedure :: constructor
  end interface

  !> Modal aerosol state
  type, extends(aerosol_state_t) :: state_t
    private
    type(mode_state_t), allocatable :: mode_states_(:)
  contains
    procedure :: raw_size
    procedure :: load_state
    procedure :: dump_state
  end type state_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of the MAM core
  function constructor( ) result( new_obj )

    use ai_constants,                  only : r8 => kDouble
    use mam_species,                   only : species_t

    type(core_t), pointer :: new_obj

    type(species_t) :: species(3)

    allocate( new_obj )

    ! Get current MAM 4 configuration

    species( 1 ) = species_t( "sulfate" )
    species( 2 ) = species_t( "nitrate" )
    species( 3 ) = species_t( "ammonia" )

    allocate( new_obj%modes_( 4 ) )

    new_obj%modes_( 1 ) = mode_t( 8e-8_r8, 2.1_r8, species )
    new_obj%modes_( 2 ) = mode_t( 9e-8_r8, 2.0_r8, species )
    new_obj%modes_( 3 ) = mode_t( 1e-7_r8, 1.9_r8, species )
    new_obj%modes_( 4 ) = mode_t( 2e-7_r8, 1.8_r8, species )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a new state object for the modal aerosol
  function get_new_state( this ) result( new_state )

    class(aerosol_state_t), pointer    :: new_state
    class(core_t),          intent(in) :: this

    class(aerosol_state_t), pointer :: mode_state
    integer :: i_mode

    allocate( state_t :: new_state )
    select type( new_state )
    type is( state_t )
      allocate( new_state%mode_states_( size( this%modes_ ) ) )
      do i_mode = 1, size( this%modes_ )
        mode_state => this%modes_( i_mode )%get_new_state( )
        select type( mode_state )
        class is( mode_state_t )
          new_state%mode_states_( i_mode ) = mode_state
        end select
        deallocate( mode_state )
      end do
    end select

  end function get_new_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Provides an accessor for the specified optics
  function optics_accessor( this, optics )

    use ai_accessor,                   only : accessor_t
    use ai_optics,                     only : optics_t
    use ai_wavelength_grid,            only : wavelength_grid_t, kWavenumber, &
                                              kCentimeter
    use mam_optics_accessor,           only : optics_accessor_t
    use mam_optics_constants,          only : shortwave_lower, longwave_lower,&
                                              shortwave_upper, longwave_upper

    class(accessor_t), pointer    :: optics_accessor
    class(core_t),     intent(in) :: this
    class(optics_t),   intent(in) :: optics

    type(wavelength_grid_t) :: shortwave, longwave

    shortwave = wavelength_grid_t( lower_bounds = shortwave_lower,           &
                                   upper_bounds = shortwave_upper,           &
                                   bounds_in    = kWavenumber,               &
                                   base_unit    = kCentimeter )

    longwave  = wavelength_grid_t( lower_bounds = longwave_lower,            &
                                   upper_bounds = longwave_upper,            &
                                   bounds_in    = kWavenumber,               &
                                   base_unit    = kCentimeter )

    optics_accessor => optics_accessor_t( shortwave, longwave, optics )

  end function optics_accessor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns a set of optical properties for the aerosol on a specified grid
  subroutine get_optics( this, optics_accessor, environmental_state,          &
      aerosol_state, optics )

    use ai_accessor,                   only : accessor_t
    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t
    use ai_util,                       only : die_msg
    use mam_optics_accessor,           only : optics_accessor_t

    class(core_t),                intent(in)    :: this
    class(accessor_t),            intent(in)    :: optics_accessor
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    class(optics_t),              intent(inout) :: optics

    select type( optics_accessor )
    class is( optics_accessor_t )
      select type( aerosol_state )
      class is( state_t )
        call calculate_optics( this, optics_accessor, environmental_state,    &
                               aerosol_state, optics )
      class default
        call die_msg( 522911428, "Invalid aerosol state for MAM aerosol" )
      end select
    class default
      call die_msg( 577391214, "Invalid accessor type for MAM optics" )
    end select

  end subroutine get_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate optical properties for a given state
  subroutine calculate_optics( core, optics_accessor, environmental_state,    &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t
    use mam_optics_accessor,           only : optics_accessor_t

    class(core_t),                intent(in)    :: core
    class(optics_accessor_t),     intent(in)    :: optics_accessor
    class(environmental_state_t), intent(in)    :: environmental_state
    class(state_t),               intent(in)    :: aerosol_state
    class(optics_t),              intent(inout) :: optics

    type(optics_t) :: mode_optics
    integer :: i_mode

    mode_optics = optics
    optics%values_(:,:) = 0.0
    do i_mode = 1, size( core%modes_ )
    associate( mode_state => aerosol_state%mode_states_( i_mode ) )
      mode_optics%values_(:,:) = 0.0
      call core%modes_( i_mode )%get_optics( optics_accessor,                 &
                                             environmental_state,             &
                                             mode_state,                      &
                                             mode_optics )
      optics%values_(:,:) = optics%values_(:,:) + mode_optics%values_(:,:)
    end associate
    end do

  end subroutine calculate_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! state_t functions
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of doubles needed to hold the raw MAM aerosol state
  integer function raw_size( this )

    class(state_t), intent(in) :: this

    integer :: i_mode

    raw_size = 0
    do i_mode = 1, size( this%mode_states_ )
      raw_size = raw_size + this%mode_states_( i_mode )%raw_size( )
    end do

  end function raw_size

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Loads raw MAM state data to the state_t object
  subroutine load_state( this, raw_state, index )

    use ai_constants,                  only : kDouble

    class(state_t),             intent(inout) :: this
    real(kind=kDouble), target, intent(inout) :: raw_state(:)
    integer,                    intent(inout) :: index

    integer :: i_mode

    do i_mode = 1, size( this%mode_states_ )
      call this%mode_states_( i_mode )%load_state( raw_state, index )
    end do

  end subroutine load_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Dumps the raw MAM state data to an array
  subroutine dump_state( this, raw_state, index )

    use ai_constants,                  only : kDouble

    class(state_t),     intent(inout) :: this
    real(kind=kDouble), intent(inout) :: raw_state(:)
    integer,            intent(inout) :: index

    integer :: i_mode

    do i_mode = 1, size( this%mode_states_ )
      call this%mode_states_( i_mode )%dump_state( raw_state, index )
    end do

  end subroutine dump_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_core
