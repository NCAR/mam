! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_mode module

!> The mode_t type and related functions
module mam_mode

  use ai_aerosol,                      only : aerosol_t
  use ai_aerosol_state,                only : aerosol_state_t
  use ai_constants,                    only : kDouble
  use mam_species,                     only : species_t

  implicit none
  private

  public :: mode_t, mode_state_t

  !> An aerosol mode
  type, extends(aerosol_t) :: mode_t
    private
    !> Geometric mean diameter [m]
    real(kind=kDouble) :: geometric_mean_diameter__m_
    !> Geometric standard deviation
    real(kind=kDouble) :: geometric_standard_deviation_
    !> Chemical species that can be present in the mode
    type(species_t), allocatable :: species_(:)
  contains
    procedure :: get_new_state
    procedure :: optics_accessor
    procedure :: get_optics
  end type mode_t

  interface mode_t
    module procedure :: constructor
  end interface mode_t

  !> Aerosol mode state
  type, extends(aerosol_state_t) :: mode_state_t
    private
    integer :: number_of_species_ = 0
    real(kind=kDouble), pointer :: number_concentration__mol_m3_
    real(kind=kDouble), pointer :: mass_concentrations__kg_m3_(:)
  contains
    procedure :: raw_size
    procedure :: load_state
    procedure :: dump_state
  end type mode_state_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of mode_t objects
  function constructor( geometric_mean_diameter__m,                           &
      geometric_standard_deviation, chemical_species ) result( new_obj )

    type(mode_t)                   :: new_obj
    real(kind=kDouble), intent(in) :: geometric_mean_diameter__m
    real(kind=kDouble), intent(in) :: geometric_standard_deviation
    type(species_t),    intent(in) :: chemical_species(:)

    new_obj%geometric_mean_diameter__m_   = geometric_mean_diameter__m
    new_obj%geometric_standard_deviation_ = geometric_standard_deviation
    new_obj%species_                      = chemical_species

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a new state object for the mode
  function get_new_state( this ) result( new_state )

    use ai_aerosol_state,              only : aerosol_state_t

    class(aerosol_state_t), pointer    :: new_state
    class(mode_t),          intent(in) :: this

    allocate( mode_state_t :: new_state )
    select type( new_state )
    type is( mode_state_t )
      new_state%number_of_species_ = size( this%species_ )
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
    class(mode_t),     intent(in) :: this
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

  !> Returns a set of optical properties for the mode on a specified grid
  subroutine get_optics( this, optics_accessor, environmental_state,   &
      aerosol_state, optics )

    use ai_accessor,                   only : accessor_t
    use ai_aerosol_state,              only : aerosol_state_t
    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t
    use ai_util,                       only : die_msg
    use mam_optics_accessor,           only : optics_accessor_t

    class(mode_t),                intent(in)    :: this
    class(accessor_t),            intent(in)    :: optics_accessor
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    class(optics_t),              intent(inout) :: optics

    select type( optics_accessor )
    class is( optics_accessor_t )
      select type( aerosol_state )
      class is( mode_state_t )
        call calculate_optics( this, optics_accessor, environmental_state,    &
                               aerosol_state, optics )
      class default
        call die_msg( 623327935, "Invalid aerosol state for MAM modes" )
      end select
    class default
      call die_msg( 118121530, "Invalid accessor type for MAM mode optics" )
    end select

  end subroutine get_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Calculate optical properties for a given state
  subroutine calculate_optics( core, optics_accessor, environmental_state,    &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t
    use mam_optics_accessor,           only : optics_accessor_t

    class(mode_t),                intent(in)    :: core
    class(optics_accessor_t),     intent(in)    :: optics_accessor
    class(environmental_state_t), intent(in)    :: environmental_state
    class(mode_state_t),          intent(in)    :: aerosol_state
    class(optics_t),              intent(inout) :: optics

    optics%values_(:,:) = 1.0

  end subroutine calculate_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! mode_state_t functions
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the number of doubles needed to hold the raw mode state
  integer function raw_size( this )

    class(mode_state_t), intent(in) :: this

    raw_size = 1 + this%number_of_species_

  end function raw_size

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Loads raw mode state data to the mode_state_t object
  subroutine load_state( this, raw_state, index )

    class(mode_state_t),         intent(inout) :: this
    real(kind=kDouble),  target, intent(inout) :: raw_state(:)
    integer,                     intent(inout) :: index

    integer :: last_index

    this%number_concentration__mol_m3_ => raw_state( index )
    index = index + 1
    last_index = index + this%number_of_species_ - 1
    this%mass_concentrations__kg_m3_ => raw_state( index : last_index )
    index = last_index + 1

  end subroutine load_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Dumps the raw mode state data into an double array
  subroutine dump_state( this, raw_state, index )

    class(mode_state_t), intent(inout) :: this
    real(kind=kDouble),  intent(inout) :: raw_state(:)
    integer,             intent(inout) :: index

    integer :: last_index

    this%number_concentration__mol_m3_ => null( )
    index = index + 1
    last_index = index + this%number_of_species_ - 1
    this%mass_concentrations__kg_m3_ => null( )
    index = last_index + 1

  end subroutine dump_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_mode
