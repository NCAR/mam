! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Mock version of mode_t

!> Support module for mam tests using mode_t objects
module mam_mode

  use ai_aerosol,                      only : aerosol_t
  use ai_aerosol_state,                only : aerosol_state_t

  implicit none
  private

  public :: mode_t, mode_state_t

  type, extends(aerosol_t) :: mode_t
    private
    integer :: type_ = 0
  contains
    procedure :: new_state
    procedure :: new_optics
    procedure, private :: shortwave_optics_scalar
    procedure, private :: shortwave_optics_array
    procedure, private :: longwave_optics_scalar
    procedure, private :: longwave_optics_array
    procedure :: shortwave_grid
    procedure :: longwave_grid
    procedure :: add_shortwave_optics
    procedure :: add_longwave_optics
    procedure :: print_state
  end type mode_t

  interface mode_t
    procedure :: constructor
  end interface

  type, extends(aerosol_state_t) :: mode_state_t
    private
    integer :: type_ = 0
  contains
    procedure :: raw_size
    procedure :: load_state
    procedure :: dump_state
    procedure :: randomize
  end type mode_state_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function constructor( config ) result( new_mode )

    use musica_config,                 only : config_t

    type(mode_t)                   :: new_mode
    class(config_t), intent(inout) :: config

    character(len=*), parameter :: my_name = "mock mode_t constructor"
    call config%get( "type", new_mode%type_, my_name )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function new_state( this )

    class(aerosol_state_t), pointer    :: new_state
    class(mode_t),          intent(in) :: this

    allocate( mode_state_t :: new_state )
    select type( new_state )
    class is( mode_state_t )
      new_state%type_ = this%type_
    end select

  end function new_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function new_optics( this, property, output_grid, interpolation_strategy )

    use ai_optics,                     only : optics_t
    use ai_wavelength_grid,            only : wavelength_grid_t
    use mam_optics_util,               only : create_optics
    use musica_interpolator,           only : interpolation_strategy_i
    use musica_property,               only : property_t

    class(optics_t),         pointer    :: new_optics
    class(mode_t),           intent(in) :: this
    class(property_t),       intent(in) :: property
    type(wavelength_grid_t), intent(in) :: output_grid
    procedure(interpolation_strategy_i), optional :: interpolation_strategy

  end function new_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine shortwave_optics_scalar( this, environmental_state,              &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t, optics_ptr

    class(mode_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    class(optics_t), target,      intent(inout) :: optics

    type(optics_ptr) :: optics_set(1)

    optics_set(1)%ptr_ => optics
    call this%shortwave_optics_array( environmental_state, aerosol_state,     &
                                      optics_set )
    nullify( optics_set(1)%ptr_ )

  end subroutine shortwave_optics_scalar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine shortwave_optics_array( this, environmental_state,               &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_ptr
    use musica_assert,                 only : die

    class(mode_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    type(optics_ptr),             intent(inout) :: optics(:)

    integer :: i_prop

    do i_prop = 1, size( optics )
      call optics( i_prop )%ptr_%reset_values( )
    end do
    select type( aerosol_state )
    class is( mode_state_t )
      call this%add_shortwave_optics( environmental_state, aerosol_state,     &
                                      optics )
    class default
      call die( 527732474 )
    end select

  end subroutine shortwave_optics_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine longwave_optics_scalar( this, environmental_state,               &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t, optics_ptr

    class(mode_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    class(optics_t), target,      intent(inout) :: optics

    type(optics_ptr) :: optics_set(1)

    optics_set(1)%ptr_ => optics
    call this%longwave_optics_array( environmental_state, aerosol_state,      &
                                      optics_set )
    nullify( optics_set(1)%ptr_ )

  end subroutine longwave_optics_scalar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine longwave_optics_array( this, environmental_state,                &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_ptr
    use musica_assert,                 only : die

    class(mode_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    type(optics_ptr),             intent(inout) :: optics(:)

    integer :: i_prop

    do i_prop = 1, size( optics )
      call optics( i_prop )%ptr_%reset_values( )
    end do
    select type( aerosol_state )
    class is( mode_state_t )
      call this%add_longwave_optics( environmental_state, aerosol_state,      &
                                     optics )
    class default
      call die( 753822403 )
    end select

  end subroutine longwave_optics_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function shortwave_grid( this )

    use ai_wavelength_grid,            only : wavelength_grid_t
    use musica_constants,              only : dk => musica_dk

    type(wavelength_grid_t)   :: shortwave_grid
    class(mode_t), intent(in) :: this

    shortwave_grid = wavelength_grid_t( (/ 12.3_dk,  100.0_dk /),             &
                                        (/ 92.3_dk, 1145.0_dk /) )

  end function shortwave_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function longwave_grid( this )

    use ai_wavelength_grid,            only : wavelength_grid_t
    use musica_constants,              only : dk => musica_dk

    type(wavelength_grid_t)   :: longwave_grid
    class(mode_t), intent(in) :: this

    longwave_grid = wavelength_grid_t( (/ 12.3_dk,  100.0_dk * 2 /),          &
                                       (/ 92.3_dk, 1145.0_dk * 2 /) )

  end function longwave_grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine add_shortwave_optics( this, environmental_state, mode_state,     &
      optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_ptr
    use ai_optics_asymmetry_factor,   only : optics_asymmetry_factor_t
    use ai_optics_extinction_optical_depth,                                   &
        only : optics_extinction_optical_depth_t
    use ai_optics_forward_scattered_fraction,                                 &
        only : optics_forward_scattered_fraction_t
    use ai_optics_single_scatter_albedo,                                      &
        only : optics_single_scatter_albedo_t
    use musica_assert,                 only : die
    use musica_constants,              only : dk => musica_dk

    class(mode_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(mode_state_t),          intent(in)    :: mode_state
    class(optics_ptr),            intent(inout) :: optics(:)

    integer :: i_prop

    do i_prop = 1, size( optics )
      select type( prop => optics( i_prop )%ptr_ )
      class is( optics_extinction_optical_depth_t )
        call prop%add_values( (/ 2.0_dk, 8.5_dk /) )
      class is( optics_single_scatter_albedo_t )
        call prop%add_values( (/ 4.0_dk, 1.8_dk /) )
      class is( optics_asymmetry_factor_t )
        call prop%add_values( (/ 6.0_dk, 5.4_dk /) )
      class is( optics_forward_scattered_fraction_t )
        call prop%add_values( (/ 8.0_dk, 3.2_dk /) )
      class default
        call die( 241133437 )
      end select
    end do

  end subroutine add_shortwave_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine add_longwave_optics( this, environmental_state, mode_state,      &
      optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_ptr
    use ai_optics_absorption_optical_depth,                                   &
        only : optics_absorption_optical_depth_t
    use musica_assert,                 only : die
    use musica_constants,              only : dk => musica_dk

    class(mode_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(mode_state_t),          intent(in)    :: mode_state
    class(optics_ptr),            intent(inout) :: optics(:)

    integer :: i_prop

    do i_prop = 1, size( optics )
      select type( prop => optics( i_prop )%ptr_ )
      class is( optics_absorption_optical_depth_t )
        call prop%add_values( (/ 20.0_dk, 82.5_dk /) )
      class default
        call die( 234868186 )
      end select
    end do

  end subroutine add_longwave_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine print_state( this, aerosol_state, io_unit )

    class(mode_t),          intent(in) :: this
    class(aerosol_state_t), intent(in) :: aerosol_state
    integer, optional,      intent(in) :: io_unit

  end subroutine print_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer function raw_size( this )

    class(mode_state_t), intent(in) :: this

    raw_size = 1

  end function raw_size

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine load_state( this, raw_state, index )

    use musica_constants,              only : musica_dk

    class(mode_state_t),  intent(inout) :: this
    real(kind=musica_dk), intent(in)    :: raw_state(:)
    integer, optional,    intent(inout) :: index

    if( present( index ) ) then
      this%type_ = raw_state( index )
      index = index + 1
    else
      this%type_ = raw_state( 1 )
    end if

  end subroutine load_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dump_state( this, raw_state, index )

    use musica_constants,              only : musica_dk

    class(mode_state_t),  intent(in)    :: this
    real(kind=musica_dk), intent(inout) :: raw_state(:)
    integer, optional,    intent(inout) :: index

    if( present( index ) ) then
      raw_state( index ) = this%type_
      index = index + 1
    else
      raw_state( 1 ) = this%type_
    end if

  end subroutine dump_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine randomize( this )

    use musica_constants,              only : musica_dk

    class(mode_state_t), intent(inout) :: this

    real(kind=musica_dk) :: rand_val

    call random_number( rand_val )
    this%type_ = rand_val * 10000.0_musica_dk

  end subroutine randomize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_mode
