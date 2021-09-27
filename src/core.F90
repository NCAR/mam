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
    type(mode_t), allocatable          :: modes_(:)
  contains
    procedure :: new_state
    procedure :: new_optics
    procedure, private :: shortwave_optics_scalar
    procedure, private :: shortwave_optics_array
    procedure, private :: longwave_optics_scalar
    procedure, private :: longwave_optics_array
    procedure :: print_state
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
    procedure :: randomize
  end type state_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of the MAM core
  function constructor( config ) result( new_obj )

    use mam_species,                   only : species_t
    use musica_config,                 only : config_t
    use musica_iterator,               only : iterator_t
    use musica_string,                 only : string_t

    type(core_t),    pointer       :: new_obj
    class(config_t), intent(inout) :: config

    character(len=*), parameter :: my_name = "MAM core_t constructor"
    type(config_t)              :: modes, mode
    type(string_t)              :: file_name
    class(iterator_t), pointer  :: iter
    integer                     :: i_mode

    allocate( new_obj )
    call config%get( "modes", modes, my_name )
    allocate( new_obj%modes_( modes%number_of_children( ) ) )
    iter => modes%get_iterator( )
    i_mode = 1
    do while( iter%next( ) )
      call modes%get( iter, mode, my_name )
      new_obj%modes_( i_mode ) = mode_t( mode )
      i_mode = i_mode + 1
    end do
    deallocate( iter )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates a new state object for the modal aerosol
  function new_state( this )

    class(aerosol_state_t), pointer    :: new_state
    class(core_t),          intent(in) :: this

    class(aerosol_state_t), pointer :: mode_state
    integer :: i_mode

    allocate( state_t :: new_state )
    select type( new_state )
    type is( state_t )
      allocate( new_state%mode_states_( size( this%modes_ ) ) )
      do i_mode = 1, size( this%modes_ )
        mode_state => this%modes_( i_mode )%new_state( )
        select type( mode_state )
        class is( mode_state_t )
          new_state%mode_states_( i_mode ) = mode_state
        end select
        deallocate( mode_state )
      end do
    end select

  end function new_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Creates an optics_t object for a given property
  function new_optics( this, property, output_grid, interpolation_strategy )

    use ai_optics,                     only : optics_t
    use ai_wavelength_grid,            only : wavelength_grid_t
    use mam_optics_util,               only : create_optics
    use musica_interpolator,           only : interpolation_strategy_i
    use musica_property,               only : property_t

    class(optics_t),                   pointer    :: new_optics
    class(core_t),                     intent(in) :: this
    class(property_t),                 intent(in) :: property
    type(wavelength_grid_t),           intent(in) :: output_grid
    procedure(interpolation_strategy_i), optional :: interpolation_strategy

    new_optics =>                                                             &
        create_optics( property, output_grid, interpolation_strategy )

  end function new_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns shortwave optical properties
  subroutine shortwave_optics_scalar( this, environmental_state,              &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t, optics_ptr

    class(core_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    class(optics_t), target,      intent(inout) :: optics

    type(optics_ptr) :: optics_set(1)

    optics_set(1)%ptr_ => optics
    call this%shortwave_optics( environmental_state, aerosol_state,           &
                                optics_set )
    nullify( optics_set(1)%ptr_ )

  end subroutine shortwave_optics_scalar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns shortwave optical properties
  subroutine shortwave_optics_array( this, environmental_state,               &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_ptr
    use musica_assert,                 only : die_msg

    class(core_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    type(optics_ptr),             intent(inout) :: optics(:)

    integer :: i_mode, i_prop

    select type( aerosol_state )
    class is( state_t )
      do i_prop = 1, size( optics )
        call optics( i_prop )%ptr_%reset_values( )
      end do
      do i_mode = 1, size( this%modes_ )
      associate( mode_state => aerosol_state%mode_states_( i_mode ),          &
                 mode       => this%modes_( i_mode ) )
        call mode%add_shortwave_optics( environmental_state,                  &
                                        mode_state,                           &
                                        optics )
      end associate
      end do
    class default
      call die_msg( 822004975, "Invalid state passed to MAM shortwave "//     &
                               "optics calculator" )
    end select

  end subroutine shortwave_optics_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns longwave optical properties
  subroutine longwave_optics_scalar( this, environmental_state,               &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_t, optics_ptr

    class(core_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    class(optics_t), target,      intent(inout) :: optics

    type(optics_ptr) :: optics_set(1)

    optics_set(1)%ptr_ => optics
    call this%longwave_optics( environmental_state, aerosol_state,            &
                               optics_set )
    nullify( optics_set(1)%ptr_ )

  end subroutine longwave_optics_scalar

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns longwave optical properties
  subroutine longwave_optics_array( this, environmental_state,                &
      aerosol_state, optics )

    use ai_environmental_state,        only : environmental_state_t
    use ai_optics,                     only : optics_ptr
    use musica_assert,                 only : die_msg

    class(core_t),                intent(in)    :: this
    class(environmental_state_t), intent(in)    :: environmental_state
    class(aerosol_state_t),       intent(in)    :: aerosol_state
    type(optics_ptr),             intent(inout) :: optics(:)

    integer :: i_mode, i_prop

    select type( aerosol_state )
    class is( state_t )
      do i_prop = 1, size( optics )
        call optics( i_prop )%ptr_%reset_values( )
      end do
      do i_mode = 1, size( this%modes_ )
      associate( mode_state => aerosol_state%mode_states_( i_mode ),          &
                 mode       => this%modes_( i_mode ) )
        call mode%add_longwave_optics( environmental_state,                   &
                                       mode_state,                            &
                                       optics )
      end associate
      end do
    class default
      call die_msg( 466350541, "Invalid state passed to MAM longwave "//      &
                               "optics calculator" )
    end select

  end subroutine longwave_optics_array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Ouptuts the current aerosol state
  subroutine print_state( this, aerosol_state, io_unit )

    use musica_assert,                 only : die_msg
    use musica_string,                 only : to_char

    class(core_t),          intent(in) :: this
    class(aerosol_state_t), intent(in) :: aerosol_state
    !> Optional output unit (defaults to 6)
    integer, optional,      intent(in) :: io_unit

    integer :: lunit, i_mode

    lunit = 6
    if( present( io_unit ) ) lunit = io_unit
    select type( aerosol_state )
    class is( state_t )
      write(lunit,*) "**** MAM Aerosol State ****"
      if( .not. allocated( this%modes_ ) ) then
        write(lunit,*) "--- Uninitialized MAM core ---"
        write(lunit,*) "**** End MAM Aerosol State ****"
        return
      end if
      if( .not. allocated( aerosol_state%mode_states_ ) ) then
        write(lunit,*) "--- Uninitialized MAM state ---"
        write(lunit,*) "**** End MAM Aerosol State ****"
        return
      end if
      do i_mode = 1, size( this%modes_ )
        write(lunit,*) "*** Mode "//trim( to_char( i_mode ) )//" ***"
        call this%modes_( i_mode )%print_state(                               &
                               aerosol_state%mode_states_( i_mode ), io_unit )
      end do
      write(lunit,*) "**** End MAM Aerosol State ****"
    class default
      call die_msg( 970908819, "Invalid state passed to MAM aerosol" )
    end select

  end subroutine print_state

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

    use musica_constants,              only : musica_dk

    class(state_t),             intent(inout) :: this
    real(kind=musica_dk), target, intent(inout) :: raw_state(:)
    integer, optional,          intent(inout) :: index

    integer :: i_mode, lindex

    lindex = 1
    if( present( index ) ) lindex = index
    do i_mode = 1, size( this%mode_states_ )
      call this%mode_states_( i_mode )%load_state( raw_state, lindex )
    end do
    if( present( index ) ) index = lindex

  end subroutine load_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Dumps the raw MAM state data to an array
  subroutine dump_state( this, raw_state, index )

    use musica_constants,              only : musica_dk

    class(state_t),     intent(inout) :: this
    real(kind=musica_dk), intent(inout) :: raw_state(:)
    integer, optional,  intent(inout) :: index

    integer :: i_mode, lindex

    lindex = 1
    if( present( index ) ) lindex = index
    do i_mode = 1, size( this%mode_states_ )
      call this%mode_states_( i_mode )%dump_state( raw_state, lindex )
    end do
    if( present( index ) ) index = lindex

  end subroutine dump_state

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Sets the MAM state to a random, but reasonable, state. For testing only.
  subroutine randomize( this )

    use musica_assert,                 only : assert_msg

    class(state_t), intent(inout) :: this

    integer :: i_mode

    call assert_msg( 652075662, allocated( this%mode_states_ ),               &
                     "Trying to randomize uninitialized MAM state" )
    do i_mode = 1, size( this%mode_states_ )
      call this%mode_states_( i_mode )%randomize( )
    end do

  end subroutine randomize

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_core
