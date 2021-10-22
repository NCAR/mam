! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Mock version of optics_lookup_t

!> Support module for mam tests using optics_lookup_t objects
module mam_optics_lookup

  implicit none
  private

  public :: optics_lookup_t

  integer, parameter :: kNumberOfCoefficients = 3
  integer, parameter :: kNumberOfBands        = 2

  !> Mock optics lookup for tests
  type :: optics_lookup_t
    private
    integer :: type_ = 0
  contains
    procedure :: get_optics
    procedure :: normalize_radius
    procedure :: minimum_radius__m
    procedure :: maximum_radius__m
    procedure :: number_of_chebyshev_coefficients
    procedure :: number_of_wavelength_bands
    procedure :: grid
  end type optics_lookup_t

  interface optics_lookup_t
    procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function constructor( config ) result( new_lookup )

    use musica_config,                 only : config_t

    type(optics_lookup_t)               :: new_lookup
    class(config_t),      intent(inout) :: config

    character(len=*), parameter :: my_name = "mock optics_lookup_t constructor"

    call config%get( "type", new_lookup%type_, my_name )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  pure subroutine get_optics( this, refractive_indices, absorption,           &
      extinction, asymmetry_factor )

    use musica_constants,              only : dk => musica_dk

    class(optics_lookup_t), intent(in) :: this
    complex(kind=dk), intent(in) :: refractive_indices(kNumberOfBands)
    real(kind=dk), optional, intent(out) ::                                   &
        absorption(kNumberOfCoefficients, kNumberOfBands)
    real(kind=dk), optional, intent(out) ::                                   &
        extinction(kNumberOfCoefficients, kNumberOfBands)
    real(kind=dk), optional, intent(out) ::                                   &
        asymmetry_factor(kNumberOfCoefficients, kNumberOfBands)

    if( present( absorption ) ) then
      absorption(:,1) = (/ 42.5e-20_dk,  62.3e-20_dk, 4.342e-20_dk /)
      absorption(:,2) = (/ 13.2e-20_dk, 142.3e-20_dk,  2.34e-20_dk /)
    end if
    if( present( extinction ) ) then
      extinction(:,1) = (/ 52.4_dk,  95.2_dk,  3.6_dk /)
      extinction(:,2) = (/ 93.5_dk, 734.2_dk, 1.62_dk /)
    end if
    if( present( asymmetry_factor ) ) then
      asymmetry_factor(:,1) = (/ 61.3_dk,  90.3_dk,  1.2_dk /)
      asymmetry_factor(:,2) = (/ 88.2_dk, 369.3_dk, 5.28_dk /)
    end if

  end subroutine get_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real(kind=musica_dk) elemental function normalize_radius( this, radius__m )

    use musica_constants,              only : musica_dk

    class(optics_lookup_t), intent(in) :: this
    real(kind=musica_dk),   intent(in) :: radius__m

    normalize_radius = radius__m + 93.4e-8_musica_dk * this%type_

  end function normalize_radius

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real(kind=musica_dk) elemental function minimum_radius__m( this )

    use musica_constants,              only : musica_dk

    class(optics_lookup_t), intent(in) :: this

    minimum_radius__m = 132.4e-9_musica_dk * this%type_

  end function minimum_radius__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real(kind=musica_dk) elemental function maximum_radius__m( this )

    use musica_constants,              only : musica_dk

    class(optics_lookup_t), intent(in) :: this

    maximum_radius__m = 132.4e-6_musica_dk * this%type_

  end function maximum_radius__m

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer elemental function number_of_chebyshev_coefficients( this )

    class(optics_lookup_t), intent(in) :: this

    number_of_chebyshev_coefficients = kNumberOfCoefficients

  end function number_of_chebyshev_coefficients

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  integer elemental function number_of_wavelength_bands( this )

    class(optics_lookup_t), intent(in) :: this

    number_of_wavelength_bands = kNumberOfBands

  end function number_of_wavelength_bands

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type(wavelength_grid_t) function grid( this )

    use ai_wavelength_grid,            only : wavelength_grid_t
    use musica_constants,              only : dk => musica_dk

    class(optics_lookup_t), intent(in) :: this

    grid = wavelength_grid_t( (/ 12.3_dk,  100.0_dk * this%type_ /),          &
                              (/ 92.3_dk, 1145.0_dk * this%type_ /) )

  end function grid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_optics_lookup
