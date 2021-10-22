! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Mock version of species_t

!> Support module for mam tests using species_t objects
module mam_species

  use musica_string,                   only : string_t

  implicit none
  private

  public :: species_t

  !> Mock version of species_t
  type :: species_t
    private
    integer        :: type_ = 0
    type(string_t) :: name_
  contains
    procedure :: name
    procedure :: volume__m3
    procedure :: shortwave_refractive_index
    procedure :: longwave_refractive_index
  end type species_t

  interface species_t
    procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function constructor( config ) result( new_species )

    use musica_config,                 only : config_t

    type(species_t)                :: new_species
    class(config_t), intent(inout) :: config

    character(len=*), parameter :: my_name = "mock species_t constructor"

    call config%get( "type", new_species%type_, my_name )
    call config%get( "name", new_species%name_, my_name )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  type(string_t) function name( this )

    class(species_t), intent(in) :: this

    name = this%name_

  end function name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  real(kind=dk) elemental function volume__m3( this, species_mass__kg )

    use musica_constants,              only : dk =>musica_dk

    class(species_t), intent(in) :: this
    real(kind=dk),    intent(in) :: species_mass__kg

    volume__m3 = species_mass__kg * this%type_

  end function volume__m3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  complex(kind=dk) elemental function shortwave_refractive_index( this, band )

    use musica_constants,              only : dk => musica_dk

    class(species_t), intent(in) :: this
    integer,          intent(in) :: band

    shortwave_refractive_index = cmplx( band * 12.5_dk,                       &
                                        this%type_ + 2.53_dk,                 &
                                        kind = dk )

  end function shortwave_refractive_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  complex(kind=dk) elemental function longwave_refractive_index( this, band )

    use musica_constants,              only : dk => musica_dk

    class(species_t), intent(in) :: this
    integer,          intent(in) :: band

    longwave_refractive_index = cmplx( band * 15.6_dk,                        &
                                       this%type_ + 1.32_dk,                  &
                                       kind = dk )

  end function longwave_refractive_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_species
