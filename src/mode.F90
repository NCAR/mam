! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_mode module

!> The mode_t type and related functions
module mam_mode

  use ai_constants,                    only : kDouble
  use mam_species,                     only : species_t

  implicit none
  private

  public :: mode_t

  !> An aerosol mode
  type :: mode_t
    private
    !> Geometric mean diameter [m]
    real(kind=kDouble) :: geometric_mean_diameter__m_
    !> Geometric standard deviation
    real(kind=kDouble) :: geometric_standard_deviation_
    !> Chemical species that can be present in the mode
    type(species_t), allocatable :: species_(:)
    !> Mass concentrations for each chemical species
    !! (species, layer, column) [kg m-3]
    real(kind=kDouble), allocatable :: mass_concentrations__kg_m3_(:,:,:)
  end type mode_t

  interface mode_t
    module procedure :: constructor
  end interface mode_t

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructor of mode_t objects
  function constructor( geometric_mean_diameter__m,                           &
      geometric_standard_deviation, chemical_species, number_of_columns,      &
      number_of_layers ) result( new_obj )

    type(mode_t)                   :: new_obj
    real(kind=kDouble), intent(in) :: geometric_mean_diameter__m
    real(kind=kDouble), intent(in) :: geometric_standard_deviation
    type(species_t),    intent(in) :: chemical_species(:)
    integer,            intent(in) :: number_of_columns
    integer,            intent(in) :: number_of_layers

    new_obj%geometric_mean_diameter__m_   = geometric_mean_diameter__m
    new_obj%geometric_standard_deviation_ = geometric_standard_deviation
    new_obj%species_                      = chemical_species
    allocate( new_obj%mass_concentrations__kg_m3_( size( chemical_species ),  &
                                                   number_of_layers,          &
                                                   number_of_columns ) )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_mode
