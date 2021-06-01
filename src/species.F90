! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_species module

!> The species_t type and related functions
module mam_species

  implicit none
  private

  public :: species_t

  type :: species_t
    private
    character(len=:), allocatable :: name_
  end type species_t

  interface species_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructs species_t objects
  function constructor( species_name ) result( new_obj )

    type(species_t) :: new_obj
    character(len=*), intent(in) :: species_name

    new_obj%name_ = species_name

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_species
