! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_species module

!> The species_t type and related functions
module mam_species

  use musica_constants,                only : musica_dk
  use musica_string,                   only : string_t

  implicit none
  private

  public :: species_t

  !> \todo species should read in the wavelength grid for refractive
  !!       indices instead of assuming it matches the MAM grid
  type :: species_t
    private
    type(string_t)                       :: name_
    real(kind=musica_dk)                 :: density__kg_m3_
    complex(kind=musica_dk), allocatable :: shortwave_refractive_index_(:)
    complex(kind=musica_dk), allocatable :: longwave_refractive_index_(:)
  contains
    procedure :: name
    procedure :: volume__m3
    procedure :: shortwave_refractive_index
    procedure :: longwave_refractive_index
  end type species_t

  interface species_t
    module procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Constructs species_t objects
  !!
  !! \todo The complex refractive indices in the NetCDF files use a different
  !!       convention for specifying the complex index of refraction that
  !!       results in the need to take the absolute value of the imaginary
  !!       part to get the complex index of refraction used in MAM.
  !!       The NetCDF files should be updated to use positive values for the
  !!       imaginary part.
  function constructor( config ) result( new_obj )

    use musica_assert,                 only : assert_msg
    use musica_config,                 only : config_t
    use musica_io,                     only : io_t
    use musica_io_netcdf,              only : io_netcdf_t

    type(species_t) :: new_obj
    class(config_t), intent(inout) :: config

    character(len=*), parameter :: my_name = "species_t constructor"
    class(io_t), pointer :: file_ptr
    type(string_t) :: file_path
    type(string_t) :: sw_real_ri, sw_imag_ri, lw_real_ri, lw_imag_ri
    type(config_t) :: optics
    real(kind=musica_dk), allocatable :: real_values(:), imag_values(:)
    logical :: found

    call config%get( "name",              new_obj%name_,           my_name )
    call config%get( "density", "kg m-3", new_obj%density__kg_m3_, my_name )
    call config%get( "optics",            optics,   my_name, found = found )
    if( found ) then
      call optics%get( "file path",         file_path,               my_name )
      file_ptr => io_netcdf_t( file_path )
      call optics%get( "real refractive index - shortwave",      sw_real_ri,  &
                       my_name )
      call optics%get( "real refractive index - longwave",       lw_real_ri,  &
                       my_name )
      call optics%get( "imaginary refractive index - shortwave", sw_imag_ri,  &
                       my_name )
      call optics%get( "imaginary refractive index - longwave",  lw_imag_ri,  &
                       my_name )
      call file_ptr%read( sw_real_ri, real_values, my_name )
      call file_ptr%read( sw_imag_ri, imag_values, my_name )
      call assert_msg( 434735228,                                             &
                       size( real_values ) .eq. size( imag_values ),          &
                       "Length mismatch for shortwave refractive index "//    &
                       "arrays for species '"//new_obj%name_//"'" )
      allocate( new_obj%shortwave_refractive_index_( size( real_values ) ) )
      new_obj%shortwave_refractive_index_(:) =                                &
          cmplx( real_values(:), abs( imag_values(:) ), kind = musica_dk )
      deallocate( real_values )
      deallocate( imag_values )
      call file_ptr%read( lw_real_ri, real_values, my_name )
      call file_ptr%read( lw_imag_ri, imag_values, my_name )
      call assert_msg( 979080793,                                             &
                       size( real_values ) .eq. size( imag_values ),          &
                       "Length mismatch for longwave refractive index "//     &
                       "arrays for species '"//new_obj%name_//"'" )
      allocate( new_obj%longwave_refractive_index_( size( real_values ) ) )
      new_obj%longwave_refractive_index_(:) =                                 &
          cmplx( real_values(:), abs( imag_values(:) ), kind = musica_dk )
      deallocate( file_ptr )
    else
      new_obj%shortwave_refractive_index_(:) = cmplx( 0.0, 0.0 )
      new_obj%longwave_refractive_index_(:)  = cmplx( 0.0, 0.0 )
    end if

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the species name
  type(string_t) function name( this )

    class(species_t), intent(in) :: this

    name = this%name_

  end function name

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the total volume occupied by the species for a given mass
  real(kind=musica_dk) elemental function volume__m3( this, species_mass__kg )

    class(species_t),     intent(in) :: this
    real(kind=musica_dk), intent(in) :: species_mass__kg

    volume__m3 = species_mass__kg / this%density__kg_m3_

  end function volume__m3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the shortwave refractive index at a specified band index
  complex(kind=musica_dk) elemental function shortwave_refractive_index( this,&
      band )

    class(species_t), intent(in) :: this
    integer,          intent(in) :: band

    shortwave_refractive_index = this%shortwave_refractive_index_( band )

  end function shortwave_refractive_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns the longwave refractive index at a specified band index
  complex(kind=musica_dk) elemental function longwave_refractive_index( this, &
      band )

    class(species_t), intent(in) :: this
    integer,          intent(in) :: band

    longwave_refractive_index = this%longwave_refractive_index_( band )

  end function longwave_refractive_index

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_species
