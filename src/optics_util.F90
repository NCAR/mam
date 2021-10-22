! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_optics_constants module

!> Constants used to calculate MAM optical properties
module mam_optics_util

    use ai_optics,                     only : optics_t
    use ai_optics_absorption_optical_depth,                                   &
        only : optics_absorption_optical_depth_t
    use ai_optics_asymmetry_factor,   only : optics_asymmetry_factor_t
    use ai_optics_extinction_optical_depth,                                   &
        only : optics_extinction_optical_depth_t
    use ai_optics_forward_scattered_fraction,                                 &
        only : optics_forward_scattered_fraction_t
    use ai_optics_single_scatter_albedo,                                      &
        only : optics_single_scatter_albedo_t
  use musica_constants,                only : musica_dk

  implicit none
  private

  public :: create_optics, add_shortwave_property, add_longwave_property

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns an optics_t object for a given property
  function create_optics( property, native_shortwave_grid,                    &
      native_longwave_grid, output_grid, interpolation_strategy )             &
      result( new_optics )

    use ai_wavelength_grid,            only : wavelength_grid_t, kWavenumber, &
                                              kCentimeter
    use musica_assert,                 only : die_msg
    use musica_interpolator,           only : interpolation_strategy_i
    use musica_property,               only : property_t
    use musica_string,                 only : string_t

    class(optics_t),  pointer :: new_optics
    class(property_t), intent(in) :: property
    type(wavelength_grid_t), intent(in) :: native_shortwave_grid
    type(wavelength_grid_t), intent(in) :: native_longwave_grid
    type(wavelength_grid_t), intent(in) :: output_grid
    procedure(interpolation_strategy_i), optional :: interpolation_strategy

    type(wavelength_grid_t) :: native_grid
    type(string_t)          :: property_name

    property_name = property%name( )
    if( property_name .eq. "layer extinction optical depth" ) then
      new_optics => optics_extinction_optical_depth_t( native_shortwave_grid, &
                                                       output_grid,           &
                             interpolation_strategy = interpolation_strategy )
    else if( property_name .eq. "layer single-scatter albedo" ) then
      new_optics => optics_single_scatter_albedo_t(    native_shortwave_grid, &
                                                       output_grid,           &
                             interpolation_strategy = interpolation_strategy )
    else if( property_name .eq. "asymmetry factor" ) then
      new_optics => optics_asymmetry_factor_t(         native_shortwave_grid, &
                                                       output_grid,           &
                             interpolation_strategy = interpolation_strategy )
    else if( property_name .eq. "forward scattered fraction" ) then
      new_optics => optics_forward_scattered_fraction_t(                      &
                                                       native_shortwave_grid, &
                                                       output_grid,           &
                             interpolation_strategy = interpolation_strategy )
    else if( property_name .eq. "layer absorption optical depth" ) then
      new_optics => optics_absorption_optical_depth_t( native_longwave_grid,  &
                                                       output_grid,           &
                             interpolation_strategy = interpolation_strategy )
    else
      call die_msg( 769442313, "Unsupported optical property: '"//            &
                               property_name//"'" )
    end if

  end function create_optics

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds extinction optical depths to optical property values
  subroutine add_shortwave_extinction_optical_depth(                          &
      extinction_optical_depth, single_scatter_albedo, asymmetry_factor,      &
      optics )

    real(kind=musica_dk), intent(in) :: extinction_optical_depth(:)
    real(kind=musica_dk), intent(in) :: single_scatter_albedo(:)
    real(kind=musica_dk), intent(in) :: asymmetry_factor(:)
    class(optics_extinction_optical_depth_t), intent(inout) :: optics

    call optics%add_values( extinction_optical_depth )

  end subroutine add_shortwave_extinction_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds single-scatter albedo to optical property values
  subroutine add_shortwave_single_scatter_albedo(                             &
      extinction_optical_depth, single_scatter_albedo, asymmetry_factor,      &
      optics )

    real(kind=musica_dk), intent(in) :: extinction_optical_depth(:)
    real(kind=musica_dk), intent(in) :: single_scatter_albedo(:)
    real(kind=musica_dk), intent(in) :: asymmetry_factor(:)
    class(optics_single_scatter_albedo_t), intent(inout) :: optics

    call optics%add_values( extinction_optical_depth(:)                       &
                            * single_scatter_albedo(:) )

  end subroutine add_shortwave_single_scatter_albedo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds asymmetry factor to optical property values
  subroutine add_shortwave_asymmetry_factor(                                  &
      extinction_optical_depth, single_scatter_albedo, asymmetry_factor,      &
      optics )

    real(kind=musica_dk), intent(in) :: extinction_optical_depth(:)
    real(kind=musica_dk), intent(in) :: single_scatter_albedo(:)
    real(kind=musica_dk), intent(in) :: asymmetry_factor(:)
    class(optics_asymmetry_factor_t), intent(inout) :: optics

    call optics%add_values( extinction_optical_depth(:)                       &
                            * single_scatter_albedo(:)                        &
                            * asymmetry_factor(:) )

  end subroutine add_shortwave_asymmetry_factor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds forward scattered fraction to optical property values
  subroutine add_shortwave_forward_scattered_fraction(                        &
      extinction_optical_depth, single_scatter_albedo, asymmetry_factor,      &
      optics )

    real(kind=musica_dk), intent(in) :: extinction_optical_depth(:)
    real(kind=musica_dk), intent(in) :: single_scatter_albedo(:)
    real(kind=musica_dk), intent(in) :: asymmetry_factor(:)
    class(optics_forward_scattered_fraction_t), intent(inout) :: optics

    call optics%add_values( extinction_optical_depth(:)                       &
                            * single_scatter_albedo(:)                        &
                            * asymmetry_factor(:)                             &
                            * asymmetry_factor(:) )

  end subroutine add_shortwave_forward_scattered_fraction

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds to shortwave property values
  subroutine add_shortwave_property(                                          &
      extinction_optical_depth, single_scatter_albedo, asymmetry_factor,      &
      optics )

    use musica_assert,               only : die_msg

    real(kind=musica_dk), intent(in)    :: extinction_optical_depth(:)
    real(kind=musica_dk), intent(in)    :: single_scatter_albedo(:)
    real(kind=musica_dk), intent(in)    :: asymmetry_factor(:)
    class(optics_t),      intent(inout) :: optics

    select type( optics )
    class is( optics_extinction_optical_depth_t )
      call add_shortwave_extinction_optical_depth(                            &
        extinction_optical_depth, single_scatter_albedo, asymmetry_factor,    &
        optics )
    class is( optics_single_scatter_albedo_t )
      call add_shortwave_single_scatter_albedo(                               &
        extinction_optical_depth, single_scatter_albedo, asymmetry_factor,    &
        optics )
    class is( optics_asymmetry_factor_t )
      call add_shortwave_asymmetry_factor(                                    &
        extinction_optical_depth, single_scatter_albedo, asymmetry_factor,    &
        optics )
    class is( optics_forward_scattered_fraction_t )
      call add_shortwave_forward_scattered_fraction(                          &
        extinction_optical_depth, single_scatter_albedo, asymmetry_factor,    &
        optics )
    class default
      call die_msg( 628273876, "Unsupported MAM shortwave optical property" )
    end select

  end subroutine add_shortwave_property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds absorption optical depth to optical property values
  subroutine add_longwave_absorption_optical_depth(                           &
      extinction_optical_depth, optics )

    real(kind=musica_dk), intent(in) :: extinction_optical_depth(:)
    class(optics_absorption_optical_depth_t), intent(inout) :: optics

    call optics%add_values( extinction_optical_depth )

  end subroutine add_longwave_absorption_optical_depth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Adds to longwave property values
  subroutine add_longwave_property( extinction_optical_depth, optics )

    use musica_assert,               only : die_msg

    real(kind=musica_dk), intent(in)    :: extinction_optical_depth(:)
    class(optics_t),      intent(inout) :: optics

    select type( optics )
    class is( optics_absorption_optical_depth_t )
      call add_longwave_absorption_optical_depth( extinction_optical_depth,   &
                                                  optics )
    class default
      call die_msg(405116022, "Unsupported MAM longwave optical property" )
    end select

  end subroutine add_longwave_property

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_optics_util
