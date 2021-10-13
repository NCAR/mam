! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the mam_optics_util module

!> Support module for tests involving interpolation strategies
module mock_interpolation_strategy

  implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function foo_mapper( from_grid, to_grid ) result( map )

    use musica_assert,                 only : assert
    use musica_constants,              only : musica_dk
    use musica_grid,                   only : grid_t, grid_iterator_t
    use musica_interpolator,           only : interpolator_element_t

    class(interpolator_element_t), allocatable :: map(:)
    class(grid_iterator_t), pointer :: iter_from, iter_to
    class(grid_t), intent(in) :: from_grid
    class(grid_t), intent(in) :: to_grid
    type(interpolator_element_t) :: local_map(3)

    iter_from => from_grid%iterator( )
    iter_to   => to_grid%iterator( )

    call assert( 320191121, iter_from%next( ) )
    call assert( 932451564, iter_to%next( ) )
    ! to(1) = from(1) * 1.0
    local_map(1) = interpolator_element_t( iter_from, iter_to, 1.0_musica_dk )

    call assert( 144769910, iter_from%next( ) )
    ! to(1) = from(2) * 0.5
    local_map(2) = interpolator_element_t( iter_from, iter_to, 0.5_musica_dk )

    call assert( 874613005, iter_to%next( ) )
    ! to(2) = from(2) * 0.5
    local_map(3) = interpolator_element_t( iter_from, iter_to, 0.5_musica_dk )

    allocate( map, source = local_map )

    deallocate( iter_from )
    deallocate( iter_to   )

  end function foo_mapper

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mock_interpolation_strategy
