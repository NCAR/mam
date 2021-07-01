! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> The mam_lookup_table_2D_1D module

!> The lookup_table_2D_1D_t type and related functions
module mam_lookup_table_2D_1D

  use ai_constants,                    only : kDouble

  implicit none
  private

  public :: lookup_table_2D_1D_t

  !> Lookup table
  !!
  !! Lookup table with 2 independent variables. Returned data is a 1D array.
  !!
  type :: lookup_table_2D_1D_t
    private
    !> Lookup table data (returned array, axis 1, axis 2)
    real(kind=kDouble), allocatable :: table_(:,:,:)
    !> Independent variable 1
    real(kind=kDouble), allocatable :: axis_1_(:)
    !> Independent variable 2
    real(kind=kDouble), allocatable :: axis_2_(:)
  contains
    procedure :: get
  end type lookup_table_2D_1D_t

  !> Constructor for lookup_table_2D_1D_t
  interface lookup_table_2D_1D_t
    procedure :: constructor
  end interface

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Reads file data to create a new lookup table
  function constructor( config ) result( new_obj )

    use musica_assert,                 only : assert_msg
    use musica_config,                 only : config_t
    use musica_file_util,              only : get_file_data
    use musica_string,                 only : string_t

    type(lookup_table_2D_1D_t), pointer       :: new_obj
    class(config_t),            intent(inout) :: config

    type(string_t) :: file_name, table_name, axis_1_name, axis_2_name
    character(len=*), parameter :: my_name = "lookup_table_2D_1D_t constructor"

    call config%get( "file path", file_name,   my_name )
    call config%get( "table",     table_name,  my_name )
    call config%get( "axis 1",    axis_1_name, my_name )
    call config%get( "axis 2",    axis_2_name, my_name )
    allocate( new_obj )
    call get_file_data( file_name,  table_name,  new_obj%table_, my_name )
    allocate( new_obj%axis_1_( size( new_obj%table_, 2 ) ) )
    allocate( new_obj%axis_2_( size( new_obj%table_, 3 ) ) )
    call get_file_data( file_name, axis_1_name, new_obj%axis_1_, my_name )
    call get_file_data( file_name, axis_2_name, new_obj%axis_2_, my_name )

  end function constructor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Returns a lookup-up array from the table
  subroutine get( this, variable_1, variable_2, array, variable_1_index,     &
     variable_2_index )

    class(lookup_table_2D_1D_t), intent(in)    :: this
    real(kind=kDouble),          intent(in)    :: variable_1
    real(kind=kDouble),          intent(in)    :: variable_2
    real(kind=kDouble),          intent(out)   :: array(:)
    integer, optional,           intent(inout) :: variable_1_index
    integer, optional,           intent(inout) :: variable_2_index

    integer :: i_var1, i_var2
    real(kind=kDouble) :: residual_1, residual_2

    i_var1 = 1
    i_var2 = 1
    if( present( variable_1_index ) ) i_var1 = variable_1_index
    if( present( variable_2_index ) ) i_var2 = variable_2_index
    call find_value( this%axis_1_, variable_1, i_var1, residual_1 )
    call find_value( this%axis_2_, variable_2, i_var2, residual_2 )
    call binary_interpolation( this%table_, i_var1, i_var2, residual_1,       &
                               residual_2, array )

  end subroutine get

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Finds the index corresponding to a given value in an array
  subroutine find_value( array, value, index, residual )

    !> Array to find value in
    !!
    !! The array must be ordered and ascending.
    real(kind=kDouble), intent(in)    :: array(:)
    !> Value to find in array
    real(kind=kDouble), intent(in)    :: value
    !> Index in array for found value
    !!
    !! The index corresponds to the closest value that is less than or equal
    !! to the given value.
    !!
    !! The value of index passed to this routine will be used as the first
    !! guess of the index.
    integer,            intent(inout) :: index
    !> The fractional remainder of the value relative to the distance to the
    !! next value in the array.
    real(kind=kDouble), intent(out)   :: residual

    do while( index .gt. 1 )
      if( array( index ) .le. value ) exit
      index = index - 1
    end do
    do while( index .lt. size( array ) )
      if( array( index + 1 ) .gt. value ) exit
      index = index + 1
    end do
    residual = 0.0_kDouble
    if( index .lt. size( array ) ) then
      residual = ( value - array( index ) )                                   &
                 / ( array( index + 1 ) - array( index ) )
    end if

  end subroutine find_value

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Does a binary interpolation given indices and fractional residuals
  subroutine binary_interpolation( table, index_1, index_2, residual_1,       &
      residual_2, array )

    !> Lookup table data (returned array, axis 1, axis 2)
    real(kind=kDouble), intent(in)  :: table(:,:,:)
    !> Index along axis 1
    integer,            intent(in)  :: index_1
    !> Index along axis 2
    integer,            intent(in)  :: index_2
    !> Fractional residual along axis 1
    real(kind=kDouble), intent(in)  :: residual_1
    !> Fractional residual along axis 2
    real(kind=kDouble), intent(in)  :: residual_2
    !> Interpolated array
    real(kind=kDouble), intent(out) :: array(:)

    integer :: next_1, next_2
    real(kind=kDouble) :: tu, tuc, tcuc, tcu

    next_1 = min( index_1 + 1, size( table, 2 ) )
    next_2 = min( index_2 + 1, size( table, 3 ) )
    tu  = residual_1 * residual_2
    tuc = residual_1 - tu
    tcuc = 1.0_kDouble - tuc - residual_2
    tcu  = residual_2 - tu
    array(:) = tcuc * table( :, index_1, index_2 ) +                          &
                tuc * table( :,  next_1, index_2 ) +                          &
                 tu * table( :,  next_1,  next_2 ) +                          &
                tcu * table( :, index_1,  next_2 )

  end subroutine binary_interpolation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module mam_lookup_table_2D_1D
