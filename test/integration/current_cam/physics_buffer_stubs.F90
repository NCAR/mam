! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module physics_buffer
  implicit none
  private
  public :: pbuf_get_index, pbuf_get_field

  type, public :: physics_buffer_desc
  end type physics_buffer_desc

  interface pbuf_get_field
    module procedure :: get_pbuf1d_field_by_index_0d_int
    module procedure :: get_pbuf1d_field_by_index_1d_r8
    module procedure :: get_pbuf1d_field_by_index_2d_r8
    module procedure :: get_pbuf1d_field_by_index_3d_r8
  end interface

  ! pbuf variables
  character(len=10), dimension(*), parameter :: kStateVariables = (/          &
      'num_c1    ', 'so4_c1    ', 'pom_c1    ', 'soa_c1    ', 'bc_c1     ',   &
      'dst_c1    ', 'ncl_c1    ', 'num_c2    ', 'so4_c2    ', 'soa_c2    ',   &
      'ncl_c2    ', 'dst_c2    ', 'num_c3    ', 'dst_c3    ', 'ncl_c3    ',   &
      'so4_c3    ', 'num_c4    ', 'pom_c4    ', 'bc_c4     ', 'ozone     ',   &
      'O2        ', 'CO2       ', 'N2O       ', 'CH4       ', 'CFC11     ',   &
      'CFC12     ', 'DGNUMWET  ', 'QAERWAT   ' /)

contains

  integer function pbuf_get_index(name, code) result( idx )
    character(len=*), intent(in) :: name
    integer, intent(out), optional :: code
    if( present( code ) ) code = 0
    do idx = 1, size( kStateVariables )
      if( trim( name ) .eq. trim( kStateVariables( idx ) ) ) return
    end do
    write(*,*) "pbuf_get_index cannot find species: '"//trim( name )//"'"
    call abort
  end function pbuf_get_index

  subroutine get_pbuf1d_field_by_index_0d_int(pbuf, index, field, start,      &
      kount, col_type, copy_if_needed, errcode)
    type(physics_buffer_desc), pointer:: pbuf(:)
    integer,             intent(in)               :: index
    integer,    pointer                           :: field
    integer,             intent(in),  optional    :: start(:),kount(:)
    integer,             intent(in),  optional    :: col_type
    logical,             intent(in),  optional    :: copy_if_needed
    integer,             intent(out), optional    :: errcode
    field => null( )
    if( present( errcode ) ) errcode = 0
  end subroutine get_pbuf1d_field_by_index_0d_int

  subroutine get_pbuf1d_field_by_index_1d_r8(pbuf, index, field, start,       &
      kount, col_type, copy_if_needed, errcode)
    use shr_kind_mod,                  only : r8 => shr_kind_r8
    type(physics_buffer_desc), pointer:: pbuf(:)
    integer,             intent(in)               :: index
    real(kind=r8), pointer                        :: field(:)
    integer,             intent(in),  optional    :: start(:),kount(:)
    integer,             intent(in),  optional    :: col_type
    logical,             intent(in),  optional    :: copy_if_needed
    integer,             intent(out), optional    :: errcode
    field => null( )
    if( present( errcode ) ) errcode = 0
  end subroutine get_pbuf1d_field_by_index_1d_r8

  subroutine get_pbuf1d_field_by_index_2d_r8(pbuf, index, field, start,       &
      kount, col_type, copy_if_needed, errcode)
    use shr_kind_mod,                  only : r8 => shr_kind_r8
    type(physics_buffer_desc), pointer:: pbuf(:)
    integer,             intent(in)               :: index
    real(kind=r8), pointer                        :: field(:,:)
    integer,             intent(in),  optional    :: start(:),kount(:)
    integer,             intent(in),  optional    :: col_type
    logical,             intent(in),  optional    :: copy_if_needed
    integer,             intent(out), optional    :: errcode
    field => null( )
    if( present( errcode ) ) errcode = 0
  end subroutine get_pbuf1d_field_by_index_2d_r8

  subroutine get_pbuf1d_field_by_index_3d_r8(pbuf, index, field, start,       &
      kount, col_type, copy_if_needed, errcode)
    use shr_kind_mod,                  only : r8 => shr_kind_r8
    type(physics_buffer_desc), pointer:: pbuf(:)
    integer,             intent(in)               :: index
    real(kind=r8), pointer                        :: field(:,:,:)
    integer,             intent(in),  optional    :: start(:),kount(:)
    integer,             intent(in),  optional    :: col_type
    logical,             intent(in),  optional    :: copy_if_needed
    integer,             intent(out), optional    :: errcode
    field => null( )
    if( present( errcode ) ) errcode = 0
  end subroutine get_pbuf1d_field_by_index_3d_r8

end module physics_buffer
