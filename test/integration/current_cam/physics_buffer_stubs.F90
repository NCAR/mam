! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module physics_buffer

  use ppgrid,                          only : pcols, pver
  use shr_kind_mod,                    only : r8 => shr_kind_r8

  implicit none
  private
  public :: pbuf_init, pbuf_get_index, pbuf_get_field

  type, public :: physics_buffer_desc
  end type physics_buffer_desc

  interface pbuf_get_field
    module procedure :: get_pbuf1d_field_by_index_0d_int
    module procedure :: get_pbuf1d_field_by_index_1d_r8
    module procedure :: get_pbuf1d_field_by_index_2d_r8
    module procedure :: get_pbuf1d_field_by_index_3d_r8
  end interface

  ! number of modes
  integer, parameter :: nmodes = 4

  ! pbuf variables
  character(len=10), dimension(*), parameter :: kStateVariables = (/          &
      'num_c1    ', 'so4_c1    ', 'pom_c1    ', 'soa_c1    ', 'bc_c1     ',   &
      'dst_c1    ', 'ncl_c1    ', 'num_c2    ', 'so4_c2    ', 'soa_c2    ',   &
      'ncl_c2    ', 'dst_c2    ', 'num_c3    ', 'dst_c3    ', 'ncl_c3    ',   &
      'so4_c3    ', 'num_c4    ', 'pom_c4    ', 'bc_c4     ', 'ozone     ',   &
      'O2        ', 'CO2       ', 'N2O       ', 'CH4       ', 'CFC11     ',   &
      'CFC12     ', 'DGNUMWET  ', 'QAERWAT   ' /)

  ! index of variables in kStateVariables
  integer, parameter :: index_num_c1   = 1
  integer, parameter :: index_so4_c1   = 2
  integer, parameter :: index_pom_c1   = 3
  integer, parameter :: index_soa_c1   = 4
  integer, parameter :: index_bc_c1    = 5
  integer, parameter :: index_dst_c1   = 6
  integer, parameter :: index_ncl_c1   = 7
  integer, parameter :: index_num_c2   = 8
  integer, parameter :: index_so4_c2   = 9
  integer, parameter :: index_soa_c2   = 10
  integer, parameter :: index_ncl_c2   = 11
  integer, parameter :: index_dst_c2   = 12
  integer, parameter :: index_num_c3   = 13
  integer, parameter :: index_dst_c3   = 14
  integer, parameter :: index_ncl_c3   = 15
  integer, parameter :: index_so4_c3   = 16
  integer, parameter :: index_num_c4   = 17
  integer, parameter :: index_pom_c4   = 18
  integer, parameter :: index_bc_c4    = 19
  integer, parameter :: index_ozone    = 20
  integer, parameter :: index_O2       = 21
  integer, parameter :: index_CO2      = 22
  integer, parameter :: index_N2O      = 23
  integer, parameter :: index_CH4      = 24
  integer, parameter :: index_CFC11    = 25
  integer, parameter :: index_CFC12    = 26
  integer, parameter :: index_DGNUMWET = 27
  integer, parameter :: index_QAERWAT  = 28

  ! state data
  real(r8), dimension(pcols, pver, nmodes), target :: dgnumwet, qaerwat

contains

  ! initialize state data
  subroutine pbuf_init( pbuf )
    use test_utils,                    only : set_values
    type(physics_buffer_desc), pointer, intent(inout) :: pbuf(:)
    allocate( pbuf( 0 ) )
    call set_values( dgnumwet, 1.0e-6_r8, 0.5_r8 )
    call set_values( qaerwat,  1.0e-4_r8, 0.9_r8 )
  end subroutine pbuf_init

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
    !write(*,*) "int request for '"//trim( kStateVariables( index ) )//"'"
    field => null( )
    if( present( errcode ) ) errcode = 0
    stop 3
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
    !write(*,*) "*double request for '"//trim( kStateVariables( index ) )//"'"
    field => null( )
    if( present( errcode ) ) errcode = 0
    stop 3
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
    !write(*,*) "**double request for '"//trim( kStateVariables( index ) )//"'"
    field => null( )
    if( present( errcode ) ) errcode = 0
    stop 3
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
    !write(*,*) "***double request for '"//trim( kStateVariables( index ) )//"'"
    select case( index )
    case( index_DGNUMWET )
      field => dgnumwet
    case( index_QAERWAT )
      field => qaerwat
    case default
      field => null( )
      stop 3
    end select
    if( present( errcode ) ) errcode = 0
  end subroutine get_pbuf1d_field_by_index_3d_r8

end module physics_buffer
