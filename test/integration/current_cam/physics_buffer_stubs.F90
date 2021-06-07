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

contains

  integer function pbuf_get_index(name, code)
    character(len=*), intent(in) :: name
    integer, intent(out), optional :: code
    pbuf_get_index = 1
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
  end subroutine get_pbuf1d_field_by_index_3d_r8

end module physics_buffer
