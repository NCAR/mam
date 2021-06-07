! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Module containing mpi stub functions and global parameters
module shr_mpi_mod

  use pio,                             only : pio_offset_kind
  use shr_kind_mod,                    only : shr_kind_in

  implicit none
  private

  public :: shr_mpi_bcast, shr_mpi_chkerr

  interface shr_mpi_bcast
    module procedure shr_mpi_bcasti0
    module procedure shr_mpi_bcasti1
    module procedure shr_mpi_bcast_pio_offset
    module procedure shr_mpi_bcastl0
    module procedure shr_mpi_bcastl1
  end interface

contains

  subroutine shr_mpi_chkerr(ierr, msg)
    integer,          intent(in) :: ierr
    character(len=*), intent(in) :: msg
  end subroutine shr_mpi_chkerr

  subroutine shr_mpi_bcasti0(buf, comm)
    integer(shr_kind_in), intent(in) :: buf
    integer(shr_kind_in), intent(in) :: comm
  end subroutine shr_mpi_bcasti0

  subroutine shr_mpi_bcasti1(buf, comm)
    integer(shr_kind_in), intent(in) :: buf(:)
    integer(shr_kind_in), intent(in) :: comm
  end subroutine shr_mpi_bcasti1

  subroutine shr_mpi_bcast_pio_offset(buf, comm)
    integer(pio_offset_kind), intent(in) :: buf
    integer(shr_kind_in),     intent(in) :: comm
  end subroutine shr_mpi_bcast_pio_offset

  subroutine shr_mpi_bcastl0(buf, comm)
    logical,              intent(in) :: buf
    integer(shr_kind_in), intent(in) :: comm
  end subroutine shr_mpi_bcastl0

  subroutine shr_mpi_bcastl1(buf, comm)
    logical,              intent(in) :: buf(:)
    integer(shr_kind_in), intent(in) :: comm
  end subroutine shr_mpi_bcastl1

end module shr_mpi_mod
