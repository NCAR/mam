! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Modules containing stub functions and global parameters
!! for tests of current MAM4 code
module constituents
  implicit none
  private
  public :: cnst_get_ind
  integer, parameter, public :: pcnst = 100 ! number of advected constituents
  character(len=10), parameter :: q_names(15) = &
    (/ 'so4_a1    ', 'pom_a1    ', 'soa_a1    ', 'bc_a1     ',&
       'dst_a1    ', 'ncl_a1    ', 'so4_a2    ', 'soa_a2    ',&
       'ncl_a2    ', 'dst_a2    ', 'dst_a3    ', 'ncl_a3    ',&
       'so4_a3    ', 'pom_a4    ', 'bc_a4     ' /)
contains
  subroutine cnst_get_ind(name, ind, abort)
    character(len=*),  intent(in)  :: name
    integer,           intent(out) :: ind
    logical, optional, intent(in)  :: abort
    do ind = 1, size( q_names )
      if( trim( name ) .eq. trim( q_names( ind ) ) ) return
    end do
    write(*,*) "WARNING: Unknown state variable requested: '"//trim( name )
    ind = 10000000 ! cause a seq fault if this is used anywhere
  end subroutine cnst_get_ind
end module constituents
