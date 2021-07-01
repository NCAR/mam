! Copyright (C) 2021 National Center for Atmospheric Research
! SPDX-License-Identifier: Apache-2.0
!
!> \file
!> Tests for the mam_lookup_table_2D_1D module
program test_lookup_table_2D_1D

  use mam_lookup_table_2D_1D
  use musica_assert
  use musica_config,                   only : config_t
  use musica_constants,                only : r8 => musica_dk

  implicit none

  character(len=*), parameter :: my_name = "lookup table 2D 1D tests"
  type(lookup_table_2D_1D_t), pointer :: lookup_table
  type(config_t) :: table_config

  call table_config%empty( )
  call table_config%add( "file path", "lookup_2D_1D.ncf", my_name )
  call table_config%add( "table",     "table",            my_name )
  call table_config%add( "axis 1",    "var1",             my_name )
  call table_config%add( "axis 2",    "var2",             my_name )

  lookup_table => null( )
  lookup_table => lookup_table_2D_1D_t( table_config )
  call assert( 929900123, associated( lookup_table ) )

  call test_get( lookup_table, 1.0_r8, 10.0_r8, 1.0_r8,   1, 1 )
  call test_get( lookup_table, 3.0_r8, 20.0_r8, 30.0_r8,  3, 2 )
  call test_get( lookup_table, 2.0_r8, 30.0_r8, 200.0_r8, 2, 3 )
  call test_get( lookup_table, 4.0_r8, 30.0_r8, 400.0_r8, 4, 3 )

  call test_get( lookup_table, 2.5_r8, 20.0_r8, 25.0_r8,  2, 2 )
  call test_get( lookup_table, 2.0_r8, 25.0_r8, 110.0_r8, 2, 2 )

  !> \todo figure out test for lookups in between points on both axes

  deallocate( lookup_table )

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine test_get( lookup_table, variable_1, variable_2, true_value,     &
      index_1, index_2 )

    use musica_string,                 only : string_t, to_char

    type(lookup_table_2D_1D_t), intent(in) :: lookup_table
    real(kind=r8),              intent(in) :: variable_1
    real(kind=r8),              intent(in) :: variable_2
    real(kind=r8),              intent(in) :: true_value
    integer,                    intent(in) :: index_1
    integer,                    intent(in) :: index_2

    integer :: i1, i2, idx1, idx2
    type(string_t) :: msg, msg_base
    real(kind=r8) :: results(1)

    msg_base = "var1: "      //trim( to_char( variable_1 ) )//               &
               " var2: "     //trim( to_char( variable_2 ) )//               &
               " idx1: "     //trim( to_char( index_1    ) )//               &
               " 1dx2: "     //trim( to_char( index_2    ) )//               &
               " exptected: "//trim( to_char( true_value ) )

    ! get lookup value without guesses
    call lookup_table%get( variable_1, variable_2, results )
    call assert_msg( 908280256, almost_equal( results(1), true_value ),      &
                     msg_base//" got: "//to_char( results( 1 ) ) )

    ! get lookup value by guessing each index for each axis individually and
    ! in combination
    do i1 = 1, 4
      do i2 = 1, 3
        idx1 = i1
        idx2 = i2
        msg = msg_base//" i1: "//trim( to_char( i1 ) )//" i2: "//            &
              trim( to_char( i2 ) )
        call lookup_table%get( variable_1, variable_2, results,              &
                               variable_1_index = idx1 )
        call assert_msg( 179915997, almost_equal( results(1), true_value ),  &
                         msg//" got: "//trim( to_char( results(1) ) )//      &
                         " got idx1: "//trim( to_char( idx1 ) ) )
        call lookup_table%get( variable_1, variable_2, results,              &
                               variable_2_index = idx2 )
        call assert_msg( 554104658, almost_equal( results(1), true_value ),  &
                         msg//" got: "//trim( to_char( results(1) ) )//      &
                         " got idx2: "//trim( to_char( idx2 ) ) )
        idx1 = i1
        idx2 = i2
        call lookup_table%get( variable_1, variable_2, results,              &
                               variable_1_index = idx1,                      &
                               variable_2_index = idx2 )
        call assert_msg( 554104658, almost_equal( results(1), true_value ),  &
                         msg//" got: "//trim( to_char( results(1) ) )//      &
                         " got idx1: "//trim( to_char( idx1 ) )//            &
                         " got idx2: "//trim( to_char( idx2 ) ) )
      end do
    end do

  end subroutine test_get

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end program test_lookup_table_2D_1D
