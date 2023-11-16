module TestGeomUtil
  use KindModule, only: I4B, DP
  use testdrive, only: check, error_type, new_unittest, test_failed, &
                       to_string, unittest_type
  use GeomUtilModule, only: get_node, get_ijk, get_jk, point_in_polygon
  use ConstantsModule, only: LINELENGTH
  implicit none
  private
  public :: collect_geomutil

contains

  subroutine collect_geomutil(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
                new_unittest("get_node_get_ijk", &
                             test_get_node_get_ijk), &
                ! new_unittest("get_node_get_jk", &
                !             test_get_node_get_jk), &
                new_unittest("point_in_polygon_sq", &
                             test_point_in_polygon_sq), &
                new_unittest("point_in_polygon_tri", &
                             test_point_in_polygon_tri), &
                new_unittest("point_in_polygon_irr", &
                             test_point_in_polygon_irr) &
                ]
  end subroutine collect_geomutil

  subroutine test_get_node_get_ijk(error)
    type(error_type), allocatable, intent(out) :: error
    integer :: ilay
    integer :: irow
    integer :: icol
    integer :: nlay
    integer :: nrow
    integer :: ncol
    integer :: nnum
    integer :: ncls
    integer :: k, i, j

    ! trivial grid with 1 cell
    nnum = get_node(1, 1, 1, 1, 1, 1)
    call get_ijk(nnum, 1, 1, 1, ilay, irow, icol)
    call check(error, nnum == 1)
    call check(error, ilay == 1)
    call check(error, irow == 1)
    call check(error, icol == 1)
    if (allocated(error)) return

    ! small grid, 3x4x5
    nlay = 3
    nrow = 4
    ncol = 5
    ncls = nlay * nrow * ncol
    do k = 1, nlay
      do i = 1, nrow
        do j = 1, ncol
          ! node number from ijk
          nnum = get_node(k, i, j, nlay, nrow, ncol)
          call check(error, nnum == (k - 1) * nrow * ncol + (i - 1) * ncol + j)
          if (allocated(error)) return

          ! ijk from node number
          call get_ijk(nnum, nrow, ncol, nlay, irow, icol, ilay)
          call check(error, ilay == k)
          call check(error, irow == i)
          call check(error, icol == j)
          if (allocated(error)) return
        end do
      end do
    end do
  end subroutine test_get_node_get_ijk

  subroutine test_point_in_polygon(error, shape, &
                                   poly, in_pts, out_pts, vert_pts, face_pts)
    type(error_type), allocatable, intent(inout) :: error
    character(len=*), intent(in) :: shape
    real(DP), allocatable, intent(in) :: poly(:, :)
    real(DP), allocatable, intent(in) :: in_pts(:, :)
    real(DP), allocatable, intent(in) :: out_pts(:, :)
    real(DP), allocatable, intent(in) :: vert_pts(:, :)
    real(DP), allocatable, intent(in) :: face_pts(:, :)
    integer(I4B) :: i
    real(DP) :: x, y

    ! test inside points
    do i = 1, size(in_pts, 1)
      x = in_pts(i, 1)
      y = in_pts(i, 2)
      call check(error, point_in_polygon(x, y, poly), &
                 "point inside "//shape//" failed: " &
                 //to_string(x)//", "//to_string(y))
      if (allocated(error)) return
    end do

    ! test outside points
    do i = 1, size(out_pts, 1)
      x = out_pts(i, 1)
      y = out_pts(i, 2)
      call check(error, (.not. point_in_polygon(x, y, poly)), &
                 "point outside "//shape//" failed: " &
                 //to_string(x)//", "//to_string(y))
      if (allocated(error)) return
    end do

    ! test vertex points
    do i = 1, size(vert_pts, 1)
      x = vert_pts(i, 1)
      y = vert_pts(i, 2)
      call check(error, point_in_polygon(x, y, poly), &
                 "point on "//shape//" vertex failed: " &
                 //to_string(x)//", "//to_string(y))
      if (allocated(error)) return
    end do

    ! test face points
    do i = 1, size(face_pts, 1)
      x = face_pts(i, 1)
      y = face_pts(i, 2)
      call check(error, point_in_polygon(x, y, poly), &
                 "point on "//shape//" face failed: " &
                 //to_string(x)//", "//to_string(y))
      if (allocated(error)) return
    end do
  end subroutine test_point_in_polygon

  !> @brief Test a unit square
  subroutine test_point_in_polygon_sq(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP), allocatable :: poly(:, :)
    real(DP), allocatable :: in_pts(:, :)
    real(DP), allocatable :: out_pts(:, :)
    real(DP), allocatable :: vert_pts(:, :)
    real(DP), allocatable :: face_pts(:, :)

    allocate (poly(4, 2))

    allocate (in_pts(3, 2))
    in_pts(1, :) = (/0.99_DP, 0.01_DP/)
    in_pts(2, :) = (/0.5_DP, 0.5_DP/)
    in_pts(3, :) = (/0.0001_DP, 0.9999_DP/)

    allocate (out_pts(2, 2))
    out_pts(1, :) = (/0.5_DP, 1.00001_DP/)
    out_pts(2, :) = (/-0.5_DP, 34.0_DP/)

    allocate (vert_pts(4, 2))
    vert_pts(1, :) = (/0.0_DP, 0.0_DP/)
    vert_pts(2, :) = (/1.0_DP, 0.0_DP/)
    vert_pts(3, :) = (/0.0_DP, 1.0_DP/)
    vert_pts(4, :) = (/1.0_DP, 1.0_DP/)

    allocate (face_pts(4, 2))
    face_pts(1, :) = (/0.0_DP, 0.5_DP/)
    face_pts(2, :) = (/0.5_DP, 0.0_DP/)
    face_pts(3, :) = (/1.0_DP, 0.5_DP/)
    face_pts(4, :) = (/0.5_DP, 1.0_DP/)

    poly(1, :) = (/0.0_DP, 0.0_DP/)
    poly(2, :) = (/0.0_DP, 1.0_DP/)
    poly(3, :) = (/1.0_DP, 1.0_DP/)
    poly(4, :) = (/1.0_DP, 0.0_DP/)
    call test_point_in_polygon(error, "clockwise square", &
                               poly, in_pts, out_pts, vert_pts, face_pts)
    if (allocated(error)) return

    poly(1, :) = (/0.0_DP, 0.0_DP/)
    poly(2, :) = (/1.0_DP, 0.0_DP/)
    poly(3, :) = (/1.0_DP, 1.0_DP/)
    poly(4, :) = (/0.0_DP, 1.0_DP/)
    call test_point_in_polygon(error, "counter-clockwise square", &
                               poly, in_pts, out_pts, vert_pts, face_pts)
    if (allocated(error)) return

    deallocate (poly)
    deallocate (in_pts)
    deallocate (out_pts)
    deallocate (vert_pts)
    deallocate (face_pts)
  end subroutine test_point_in_polygon_sq

  !> @brief Test a right triangle
  subroutine test_point_in_polygon_tri(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP), allocatable :: poly(:, :)
    real(DP), allocatable :: in_pts(:, :)
    real(DP), allocatable :: out_pts(:, :)
    real(DP), allocatable :: vert_pts(:, :)
    real(DP), allocatable :: face_pts(:, :)

    allocate (poly(3, 2))

    allocate (in_pts(3, 2))
    in_pts(1, :) = (/0.8_DP, 0.0001_DP/)
    in_pts(2, :) = (/0.5_DP, 0.49999_DP/)
    in_pts(3, :) = (/0.0001_DP, 0.8_DP/)

    allocate (out_pts(2, 2))
    out_pts(1, :) = (/0.5_DP, 0.50001_DP/)
    out_pts(2, :) = (/-0.5_DP, 34.0_DP/)

    allocate (vert_pts(3, 2))
    vert_pts(1, :) = (/0.0_DP, 0.0_DP/)
    vert_pts(2, :) = (/1.0_DP, 0.0_DP/)
    vert_pts(3, :) = (/0.0_DP, 1.0_DP/)

    allocate (face_pts(3, 2))
    face_pts(1, :) = (/0.0_DP, 0.5_DP/)
    face_pts(2, :) = (/0.5_DP, 0.0_DP/)
    face_pts(3, :) = (/0.5_DP, 0.5_DP/)

    poly(1, :) = (/0.0_DP, 0.0_DP/)
    poly(2, :) = (/0.0_DP, 1.0_DP/)
    poly(3, :) = (/1.0_DP, 0.0_DP/)
    call test_point_in_polygon(error, "clockwise triangle", &
                               poly, in_pts, out_pts, vert_pts, face_pts)
    if (allocated(error)) return

    poly(1, :) = (/0.0_DP, 0.0_DP/)
    poly(2, :) = (/1.0_DP, 0.0_DP/)
    poly(3, :) = (/0.0_DP, 1.0_DP/)
    call test_point_in_polygon(error, "counter-clockwise triangle", &
                               poly, in_pts, out_pts, vert_pts, face_pts)
    if (allocated(error)) return

    deallocate (poly)
    deallocate (in_pts)
    deallocate (out_pts)
    deallocate (vert_pts)
    deallocate (face_pts)
  end subroutine test_point_in_polygon_tri

  !> @brief Test an irregular polygon
  subroutine test_point_in_polygon_irr(error)
    type(error_type), allocatable, intent(out) :: error
    real(DP), allocatable :: poly(:, :)
    real(DP), allocatable :: in_pts(:, :)
    real(DP), allocatable :: out_pts(:, :)
    real(DP), allocatable :: vert_pts(:, :)
    real(DP), allocatable :: face_pts(:, :)

    allocate (poly(5, 2))

    allocate (in_pts(3, 2))
    in_pts(1, :) = (/0.5_DP, 0.1_DP/)
    in_pts(2, :) = (/0.5_DP, 0.49_DP/)
    in_pts(3, :) = (/1.999_DP, 1.999_DP/)

    allocate (out_pts(3, 2))
    out_pts(1, :) = (/0.5_DP, -0.1_DP/)
    out_pts(2, :) = (/0.5_DP, 0.51_DP/)
    out_pts(3, :) = (/-0.5_DP, 34.0_DP/)

    allocate (vert_pts(5, 2))
    vert_pts(1, :) = (/0.0_DP, 0.0_DP/)
    vert_pts(2, :) = (/1.0_DP, 1.0_DP/)
    vert_pts(3, :) = (/1.0_DP, 2.0_DP/)
    vert_pts(4, :) = (/2.0_DP, 2.0_DP/)
    vert_pts(5, :) = (/2.0_DP, 0.0_DP/)

    allocate (face_pts(3, 2))
    face_pts(1, :) = (/0.5_DP, 0.5_DP/)
    face_pts(2, :) = (/2.0_DP, 1.0_DP/)
    face_pts(3, :) = (/1.5_DP, 2.0_DP/)

    poly(1, :) = (/0.0_DP, 0.0_DP/)
    poly(2, :) = (/1.0_DP, 1.0_DP/)
    poly(3, :) = (/1.0_DP, 2.0_DP/)
    poly(4, :) = (/2.0_DP, 2.0_DP/)
    poly(5, :) = (/2.0_DP, 0.0_DP/)
    call test_point_in_polygon(error, &
                               "clockwise irregular polygon", &
                               poly, in_pts, out_pts, vert_pts, face_pts)
    if (allocated(error)) return

    poly(1, :) = (/0.0_DP, 0.0_DP/)
    poly(2, :) = (/2.0_DP, 0.0_DP/)
    poly(3, :) = (/2.0_DP, 2.0_DP/)
    poly(4, :) = (/1.0_DP, 2.0_DP/)
    poly(5, :) = (/1.0_DP, 1.0_DP/)
    call test_point_in_polygon(error, &
                               "counter-clockwise irregular polygon", &
                               poly, in_pts, out_pts, vert_pts, face_pts)
    if (allocated(error)) return

    deallocate (poly)
    deallocate (in_pts)
    deallocate (out_pts)
    deallocate (vert_pts)
    deallocate (face_pts)
  end subroutine test_point_in_polygon_irr

end module TestGeomUtil
