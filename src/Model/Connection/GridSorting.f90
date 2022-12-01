module GridSorting
  use KindModule, only: I4B, DP, LGP
  use ConstantsModule, only: DHALF
  use CellWithNbrsModule, only: GlobalCellType
  use GenericUtilitiesModule, only: is_same
  use BaseDisModule, only: dis_transform_xy
  implicit none
  private

  public :: quickSortGrid

contains
  ! Sort an array of integers
  subroutine quickSortGrid(array, arraySize, idxToGlobal)
    integer, intent(inout), dimension(:) :: array
    integer, intent(in) :: arraySize
    type(GlobalCellType), dimension(:), pointer :: idxToGlobal
    ! local
    integer :: QSORT_THRESHOLD = 8
    include "qsort_inline.inc"

  contains
    subroutine init()
    end subroutine init

    ! Compare two grid cells, this doesn't work as
    ! smooth for staggered discretizations though...
    function lessThan(n, m) result(isLess)
      integer(I4B), intent(in) :: n
      integer(I4B), intent(in) :: m
      logical(LGP) :: isLess
      ! local
      type(GlobalCellType), pointer :: gcn, gcm
      real(DP) :: xn, yn, zn, xm, ym, zm
      real(DP), dimension(:), pointer, contiguous :: dis_top_n, dis_bot_n, &
                                                     dis_top_m, dis_bot_m
      real(DP), dimension(:), pointer, contiguous :: dis_xc_n, dis_yc_n, &
                                                     dis_xc_m, dis_yc_m
      real(DP), pointer :: xorigin_n, yorigin_n, angrot_n, &
                           xorigin_m, yorigin_m, angrot_m

      ! get coordinates
      gcn => idxToGlobal(array(n))
      gcm => idxToGlobal(array(m))

      ! load model data
      ! TODO_MJR: we should probably cache this
      ! for n:
      call gcn%dmodel%load(dis_top_n, 'TOP', 'DIS')
      call gcn%dmodel%load(dis_bot_n, 'BOT', 'DIS')
      call gcn%dmodel%load(dis_xc_n, 'XC', 'DIS')
      call gcn%dmodel%load(dis_yc_n, 'YC', 'DIS')
      call gcn%dmodel%load(xorigin_n, 'XORIGIN', 'DIS')
      call gcn%dmodel%load(yorigin_n, 'YORIGIN', 'DIS')
      call gcn%dmodel%load(angrot_n, 'ANGROT', 'DIS')
      ! for m:
      call gcm%dmodel%load(dis_top_m, 'TOP', 'DIS')
      call gcm%dmodel%load(dis_bot_m, 'BOT', 'DIS')
      call gcm%dmodel%load(dis_xc_m, 'XC', 'DIS')
      call gcm%dmodel%load(dis_yc_m, 'YC', 'DIS')
      call gcm%dmodel%load(xorigin_m, 'XORIGIN', 'DIS')
      call gcm%dmodel%load(yorigin_m, 'YORIGIN', 'DIS')
      call gcm%dmodel%load(angrot_m, 'ANGROT', 'DIS')

      ! convert coordinates
      call dis_transform_xy(dis_xc_n(gcn%index), dis_yc_n(gcn%index), &
                            xorigin_n, yorigin_n, angrot_n, &
                            xn, yn)
      zn = DHALF * (dis_top_n(gcn%index) + &
                    dis_bot_n(gcn%index))

      call dis_transform_xy(dis_xc_m(gcm%index), dis_yc_m(gcm%index), &
                            xorigin_m, yorigin_m, angrot_m, &
                            xm, ym)
      zm = DHALF * (dis_top_m(gcm%index) + &
                    dis_bot_m(gcm%index))

      ! compare
      if (.not. is_same(zn, zm, 10 * epsilon(zn))) then
        isLess = zn > zm
      else if (.not. is_same(yn, ym, 10 * epsilon(yn))) then
        isLess = yn > ym
      else if (.not. is_same(xn, xm, 10 * epsilon(xn))) then
        isLess = xn < xm
      else
        isLess = .false.
      end if

    end function lessThan

    ! swap indices
    subroutine swap(a, b)
      integer, intent(in) :: a, b
      integer :: hold

      hold = array(a)
      array(a) = array(b)
      array(b) = hold

    end subroutine swap

    ! circular shift-right by one
    subroutine rshift(left, right)
      integer, intent(in) :: left, right
      integer :: hold

      hold = array(right)
      array(left + 1:right) = array(left:right - 1)
      array(left) = hold

    end subroutine rshift
  end subroutine quickSortGrid
end module GridSorting
