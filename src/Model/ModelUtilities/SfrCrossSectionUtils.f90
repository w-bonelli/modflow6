!> @brief This module contains stateless sfr subroutines and functions
!!
!! This module contains the functions to calculate the wetted perimeter
!! and cross-sectional area for a reach cross-section that are used in
!! the streamflow routing (SFR) package. It also contains subroutines to
!! calculate the wetted perimeter and cross-sectional area for each
!! line segment in the cross-section. This module does not depend on the
!! SFR package.
!!
!<
module GwfSfrCrossSectionUtilsModule

  use KindModule, only: DP, I4B
  use ConstantsModule, only: DZERO, DHALF, DTWOTHIRDS, DONE, DTWO

  implicit none
  private
  public :: get_saturated_topwidth
  public :: get_wetted_topwidth
  public :: get_wetted_perimeter
  public :: get_cross_section_area
  public :: get_hydraulic_radius
  public :: get_mannings_section

contains

  !> @brief Calculate the saturated top width for a reach
  !!
  !! Function to calculate the maximum top width for a reach using the
  !! cross-section station data.
  !!
  !! @return      w               saturated top width
  !<
  function get_saturated_topwidth(npts, stations) result(w)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts !< number of station-height data for a reach
    real(DP), dimension(npts), intent(in) :: stations !< cross-section station distances (x-distance)
    ! -- local variables
    real(DP) :: w
    !
    ! -- calculate the saturated top width
    if (npts > 1) then
      w = stations(npts) - stations(1)
    else
      w = stations(1)
    end if
    !
    ! -- return
    return
  end function get_saturated_topwidth

  !> @brief Calculate the wetted top width for a reach
  !!
  !! Function to calculate the wetted top width for a reach using the
  !! cross-section station-height data given a passed depth.
  !!
  !! @return      w               wetted top width
  !<
  function get_wetted_topwidth(npts, stations, heights, d) result(w)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts !< number of station-height data for a reach
    real(DP), dimension(npts), intent(in) :: stations !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: heights !< cross-section height data
    real(DP), intent(in) :: d !< depth to evaluate cross-section
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: w
    real(DP), dimension(npts - 1) :: widths
    !
    ! -- intitialize the wetted perimeter for the reach
    w = DZERO
    !
    ! -- calculate the wetted top width for each line segment
    call get_wetted_topwidths(npts, stations, heights, d, widths)
    !
    ! -- calculate the wetted top widths
    do n = 1, npts - 1
      w = w + widths(n)
    end do
    !
    ! -- return
    return
  end function get_wetted_topwidth

  !> @brief Calculate the wetted perimeter for a reach
  !!
  !! Function to calculate the wetted perimeter for a reach using the
  !! cross-section station-height data given a passed depth.
  !!
  !! @return      p               wetted perimeter
  !<
  function get_wetted_perimeter(npts, stations, heights, d) result(p)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts !< number of station-height data for a reach
    real(DP), dimension(npts), intent(in) :: stations !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: heights !< cross-section height data
    real(DP), intent(in) :: d !< depth to evaluate cross-section
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: p
    real(DP), dimension(npts - 1) :: perimeters
    !
    ! -- intitialize the wetted perimeter for the reach
    p = DZERO
    !
    ! -- calculate the wetted perimeter for each line segment
    call get_wetted_perimeters(npts, stations, heights, d, perimeters)
    !
    ! -- calculate the wetted perimenter
    do n = 1, npts - 1
      p = p + perimeters(n)
    end do
    !
    ! -- return
    return
  end function get_wetted_perimeter

  !> @brief Calculate the cross-sectional area for a reach
  !!
  !! Function to calculate the cross-sectional area for a reach using
  !! the cross-section station-height data given a passed depth.
  !!
  !! @return      a               cross-sectional area
  !<
  function get_cross_section_area(npts, stations, heights, d) result(a)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts !< number of station-height data for a reach
    real(DP), dimension(npts), intent(in) :: stations !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: heights !< cross-section height data
    real(DP), intent(in) :: d !< depth to evaluate cross-section
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: a
    real(DP), dimension(npts - 1) :: areas
    !
    ! -- intitialize the area
    a = DZERO
    !
    ! -- calculate the cross-sectional area for each line segment
    call get_cross_section_areas(npts, stations, heights, d, areas)
    !
    ! -- calculate the cross-sectional area
    do n = 1, npts - 1
      a = a + areas(n)
    end do
    !
    ! -- return
    return
  end function get_cross_section_area

  !> @brief Calculate the hydraulic radius for a reach
  !!
  !! Function to calculate the hydraulic radius for a reach using
  !! the cross-section station-height data given a passed depth.
  !!
  !! @return      r               hydraulic radius
  !<
  function get_hydraulic_radius(npts, stations, heights, d) result(r)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts !< number of station-height data for a reach
    real(DP), dimension(npts), intent(in) :: stations !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: heights !< cross-section height data
    real(DP), intent(in) :: d !< depth to evaluate cross-section
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: r
    real(DP) :: p
    real(DP) :: a
    real(DP), dimension(npts - 1) :: areas
    real(DP), dimension(npts - 1) :: perimeters
    !
    ! -- intitialize the hydraulic radius, perimeter, and area
    r = DZERO
    p = DZERO
    a = DZERO
    !
    ! -- calculate the wetted perimeter for each line segment
    call get_wetted_perimeters(npts, stations, heights, d, perimeters)
    !
    ! -- calculate the wetted perimenter
    do n = 1, npts - 1
      p = p + perimeters(n)
    end do
    !
    ! -- calculate the hydraulic radius only if the perimeter is non-zero
    if (p > DZERO) then
      !
      ! -- calculate the cross-sectional area for each line segment
      call get_cross_section_areas(npts, stations, heights, d, areas)
      !
      ! -- calculate the cross-sectional area
      do n = 1, npts - 1
        a = a + areas(n)
      end do
      !
      ! -- calculate the hydraulic radius
      r = a / p
    end if
    !
    ! -- return
    return
  end function get_hydraulic_radius

  !> @brief Calculate the manning's discharge for a reach
  !!
  !! Function to calculate the mannings discharge for a reach
  !! by calculating the discharge for each section, which can
  !! have a unique Manning's coefficient given a passed depth.
  !!
  !! @return      q               reach discharge
  !<
  function get_mannings_section(npts, stations, heights, roughfracs, &
                                roughness, conv_fact, slope, d) result(q)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts !< number of station-height data for a reach
    real(DP), dimension(npts), intent(in) :: stations !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: heights !< cross-section height data
    real(DP), dimension(npts), intent(in) :: roughfracs !< cross-section Mannings roughness fraction data
    real(DP), intent(in) :: roughness !< base reach roughness
    real(DP), intent(in) :: conv_fact !< unit conversion factor
    real(DP), intent(in) :: slope !< reach slope
    real(DP), intent(in) :: d !< depth to evaluate cross-section
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: q
    real(DP) :: rh
    real(DP) :: r
    real(DP) :: p
    real(DP) :: a
    real(DP), dimension(npts - 1) :: areas
    real(DP), dimension(npts - 1) :: perimeters
    !
    ! -- intitialize the hydraulic radius, perimeter, and area
    q = DZERO
    rh = DZERO
    r = DZERO
    p = DZERO
    a = DZERO
    !
    ! -- calculate the wetted perimeter for each line segment
    call get_wetted_perimeters(npts, stations, heights, d, perimeters)
    !
    ! -- calculate the wetted perimenter
    do n = 1, npts - 1
      p = p + perimeters(n)
    end do
    !
    ! -- calculate the hydraulic radius only if the perimeter is non-zero
    if (p > DZERO) then
      !
      ! -- calculate the cross-sectional area for each line segment
      call get_cross_section_areas(npts, stations, heights, d, areas)
      !
      ! -- calculate the cross-sectional area
      do n = 1, npts - 1
        p = perimeters(n)
        r = roughness * roughfracs(n)
        if (p * r > DZERO) then
          a = areas(n)
          rh = a / p
          q = q + conv_fact * a * rh**DTWOTHIRDS * sqrt(slope) / r
        end if
      end do
    end if
    !
    ! -- return
    return
  end function get_mannings_section

  ! -- private functions and subroutines

  !> @brief Calculate the wetted perimeters for each line segment
  !!
  !! Subroutine to calculate the wetted perimeter for each line segment
  !! that defines the reach using the cross-section station-height
  !! data given a passed depth.
  !!
  !<
  subroutine get_wetted_perimeters(npts, stations, heights, d, p)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts !< number of station-height data for a reach
    real(DP), dimension(npts), intent(in) :: stations !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: heights !< cross-section height data
    real(DP), intent(in) :: d !< depth to evaluate cross-section
    real(DP), dimension(npts - 1), intent(inout) :: p !< wetted perimeter for each line segment
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: x0
    real(DP) :: x1
    real(DP) :: d0
    real(DP) :: d1
    real(DP) :: dmax
    real(DP) :: dmin
    real(DP) :: xlen
    real(DP) :: dlen
    !
    ! -- iterate over the station-height data
    do n = 1, npts - 1
      !
      ! -- initialize the wetted perimeter
      p(n) = DZERO
      !
      ! -- initialize station-height data for segment
      x0 = stations(n)
      x1 = stations(n + 1)
      d0 = heights(n)
      d1 = heights(n + 1)
      !
      ! -- get the start and end station position of the wetted segment
      call get_wetted_station(x0, x1, d0, d1, dmax, dmin, d)
      !
      ! -- calculate the wetted perimeter for the segment
      xlen = x1 - x0
      dlen = DZERO
      if (xlen > DZERO) then
        if (d > dmax) then
          dlen = dmax - dmin
        else
          dlen = d - dmin
        end if
      else
        if (d > dmin) then
          dlen = min(d, dmax) - dmin
        else
          dlen = DZERO
        end if
      end if
      p(n) = sqrt(xlen**DTWO + dlen**DTWO)
    end do
    !
    ! -- return
    return
  end subroutine get_wetted_perimeters

  !> @brief Calculate the cross-sectional areas for each line segment
  !!
  !! Subroutine to calculate the cross-sectional area for each line segment
  !! that defines the reach using the cross-section station-height
  !! data given a passed depth.
  !!
  !<
  subroutine get_cross_section_areas(npts, stations, heights, d, a)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts !< number of station-height data for a reach
    real(DP), dimension(npts), intent(in) :: stations !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: heights !< cross-section height data
    real(DP), intent(in) :: d !< depth to evaluate cross-section
    real(DP), dimension(npts - 1), intent(inout) :: a !< cross-sectional area for each line segment
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: x0
    real(DP) :: x1
    real(DP) :: d0
    real(DP) :: d1
    real(DP) :: dmax
    real(DP) :: dmin
    real(DP) :: xlen
    !
    ! -- iterate over the station-height data
    do n = 1, npts - 1
      !
      ! -- initialize the cross-sectional area
      a(n) = DZERO
      !
      ! -- initialize station-height data for segment
      x0 = stations(n)
      x1 = stations(n + 1)
      d0 = heights(n)
      d1 = heights(n + 1)
      !
      ! -- get the start and end station position of the wetted segment
      call get_wetted_station(x0, x1, d0, d1, dmax, dmin, d)
      !
      ! -- calculate the cross-sectional area for the segment
      xlen = x1 - x0
      if (xlen > DZERO) then
        !
        ! -- add the area above dmax
        if (d > dmax) then
          a(n) = xlen * (d - dmax)
        end if
        !
        ! -- add the area below dmax
        if (dmax /= dmin .and. d > dmin) then
          a(n) = a(n) + DHALF * (d - dmin)
        end if
      end if
    end do
    !
    ! -- return
    return
  end subroutine get_cross_section_areas

  !> @brief Calculate the wetted top widths for each line segment
  !!
  !! Subroutine to calculate the wetted top width for each line segment
  !! that defines the reach using the cross-section station-height
  !! data given a passed depth.
  !!
  !<
  subroutine get_wetted_topwidths(npts, stations, heights, d, w)
    ! -- dummy variables
    integer(I4B), intent(in) :: npts !< number of station-height data for a reach
    real(DP), dimension(npts), intent(in) :: stations !< cross-section station distances (x-distance)
    real(DP), dimension(npts), intent(in) :: heights !< cross-section height data
    real(DP), intent(in) :: d !< depth to evaluate cross-section
    real(DP), dimension(npts - 1), intent(inout) :: w !< wetted top widths for each line segment
    ! -- local variables
    integer(I4B) :: n
    real(DP) :: x0
    real(DP) :: x1
    real(DP) :: d0
    real(DP) :: d1
    real(DP) :: dmax
    real(DP) :: dmin
    !
    ! -- iterate over the station-height data
    do n = 1, npts - 1
      !
      ! -- initialize station-height data for segment
      x0 = stations(n)
      x1 = stations(n + 1)
      d0 = heights(n)
      d1 = heights(n + 1)
      !
      ! -- get the start and end station position of the wetted segment
      call get_wetted_station(x0, x1, d0, d1, dmax, dmin, d)
      !
      ! -- calculate the wetted top width for the segment
      w(n) = x1 - x0
    end do
    !
    ! -- return
    return
  end subroutine get_wetted_topwidths

  !> @brief Calculate the station values for the wetted portion of the cross-section
  !!
  !! Subroutine to calculate the station values that define the extent of the
  !! wetted portion of the cross section for a line segment. The left (x0) and
  !! right (x1) station positions are altered if the passed depth is less
  !! than the maximum line segment depth. If the line segment is dry the left
  !! and right station are equal. Otherwise the wetted station values are equal
  !! to the full line segment or smaller if the passed depth is less than
  !! the maximum line segment depth.
  !!
  !<
  pure subroutine get_wetted_station(x0, x1, d0, d1, dmax, dmin, d)
    ! -- dummy variables
    real(DP), intent(inout) :: x0 !< left station position
    real(DP), intent(inout) :: x1 !< right station position
    real(DP), intent(in) :: d0 !< depth at the left station
    real(DP), intent(in) :: d1 !< depth at the right station
    real(DP), intent(inout) :: dmax !< maximum depth
    real(DP), intent(inout) :: dmin !< minimum depth
    real(DP), intent(in) :: d !< depth to evaluate cross-section
    ! -- local variables
    real(DP) :: xlen
    real(DP) :: dlen
    real(DP) :: slope
    real(DP) :: dx
    real(DP) :: xt
    real(DP) :: xt0
    real(DP) :: xt1
    !
    ! -- calculate the minimum and maximum depth
    dmin = min(d0, d1)
    dmax = max(d0, d1)
    !
    ! -- if d is less than or equal to the minimum value the
    !    station length (xlen) is zero
    if (d <= dmin) then
      x1 = x0
      ! -- if d is between dmin and dmax station length is less
      !    than d1 - d0
    else if (d < dmax) then
      xlen = x1 - x0
      dlen = d1 - d0
      if (abs(dlen) > DZERO) then
        slope = xlen / dlen
      else
        slope = DZERO
      end if
      if (d0 > d1) then
        dx = (d - d1) * slope
        xt = x1 + dx
        xt0 = xt
        xt1 = x1
      else
        dx = (d - d0) * slope
        xt = x0 + dx
        xt0 = x0
        xt1 = xt
      end if
      x0 = xt0
      x1 = xt1
    end if
    !
    ! -- return
    return
  end subroutine get_wetted_station

end module GwfSfrCrossSectionUtilsModule
