!***********************************************************************
!>
!! Module for smooth interpolation of points using splines.
!!
!! Version: 1.0
!! Author: Artur K. Lidtke (artur.lidtke@gmail.com)
!!
!! CHANGELOG
!! v 1.0 - original implementation
!!
!<
!***********************************************************************
module splineInterpolation
	implicit none

! - Module-level variables. -
private

!> ==============================
!! SPLINE INTERPOLATOR CLASS DECLARATION
!! ==============================
!!
!! Holds data of y(x) and uses splines to provide smooth interpolation of the data.
!!
!< ==============================
type, public :: splineInterpolatorClass
    real(8), dimension(:), allocatable :: x, y, dydx2

    contains
        procedure :: getValue => getValue_

end type splineInterpolatorClass

! Create an interface to make sure the constructor gets called automatically.
interface splineInterpolatorClass
    module procedure splineInterpolatorConstructor_
end interface

! - Private function declarations. -
private prepareSpline, interpSpline

contains

!>
!! Construct an interpolating cubic spline by providing it with the base data. Finite
!! differences will be used to compute the 2nd derivative of y w.r.t. x, which is
!! needed by the interpolating algorithm.
!! Alogorithm adapted from "Numerical recipes in C++, 2nd Ed", Chap. 3 by Press et al.
!! May also request portion of the data to be mirrored, which helps to preserve
!! smoothness of periodic data.
!<
function splineInterpolatorConstructor_(x, y, nPtsToMirror) result(newSpline)
    implicit none
    real(8), dimension(:), intent(in) :: x, y
    integer, intent(in), optional :: nPtsToMirror
    type(splineInterpolatorClass) :: newSpline

    integer :: nDataPts

    if (present(nPtsToMirror)) then
        nDataPts = size(x) + nPtsToMirror*2
    else
        nDataPts = size(x)
    endif

    allocate(newSpline%x(nDataPts))
    allocate(newSpline%y(nDataPts))
    allocate(newSpline%dydx2(nDataPts))

    if (present(nPtsToMirror)) then
        newSpline%x(1:nPtsToMirror) = x(size(x)-nPtsToMirror:size(x)-1) - x(size(x))
        newSpline%y(1:nPtsToMirror) = y(size(x)-nPtsToMirror:size(x)-1)

        newSpline%x(1+nPtsToMirror:nDataPts-nPtsToMirror+1) = x
        newSpline%y(1+nPtsToMirror:nDataPts-nPtsToMirror+1) = y

        newSpline%x(nDataPts-nPtsToMirror+1:nDataPts) = x(2:nPtsToMirror+1) + x(size(x))
        newSpline%y(nDataPts-nPtsToMirror+1:nDataPts) = y(2:nPtsToMirror+1)
    else
        newSpline%x = x
        newSpline%y = y
    endif

    call prepareSpline(newSpline%dydx2, newSpline%x, newSpline%y)

end function splineInterpolatorConstructor_

!>
!! Use the spline to return a value at the required location.
!<
real(8) function getValue_(self, xi) result(yi)
    implicit none
    class(splineInterpolatorClass), intent(in) :: self
    real(8), intent(in) :: xi

    call interpSpline(yi, xi, self%x, self%y, self%dydx2)

end function getValue_

!>
!! Computes the 2nd derivative of y vs x to be used for cubic spline interpolation.
!! Alogorithm adapted from "Numerical recipes in C++, 2nd Ed", Chap. 3 by Press et al.
!<
pure subroutine prepareSpline(dydx2, x, y)
    real*8, dimension(:), intent(out) :: dydx2
    real*8, dimension(:), intent(in) :: x, y

    real*8, dimension(:), allocatable :: u
    real*8 :: dydx0, dydxN
    integer :: n
    real*8 :: sig, p, qn, un
    integer :: i

    ! allocate variable size arrays
    n = size(x)
    allocate(u(n-1))

    ! Calculate the gradient that will be prescribed to the data at the boundaries.
    ! Assume y is periodic across x, so that the gradient at x(1) and x(N) should be
    ! identical.
    dydx0 = (y(2)-y(1))/(x(2)-x(1))
    dydxN = (y(n)-y(n-1))/(x(n)-x(n-1))

    ! dydx0 = (dydx0+dydxN)*0.5
    ! dydxN = dydx0

    ! BC at the beginning
    ! for a natural spline (0 second derivative)
    dydx2(1) = 0.
    u(1) = 0.
    ! fixed slope
    ! dydx2(1) = -0.5
    ! u(1) = (3./(x(2)-x(1))) * ((y(2)-y(1))/(x(2)-x(1))-dydx0)

    ! internal points
    do i = 2, n-1
        ! decomposition for the tridiagonal algorithm
        sig = (x(i)-x(i-1))/(x(i+1)-x(i-1))
        p = sig*dydx2(i-1)+2.
        dydx2(i) = (sig-1.)/p
        u(i) = (y(i+1)-y(i))/(x(i+1)-x(i)) - (y(i)-y(i-1))/(x(i)-x(i-1))
        u(i) = (6.*u(i)/(x(i+1)-x(i-1)) - sig*u(i-1))/p
    end do

    ! BC at the end
    ! natural spline
    qn = 0.
    un = 0.
    ! fixed gradient
    ! qn = 0.5
    ! un = (3./(x(n)-x(n-1))) * (dydxN - (y(n)-y(n-1))/(x(n)-x(n-1)))

    ! backsubstitution for the tridiagonal algorithm
    ! TODO is it u(n-2) or u(n-1) ??? - seems to work so it's probably fine
    dydx2(n) = (un-qn*u(n-2)) /  (qn*dydx2(n-1)+1.)
    do i = n-1, 1, -1
        dydx2(i) = dydx2(i)*dydx2(i+1)+u(i)
    end do

    deallocate(u)
end subroutine prepareSpline

!>
!! Performs cubic spline interpolation of data given by x and y. Needs to be
!! supplied with the 2nd derivative of y vs x, which may be computed using prepareSpline().
!! The subroutine returns the interpolated value yi defined at xi.
!! Alogorithm adapted from "Numerical recipes in C++, 2nd Ed", Chapt. 3 by Press et al.
!<
pure subroutine interpSpline(yi, xi, x, y, dydx2)
    real*8, intent(out) :: yi
    real*8, intent(in) :: xi
    real*8, dimension(:), intent(in) :: x, y, dydx2

    integer :: i
    real*8 :: a, b, h

    ! find indices of the interpolation points
    ! FIXME bi-section would be a more efficient approach
    do i = 1, size(x)-1
        if ((x(i)<=xi) .and. (xi<x(i+1))) exit
    end do

    h = x(i+1)-x(i)
    a = (x(i+1)-xi)/h
    b = (xi-x(i))/h
    yi = a*y(i) + b*y(i+1) + ((a*a*a-a)*dydx2(i) + (b*b*b-b)*dydx2(i+1))*(h*h)/6.
end subroutine interpSpline

end module splineInterpolation
