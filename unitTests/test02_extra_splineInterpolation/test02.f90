program test02
    use unitTests ! Module for unit testing.

    use splineInterpolation

    implicit none

    integer :: i, n, ier
    real(8), parameter :: pi=3.141592653589793
    real(8), dimension(:,:), allocatable :: xyData
    real(8), dimension(:), allocatable :: interpCoords, interpResult, &
                                            answer_dydx2, answer_interpolation
    real(8) :: rmsError
    logical :: pass
    type(splineInterpolatorClass) :: spline, splineMirrored

    ! - Allocate containers. -
    n = 201
    allocate(xyData(n,2), stat=ier)
    ! allocate(dydx2Data(n), stat=ier)
    allocate(answer_dydx2(n), stat=ier)
    allocate(interpCoords(n-1), stat=ier)
    allocate(interpResult(n-1), stat=ier)
    allocate(answer_interpolation(n-1), stat=ier)

    ! - Prepare test data - sin(x) in range <0,2pi>. -
    do i=1, n
        xyData(i,1) = 2.0*pi * (real(i)-1.0)/(real(n)-1.0)
        xyData(i,2) = sin(xyData(i,1))
        answer_dydx2(i) = -1.0 * sin(xyData(i,1))
    enddo
    do i=1, n-1
        interpCoords(i) = (xyData(i,1) + xyData(i+1,1)) * 0.5
        answer_interpolation(i) = sin(interpCoords(i))
    enddo

    ! - Create the spline objects. -
    spline = splineInterpolatorClass(xyData(:,1), xyData(:,2))
    splineMirrored = splineInterpolatorClass(xyData(:,1), xyData(:,2), nPtsToMirror=4)

    ! ---
    ! Test the 2nd derivative computation.
    rmsError = sqrt(sum((spline%dydx2 - answer_dydx2)**2.0))
    call assert(rmsError < 1e-3, name="d2y/dx2")

    ! ---
    ! Test the interpolation.
    do i=1, n-1
        interpResult(i) = spline%getValue(interpCoords(i))
    enddo
    rmsError = sqrt(sum((interpResult - answer_interpolation)**2.0))
    call assert(rmsError < 1e-6, name="y(x)")

    ! ---
    ! Test the interpolation with mirrored points for periodic behaviour.
    do i=1, n-1
        interpResult(i) = splineMirrored%getValue(interpCoords(i))
    enddo
    rmsError = sqrt(sum((interpResult - answer_interpolation)**2.0))
    call assert(rmsError < 1e-6, name="y(x) with mirrored end data", value=pass)
    if (.not. pass) then
        print *, "Got error", rmsError
        print *, "Got values", interpResult
    endif

    ! ---
    call finishUnitTest

    ! - Clean up. -
    deallocate(xyData, stat=ier)
    deallocate(interpCoords, stat=ier)
    deallocate(interpResult, stat=ier)
    deallocate(answer_dydx2, stat=ier)
    deallocate(answer_interpolation, stat=ier)

end program test02
