module test05module
    use odeSolver

    implicit none

    ! ============
    ! Example class showing the intended use of the module by implementing dynamics
    ! of a linear 2D mass-spring-damper subject to sinusoidal excitation.
    ! ============
    type, extends(odeClass) :: msdClass
        ! --- properties ---
        real, dimension(:,:), allocatable :: m, k, c
        real, dimension(:), allocatable :: F, omega

        ! --- member function declarations ---
        contains
            ! Calculation of the derivative vector
            procedure :: derivs => msdDerivs_

            ! callbacks
            procedure :: callbackStep => msdCallbackStep_
            procedure :: callbackEnd => msdCallbackEnd_

    end type msdClass

    ! create an interface to make sure the constructor gets called automatically
    interface msdClass
        module procedure msdConstructor_
    end interface

contains

!>
!! Initialises the object.
!<
!=====================================================================
function msdConstructor_(noDegreesOfFreedom) result(newMsdOde)
!=====================================================================
    integer, intent(in) :: noDegreesOfFreedom
    type(msdClass) :: newMsdOde
    real, parameter :: pi=3.14159265359

    ! Bare minimum of initalisation needed by the base class.
    ! Start at a fixed displacement with zero speed.
    call odeBaseClassConstructor(newMsdOde, noDegreesOfFreedom, y0=(/0.2, 0.2/))

    ! allocate mass, damping and stiffness arrays
    allocate(newMsdOde%m(newMsdOde%nDof, newMsdOde%nDof))
    allocate(newMsdOde%k(newMsdOde%nDof, newMsdOde%nDof))
    allocate(newMsdOde%c(newMsdOde%nDof, newMsdOde%nDof))

    ! allocate forcing arrays
    allocate(newMsdOde%F(newMsdOde%nDof))
    allocate(newMsdOde%omega(newMsdOde%nDof))

    ! Assign values - hard-coded for the purpose of the test.
    newMsdOde%m(1,:) = (/0.2, 0.0/)
    newMsdOde%m(2,:) = (/0.0, 0.2/)

    newMsdOde%k(1,:) = (/5.0, 0.0/)
    newMsdOde%k(2,:) = (/0.0, 9.0/)

    newMsdOde%c(1,:) = (/0.2, 0.0/)
    newMsdOde%c(2,:) = (/0.0, 0.3/)

    newMsdOde%F = (/1.0, 1.0/)
    newMsdOde%omega = (/2.*pi*1., 2.*pi*2./)

end function msdConstructor_

!>
!! Computes accelerations of the state vector of a 2D mass-spring-damper system
!<
!=====================================================================
subroutine msdDerivs_(self, d2ydtNew, tExt, yExt, dydtExt)
!=====================================================================
    implicit none

    class(msdClass), intent(inout) :: self
    real, dimension(self%nDof), intent(out) :: d2ydtNew
    real, intent(in), optional :: tExt
    real, dimension(self%nDof), intent(in), optional :: yExt, dydtExt

    real :: t
    real, dimension(self%nDof) :: y, dydt

    real, dimension(:,:), allocatable :: mInv
    real, dimension(:), allocatable :: effK, effC, effF
    integer :: i

    ! Parse state overrides.
    if (present(tExt)) then
        t = tExt
    else
        t = self%time
    endif
    if (present(yExt)) then
        y = yExt
    else
        y = self%y
    endif
    if (present(dydtExt)) then
        dydt = dydtExt
    else
        dydt = self%dydt
    endif

    ! Allocate temporary arrays
    allocate(mInv(self%nDof, self%nDof))
    allocate(effK(self%nDof))
    allocate(effC(self%nDof))
    allocate(effF(self%nDof))

    ! Invert the mass matrix (assume it's diagonal).
    mInv = 0.0
    do i=1, self%nDof
        mInv(i,i) = 1.0 / self%m(i,i)
    enddo

    ! Calcualte the unsteady forcing term.
    do i=1, self%nDof
        effF(i) = self%F(i)*sin(self%omega(i)*t)
    enddo

    ! Compute effective stiffness and damping, as well as the forcing term on the
    ! RHS, leaving LHS with the vector of unknowns only.
    effF = matmul(mInv, effF)
    effK = matmul(matmul(self%k, mInv), y)
    effC = matmul(matmul(self%c, mInv), dydt)
    d2ydtNew = effF - effK - effC

    ! Clean up.
    deallocate(mInv)
    deallocate(effK)
    deallocate(effC)
    deallocate(effF)

end subroutine msdDerivs_

subroutine msdCallbackStep_(self, t, y, dydt, d2ydt2, dt, stopNow)
    class(msdClass), intent(inout) :: self
    real, dimension(self%nDof*2), intent(inout), optional :: y, dydt, d2ydt2
    real, intent(inout), optional :: t, dt
    logical, intent(out), optional :: stopNow
    if (.false.) print *, y
    if (.false.) print *, t
    if (.false.) print *, dydt
    if (.false.) print *, d2ydt2
    if (.false.) print *, dt
    if (.false.) print *, stopNow
end subroutine msdCallbackStep_

subroutine msdCallbackEnd_(self, t, y, dydt, d2ydt2, dt)
    class(msdClass), intent(inout) :: self
    real, dimension(self%nDof*2), intent(inout), optional :: y, dydt, d2ydt2
    real, intent(inout), optional :: t, dt
    if (.false.) print *, y
    if (.false.) print *, t
    if (.false.) print *, dydt
    if (.false.) print *, d2ydt2
    if (.false.) print *, dt
end subroutine msdCallbackEnd_

end module test05module

! ===

program test05
    use unitTests ! Module for unit testing.

    use odeSolver
    use test05module

    implicit none

    integer, parameter :: nDof=2
    type(msdClass) :: msdSystem
    real, dimension(nDof*2) :: vecResult, vecAnswer
    real :: rmsError
    logical :: pass

    ! ---
    ! Create a mass-spring-damper system object.
    msdSystem = msdClass(nDof)

    ! - Test 0 -
    ! Retrieve vectorised initial state.
    vecAnswer = (/0.2, 0.2, 0.0, 0.0/)
    vecResult = getState(msdSystem)
    rmsError = sqrt(sum((vecAnswer - vecResult)**2.0))
    call assert(rmsError < 1e-6, name="initial state", value=pass)
    if (.not. pass) then
        print "(a,10f8.3)", "Got error", rmsError
        print "(a,10f8.3)", "Got values", vecResult
    endif

    ! - Test 1 -
    ! Compute the initial state - acceleration should be equal to k*x0/m.
    vecAnswer = (/0.0, 0.0, -5.0, -9.0/)
    vecResult = 0.0
    call msdSystem%derivs(vecResult(3:4))
    rmsError = sqrt(sum((vecAnswer - vecResult)**2.0))
    call assert(rmsError < 1e-6, name="initial accelerations", value=pass)
    if (.not. pass) then
        print "(a,10f8.3)", "Got error", rmsError
        print "(a,10f8.3)", "Got values", vecResult
    endif

    ! ---
    ! ! Test the interpolation with mirrored points for periodic behaviour.
    ! interpResult = 0.1
    ! ! rmsError = sqrt(sum((0.1 - interpResult)**2.0))
    ! rmsError = sqrt((0.1 - interpResult)**2.0)
    ! call assert(rmsError < 1e-6, name="y(x) with mirrored end data", value=pass)
    ! if (.not. pass) then
    !     print *, "Got error", rmsError
    !     print *, "Got values", interpResult
    ! endif

    ! ---
    call finishUnitTest

end program test05
