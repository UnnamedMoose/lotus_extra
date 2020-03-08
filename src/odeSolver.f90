!***********************************************************************
!>
!! Module for solving ordinary differential equations (ODEs).
!!
!! Version: 1.0
!! Author: Artur K. Lidtke (artur.lidtke@gmail.com)
!!
!! CHANGELOG
!! v 1.0 - original implementation
!!
!! TODO write this thing
!<
!***********************************************************************
module odeSolver

    implicit none

    ! ==========================
    ! ODE BASE CLASS
    ! ==========================
    type, abstract :: odeClass
    	! --- Properties. ---
    	! no. degrees of freedom.
    	integer :: nDof
        ! Current independent variable value.
    	real :: time
        ! State vector and its time derivatives.
    	real, dimension(:), allocatable :: y, dydt, d2ydt2

    	! --- Data storage. ---
    	real, dimension(:,:), allocatable :: timeHistory

    	! --- Member function declarations. ---
    	contains
            ! Return vectorised state vector (dydt, y), to be used directly in the time
            ! integration routines.
            ! procedure :: state => getState_

            ! Compute the derivatives of the system (e.g. force/mass for linear
            ! motion) and concatenate together with velocities in order to get
            ! a vector representation of the dynamics.
            ! NOTE AL: main function that needs to be re-defined in the derived classes.
            ! procedure :: ddtState => getDdtState_
            procedure(derivs_ode), deferred :: ddtState

    		! Callbacks to be executed at the end of each time step and at the end
    		! of time integration.
    		procedure(callbackStep_ode), deferred :: callbackStep
    		procedure(callbackEnd_ode), deferred :: callbackEnd

    end type odeClass

    ! Define an interface for the abstract method to allow inheritance and overloading
    ! in derived types.
    abstract interface
        !>
        !! Computes the time derivative of the state vector (acc, vel).
        !! Allows the state variables to be externally overriden, which allows
        !! intermediate time steps to be used in time integration (e.g. Runge-Kutta method).
        !!
        !! @param ddtStateVec - return value of size (nDof*2) = [d2ydt2, dydt]
        !! @param tExt - external value of the independent variable, use class-stored
        !!      value otherwise.
        !! @param yExt - external value of state, use internal values otherwise.
        !! @param dydtExt - external value of state vector time derivatives, use internal
        !!      values otherwise.
        !<
    	subroutine derivs_ode(self, ddtStateVec, tExt, yExt, dydtExt)
    		import odeClass
    		class(odeClass), intent(inout) :: self
    		real, dimension(self%nDof*2), intent(out) :: ddtStateVec
            real, intent(in), optional :: tExt
    		real, dimension(self%nDof), intent(in), optional :: yExt, dydtExt
    	end subroutine

    	! Can return an optional value that allows the integration to be stopped.
    	subroutine callbackStep_ode(self, t, y, dydt, d2ydt2, dt, stopNow)
    		import odeClass
    		class(odeClass), intent(inout) :: self
    		real, dimension(self%nDof*2), intent(inout), optional :: y, dydt, d2ydt2
    		real, intent(inout), optional :: t, dt
    		logical, intent(out), optional :: stopNow
    	end subroutine

    	subroutine callbackEnd_ode(self, t, y, dydt, d2ydt2, dt)
    		import odeClass
    		class(odeClass), intent(inout) :: self
    		real, dimension(self%nDof*2), intent(inout), optional :: y, dydt, d2ydt2
    		real, intent(inout), optional :: t, dt
    	end subroutine
    end interface

contains

!>
!! Routine for initialising the base-type fields inside a derived class constructor.
!<
subroutine odeBaseClassConstructor(newOde, nDof, t0, y0, dydt0, d2ydt20)
    implicit none
    class(odeClass), intent(inout) :: newOde
    integer, intent(in) :: nDof
    real, intent(in), optional :: t0
    real, dimension(nDof), intent(in), optional :: y0, dydt0, d2ydt20

    newOde%nDof = nDof
    allocate(newOde%y(newOde%nDof))
    allocate(newOde%dydt(newOde%nDof))
    allocate(newOde%d2ydt2(newOde%nDof))
    ! allocate data storage, keep zero entries to begin with
    allocate(newOde%timeHistory(0,newOde%nDof*3+1))

    ! Assume zero inital condition by default.
    if (present(t0)) then
        newOde%time = t0
    else
        newOde%time = 0.0
    endif
    if (present(y0)) then
        newOde%y = y0
    else
        newOde%y = 0.0
    endif
    if (present(dydt0)) then
        newOde%dydt = dydt0
    else
        newOde%dydt = 0.0
    endif
    if (present(d2ydt20)) then
        newOde%d2ydt2 = d2ydt20
    else
        newOde%d2ydt2 = 0.0
    endif
end subroutine odeBaseClassConstructor

!>
!! Concatenate the state of the system into vectorised form.
!! TODO make this one work to make the higher level code neater.
!<
! subroutine getState_(self, stateVec)
!     implicit none
!     class(odeClass), intent(in) :: self
!     real, dimension(:), allocatable, intent(out) :: stateVec
!     allocate(stateVec(self%nDof*2))
!     stateVec(1:self%nDof) = self%dydt
!     stateVec(self%nDof+1:self%nDof*2) = self%y
!     print *, "I'm a base type, yo!"
! end subroutine getState_

!>
!! Compute the accelerations and concatenate with velocities in order to get the
!! time derivative of the state of the system in vectorised form. Allows the
!! state variables to be externally overriden, which allows intermediate time
!! steps to be used in time integration (e.g. Runge-Kutta method).
!!
!! @param ddtStateVec - return value of size (nDof*2) = [d2ydt2, dydt]
!! @param tExt - external value of the independent variable, use class-stored
!!      value otherwise.
!! @param yExt - external value of state, use internal values otherwise.
!! @param dydtExt - external value of state vector time derivatives, use internal
!!      values otherwise.
!<
! subroutine getDdtState_(self, ddtStateVec, tExt, yExt, dydtExt)
!     implicit none
!     class(odeClass), intent(in) :: self
!     real, dimension(:), allocatable, intent(out) :: ddtStateVec
!     real, intent(in), optional :: tExt
!     real, dimension(self%nDof), intent(in), optional :: yExt, dydtExt
!
!     real :: t
!     real, dimension(self%nDof) :: y, dydt, d2ydt2
!
!     ! Parse state overrides.
!     if (present(tExt)) then
!         t = tExt
!     else
!         y = self%time
!     endif
!     if (present(yExt)) then
!         y = yExt
!     else
!         y = self%y
!     endif
!     if (present(dydtExt)) then
!         dydt = dydtExt
!     else
!         dydt = self%dydt
!     endif
!
!     ! Compute time derivatives.
!     ! NOTE: overwrite here in derived classes in order to implement specific functionality.
!     d2ydt2 = 0.0d0
!     ! NOTE: overwrite here in derived classes in order to implement specific functionality.
!
!     ! Allocate storage for the time derivative vector and concatenate.
!     allocate(ddtStateVec(self%nDof*2))
!     ddtStateVec(1:self%nDof) = d2ydt2
!     ddtStateVec(self%nDof+1:self%nDof*2) = dydt
!
! end subroutine getDdtState_

end module odeSolver
