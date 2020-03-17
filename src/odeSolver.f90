!*******************************************************************************
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
!*******************************************************************************
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

    	! --- Member function declarations. ---
    	contains
            ! Compute the derivatives of the system (e.g. force/mass for linear
            ! motion) and concatenate together with velocities in order to get
            ! a vector representation of the dynamics.
            ! NOTE AL: main function that needs to be re-defined in the derived classes.
            procedure(derivs_ode), deferred :: derivs

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
    	subroutine derivs_ode(self, d2ydtNew, tExt, yExt, dydtExt)
    		import odeClass
    		class(odeClass), intent(inout) :: self
    		real, dimension(self%nDof), intent(out) :: d2ydtNew
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
!! Concatenate the state of the system into vectorised form (y, dydt).
!<
function getState(ode) result(stateVec)
    implicit none
    class(odeClass), intent(in) :: ode
    real, dimension(ode%nDof*2) :: stateVec
    stateVec(1:ode%nDof) = ode%y
    stateVec(ode%nDof+1:ode%nDof*2) = ode%dydt
end function getState

!>
!! Concatenate the d/dt of the state of the system into vectorised form (dydt, d2ydt2).
!<
function getDdtState(ode) result(ddtStateVec)
    implicit none
    class(odeClass), intent(in) :: ode
    real, dimension(ode%nDof*2) :: ddtStateVec
    ddtStateVec(1:ode%nDof) = ode%dydt
    ddtStateVec(ode%nDof+1:ode%nDof*2) = ode%d2ydt2
end function getDdtState

!>
!! Given the new state (pos, vel), update the fields inside the ode object.
!<
subroutine updateState(ode, newStateVec)
    implicit none
    class(odeClass), intent(inout) :: ode
    real, dimension(ode%nDof*2), intent(in) :: newStateVec
    ode%y = newStateVec(1:ode%nDof)
    ode%dydt = newStateVec(ode%nDof+1:ode%nDof*2)
end subroutine updateState

! ===
! Time integration routines.
! ===

!>
!! Single step of basic 1st order Euler time integration.
!<
subroutine odeIntStepEuler(ode, dt)
    implicit none
    class(odeClass), intent(inout) :: ode
    real, intent(in) :: dt

	real, dimension(ode%nDof*2) :: stateNew

    ! Make an Euler step based on the current accelerations.
    ode%time = ode%time + dt
    stateNew = getState(ode) + getDdtState(ode)*dt

    ! Update the state. New derivatives have to be computed externally to this
    ! routine. This allows flow forces at the new time level to included.
    call updateState(ode, stateNew)

end subroutine odeIntStepEuler

end module odeSolver
