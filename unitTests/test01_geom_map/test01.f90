program test01
    use unitTests ! Module for unit testing.

    use geom_map

    implicit none

    ! - For testing pointers outside of map class. -
    abstract interface
        real(8) pure function funcInterface(t)
            real(8), intent(in) :: t
        end function funcInterface
    end interface
    procedure(funcInterface), pointer :: posPointer => null()

    ! - For testing map-derived objects. -
    type(rigid) :: a!, b, answer, result

    ! ---
    ! Test position and velocity functions.
    call assert(posFunction(0.0d0) ==  0.0d0, name="posFunction(0)")
    call assert(posFunction(1.0d0) == -1.0d0, name="posFunction(1)")
    call assert(velFunction(0.0d0) ==  0.0d0, name="velFunction(0)")
    call assert(velFunction(1.0d0) == -1.0d0, name="velFunction(1)")

    ! ---
    ! Test a function pointer.
    call assert(.not. associated(posPointer), name="posPointer==null")
    posPointer => posFunction
    call assert(associated(posPointer), name="posPointer/=null")
    call assert(posPointer(0.0d0) ==  0.0d0, name="posPointer(0)")
    call assert(posPointer(1.0d0) == -1.0d0, name="posPointer(1)")

    ! ---
    ! Test the init rigid method.
    a = init_rigid(axis=1, pos=posFunction, vel=velFunction)

    ! ---
    call finishUnitTest

contains

!>
!! Dummy position update function. Returns the negative value of time.
!<
real(8) pure function posFunction(t)
    real(8), intent(in) :: t
    posFunction = -1.0d0 * t
end function posFunction

!>
!! Dummy velocity update function. Returns the negative value of time.
!<
real(8) pure function velFunction(t)
    real(8), intent(in) :: t
    velFunction = -1.0d0 * t
end function velFunction

end program test01
