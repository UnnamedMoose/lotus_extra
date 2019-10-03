program test00
    use unitTests ! Module for unit testing.

    use geom_global

    implicit none

    type(prop) :: a, b, answer
    logical :: pass

    a = prop(2, (/1,0,0/), 0., 0.)
    b = prop(-2, (/0,1,0/), 0., 0.1)

    ! ---
    ! Test the comparative function.
    call assert((compareProps(a, a) .and. (.not. compareProps(a, b))), name="Comparator")

    ! ---
    ! Should be b.
    answer = b
    call assert(compareProps(a.or.b, answer), name="Union", value=pass)
    if (.not. pass) then
        call printProp(a.or.b, "Result")
        call printProp(answer, "Answer")
        call printProp(a, "a")
        call printProp(b, "b")
        print *, ""
    endif

    ! ---
    ! Should be a, with b for dis2, normal2.
    answer = a
    answer%distance2 = b%distance
    answer%normal2 = b%normal
    call assert(compareProps(a.and.b, answer), name="Intersection", value=pass)
    if (.not. pass) then
        call printProp(a.and.b, "Result")
        call printProp(answer, "Answer")
        call printProp(a, "a")
        call printProp(b, "b")
        print *, ""
    endif

    ! ---
    ! Equal distance, but it picks b.
    answer = b
    call assert(compareProps(a-b, answer), name="Subtraction", value=pass)
    if (.not. pass) then
        call printProp(a-b, "Result")
        call printProp(answer, "Answer")
        call printProp(a, "a")
        call printProp(b, "b")
        print *, ""
    endif

    ! ---
    call finishUnitTest

contains

!>
!! Prints a given property object for inspection.
!<
subroutine printProp(prop0, name)
    implicit none
    type(prop), intent(in) :: prop0
    character(len=*), intent(in) :: name

    print "(a)", "---"
    print "(2a)", trim(name), ":"
    print "(a,3f8.4)", "d  ", prop0%distance
    print "(a,3f8.4)", "d2 ", prop0%distance2
    print "(a,3f8.4)", "n  ", prop0%normal
    print "(a,3f8.4)", "n2 ", prop0%normal2
    print "(a,3f8.4)", "k  ", prop0%kappa
    print "(a,3f8.4)", "v  ", prop0%velocity
end subroutine printProp

!>
!! Compares two property objects,
!<
logical function compareProps(prop0, prop1) result(areEqual)
    implicit none
    type(prop), intent(in) :: prop0, prop1
    integer :: i
    real(8), parameter :: tol=1e-12

    areEqual = .true.
    do i=1, 3
        if ( (abs(prop0%normal(i) - prop1%normal(i)) > tol) .or. &
             (abs(prop0%velocity(i) - prop1%velocity(i)) > tol) .or. &
             (abs(prop0%normal2(i) - prop1%normal2(i)) > tol) ) then
            areEqual = .false.
            return
        endif
    enddo
    do i=1, 2
        if (abs(prop0%kappa(i) - prop1%kappa(i)) > tol) then
            areEqual = .false.
            return
        endif
    enddo
    if ( (abs(prop0%distance - prop1%distance) > tol) .or. &
         (abs(prop0%distance2 - prop1%distance2) > tol) ) then
        areEqual = .false.
        return
    endif

end function compareProps

end program test00
