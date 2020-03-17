!***********************************************************************
!>
!! Module for carrying out unit tests.
!!
!! Version: 1.1
!! Author: Artur K. Lidtke (A.Lidtke@marin.nl)
!!
!! CHANGELOG
!! v 1.0 - original implementation
!!
!! TODO AL: could add some sort of test logging mechanism.
!<
!***********************************************************************

module unitTests
	implicit none

! - Module-level variables. -
private
	! Statistics of how well (or badly) the test program is doing.
	integer :: nAssertions=0, nPassedAssertions=0

! - Public function declarations. -
public assert, finishUnitTest

contains

	!>
	!! Make an assertion, print its staus, optionally return the value.
	!<
	subroutine assert(expression, name, value)
		implicit none
		logical, intent(in) :: expression
		character(len=*), optional, intent(in) :: name
		logical, optional, intent(out) :: value
		logical :: pass

		pass = .false.
		nAssertions = nAssertions + 1
		if (expression) then
			nPassedAssertions = nPassedAssertions + 1
			pass = .true.
		endif
		if (present(value)) value = pass

		if (present(name)) then
			print "(a,i0,3a,l)", "Assertion ", nAssertions, " '", trim(name), "', result: ", pass
		else
			print "(a,i0,a,l)", "Assertion ", nAssertions, ": ", pass
		endif
	end subroutine assert

	!>
	!! Print a summary and exit with an exit code that denotes the number of failures.
	!<
	subroutine finishUnitTest
		implicit none
		print "(a)", "------------"
		print "(a,i0)", "Assertions tested: ", nAssertions
		print "(a,i0)", "Assertions failed: ", nAssertions-nPassedAssertions
		call exit(nAssertions-nPassedAssertions)
	end subroutine finishUnitTest

end module unitTests
