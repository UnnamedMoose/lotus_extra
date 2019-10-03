!***********************************************************************
!>
!! Module for reading control files.
!!
!! Version: 1.0
!! Author: Artur K. Lidtke (artur.lidtke@gmail.com)
!!
!! CHANGELOG
!! v 1.0 - original implementation
!!
!! TODO replace fixed max no. lines with a dynamic list.
!! TODO the value-retrieving functions are topologically identical for all data types.
!!      Would be much neater to template them somehow to avoid code duplication, but
!!      don't know how to do that because Fortran :/
!<
!***********************************************************************
module controlFileInterface
    use, intrinsic :: ieee_arithmetic, only: IEEE_Value, IEEE_QUIET_NAN

    implicit none

! - Module-level variables. -
private
    integer, parameter :: maxLineLength=200, maxnLines=500

!> ==============================
!! CONTROL FILE CLASS DECLARATION
!! ==============================
!!
!! Keeps a collection of text lines read from a file and provides methods for
!! retrieving key-value pairs.
!!
!< ==============================
type, public :: controlFileClass
    integer :: nLines
    character(len=maxLineLength), dimension(maxnLines) :: contents

    contains
        procedure :: getFloat => getFloat_
        procedure :: getInt => getInt_
        procedure :: getVector => getVector_

end type controlFileClass

! Create an interface to make sure the constructor gets called automatically.
interface controlFileClass
    module procedure controlFileConstructor_
end interface

contains

!>
!! Create the control file object by reading the file's contents and storing them.
!!
!! @param filename - name of the input file to be read.
!<
function controlFileConstructor_(filename) result(newFile)
    implicit none
    type(controlFileClass) :: newFile
    character(len=*), intent(in) :: filename

    integer :: fileUnit, iLine, ios
    character(len=maxLineLength) :: buffer

    ios = 0; iLine = 0

    open(newunit=fileUnit, file=filename)

    ! ios is negative if an end of record condition is encountered or if
    ! an endfile condition was detected. It is positive if an error was
    ! detected. ios is zero otherwise.
    do while (ios == 0)
        read(fileUnit, '(A)', iostat=ios) buffer
        if (ios == 0) then
            iLine = iLine + 1
            newFile%contents(iLine) = buffer
        end if
    end do

    close(fileUnit)

    newFile%nLines = iLine

end function controlFileConstructor_

!>
!! Scan through the control file contents and retrieve the required value.
!!
!! @param keyword - string identifier of the desired value.
!! @param suppressKeyNotFoundWarnig - do not show a warning, even if the required key
!!      has not been found.
!<
real(8) function getFloat_(self, keyword, suppressKeyNotFoundWarnig) result(retValue)
    implicit none
    class(controlFileClass), intent(in) :: self
    character(len=*), intent(in) :: keyword
    logical, intent(in), optional :: suppressKeyNotFoundWarnig

    integer :: pos, iLine, ios
    logical :: keywordFound, suppressWarning
    character(len=maxLineLength) :: buffer, label

    keywordFound = .false.
    do iLine=1, self%nLines
        ! Find the first instance of the separator. Split label and data.
        pos = scan(self%contents(iLine), ':')
        label = self%contents(iLine)(1:pos-1)
        buffer = self%contents(iLine)(pos+1:)

        ! See if this is the keyword that has been requested.
        if (label == trim(keyword)) then
            read(buffer, *, iostat=ios) retValue
            keywordFound = .true.
            exit
        endif
    end do

    ! Return NaN if the value has not been found.
    suppressWarning = .false.
    if (present(suppressKeyNotFoundWarnig)) suppressWarning = suppressKeyNotFoundWarnig
    if ((.not. keywordFound) .and. (.not. suppressWarning)) then
        print "(3a)", 'WARNING in controlFileInterface::getFloat: no value found for requested keyword "', trim(keyword), '"'
    endif
    if (.not. keywordFound) then
        retValue = IEEE_VALUE(retValue, IEEE_QUIET_NAN)
    endif

end function getFloat_

!>
!! Scan through the control file contents and retrieve the required value.
!!
!! @param keyword - string identifier of the desired value.
!! @param suppressKeyNotFoundWarnig - do not show a warning, even if the required key
!!      has not been found.
!<
integer function getInt_(self, keyword, suppressKeyNotFoundWarnig) result(retValue)
    implicit none
    class(controlFileClass), intent(in) :: self
    character(len=*), intent(in) :: keyword
    logical, intent(in), optional :: suppressKeyNotFoundWarnig

    integer :: pos, iLine, ios
    logical :: keywordFound, suppressWarning
    character(len=maxLineLength) :: buffer, label

    keywordFound = .false.
    do iLine=1, self%nLines
        pos = scan(self%contents(iLine), ':')
        label = self%contents(iLine)(1:pos-1)
        buffer = self%contents(iLine)(pos+1:)

        ! See if this is the keyword that has been requested.
        if (label == trim(keyword)) then
            read(buffer, *, iostat=ios) retValue
            keywordFound = .true.
            exit
        endif
    end do

    ! Return zero if value has not been found.
    suppressWarning = .false.
    if (present(suppressKeyNotFoundWarnig)) suppressWarning = suppressKeyNotFoundWarnig
    if ((.not. keywordFound) .and. (.not. suppressWarning)) then
        print "(3a)", 'WARNING in controlFileInterface::getInt: no value found for requested keyword "', trim(keyword), '"'
    endif
    if (.not. keywordFound) then
        retValue = -1
    endif

end function getInt_

!>
!! Scan through the control file contents and retrieve the required value.
!!
!! @param keyword - string identifier of the desired value.
!! @param suppressKeyNotFoundWarnig - do not show a warning, even if the required key
!!      has not been found.
!<
function getVector_(self, keyword, suppressKeyNotFoundWarnig) result(retValue)
    implicit none
    real(8), dimension(3) :: retValue
    class(controlFileClass), intent(in) :: self
    character(len=*), intent(in) :: keyword
    logical, intent(in), optional :: suppressKeyNotFoundWarnig

    integer :: pos, iLine, ios
    logical :: keywordFound, suppressWarning
    character(len=maxLineLength) :: buffer, label

    keywordFound = .false.
    do iLine=1, self%nLines
        pos = scan(self%contents(iLine), ':')
        label = self%contents(iLine)(1:pos-1)
        buffer = self%contents(iLine)(pos+1:)

        if (label == trim(keyword)) then
            read(buffer, *, iostat=ios) retValue
            keywordFound = .true.
            exit
        endif
    end do

    ! Fill the vector with NaN's if the value is missing.
    suppressWarning = .false.
    if (present(suppressKeyNotFoundWarnig)) suppressWarning = suppressKeyNotFoundWarnig
    if ((.not. keywordFound) .and. (.not. suppressWarning)) then
        print "(3a)", 'WARNING in controlFileInterface::getVector: no value found for requested keyword "', trim(keyword), '"'
    endif
    if (.not. keywordFound) then
        do pos=1, 3
            retValue(pos) = IEEE_VALUE(retValue(pos), IEEE_QUIET_NAN)
        enddo
    endif

end function getVector_

end module controlFileInterface
